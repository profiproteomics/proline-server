package fr.proline.module.parser.mascot

import java.io.File

import scala.Array.canBuildFrom
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.break
import scala.util.control.Breaks.breakable

import org.apache.commons.lang3.StringUtils

import com.typesafe.scalalogging.slf4j.Logging

import fr.proline.core.om.model.msi.FragmentMatch
import fr.proline.core.om.model.msi.FragmentMatchType
import fr.proline.core.om.model.msi.Fragmentation
import fr.proline.core.om.model.msi.SpectrumMatch
import fr.proline.core.om.model.msi.TheoreticalFragmentSeries
import fr.profi.util.ms.MassTolUnit
import fr.profi.util.ms.calcMozTolInDalton
import matrix_science.msparser.ms_aahelper
import matrix_science.msparser.ms_errs
import matrix_science.msparser.ms_fragment
import matrix_science.msparser.ms_fragmentationrules
import matrix_science.msparser.ms_fragmentvector
import matrix_science.msparser.ms_mascotresfile
import matrix_science.msparser.ms_masses
import matrix_science.msparser.ms_modification
import matrix_science.msparser.ms_modvector
import matrix_science.msparser.ms_peptide
import matrix_science.msparser.ms_quant_method
import matrix_science.msparser.ms_umod_configfile
import matrix_science.msparser.msparserConstants
import matrix_science.msparser.vectori

trait LoggingFake {
  object logger {
    def info(s: String) {}
    def debug(s: String) {}
    def error(s: String) {}
  }
}

case class Peak(moz: Double, intensity: Float)

class MascotSpectrumMatcher(mascotResFile: ms_mascotresfile, mascotConfig: IMascotConfig, ptmHelper: MascotPTMHelper) extends Logging {

  val FREE_MEMORY = true // TODO: remove me when the memory leaks have been solved
  val mascotSearchParams = mascotResFile.params
  val mascotVersion = mascotResFile.getMascotVer
  val mascotFragRules = mascotConfig.fragmentationRulesFile.getInstrumentByName(mascotSearchParams.getINSTRUMENT())
  val aaHelpersByQuantComp = mutable.Map.empty[String, ms_aahelper]

  /* Mutable collection @GuardedBy("itself") */
  val m_createdAAHelpers = ArrayBuffer.empty[ms_aahelper]

  final val DEFAULT_QUANT_COMP_KEY = "DEFAULT"

  require(
    mascotFragRules != null, "can't find fragmentation rules for instrument named " + mascotSearchParams.getINSTRUMENT() +
      " please check that your Mascot 'fragmentation_rules' config file contains this instrument " +
      "and that you provided an URL targeting your own Mascot server"
  )
  // Map Mascot fragmentation series names by internal ones (handle doubly charged state)
  val fragSeriesByMascotFragSeries = Map() ++
    Fragmentation.defaultIonTypeByMascotSeriesName.map(p => p._1 -> p._2.toString) ++
    Fragmentation.defaultIonTypeByMascotSeriesName.map(p => (p._1 + "++") -> (p._2.toString + "++"))

  val quantitationMethod: ms_quant_method = {

    val quantMethodName = mascotSearchParams.getQUANTITATION
    val quantCfgFile = mascotConfig.quantitationFile

    if (quantMethodName == null || quantCfgFile == null) null
    else quantCfgFile.getMethodByName(quantMethodName)
  }

  def getSpectrumMatch(mascotPep: ms_peptide): SpectrumMatch = { //, spectrum: Spectrum

    var start = System.currentTimeMillis()

    val pepSequence = mascotPep.getPeptideStr
    val mascotQueryId = mascotPep.getQuery

    val usedPeaksCount = mascotPep.getPeaksUsedFromIons1() // was numUsed    

    val (ms2ErrorTol, ms2ErrorTolUnitStr) = (mascotSearchParams.getITOL, mascotSearchParams.getITOLU)
    val ms2ErrorTolUnit = MassTolUnit.withName(ms2ErrorTolUnitStr)

    // To find the matching peaks we need to have them in the correct order
    // Mascot store peaks in a custom order so we have to parse again the data
    val ionsStr = mascotResFile.getQuerySectionValueStr(mascotQueryId, "Ions1")
    val allPeaks = parseMascotQueryIonsStr(ionsStr)
    var usedPeaks = new ArrayBuffer[Peak](usedPeaksCount + 2)

    // Fill usedPeaks with firstSortedPeaks data  
    usedPeaks += Peak(0.0, 0f)
    usedPeaks ++= allPeaks.take(usedPeaksCount).sortBy(_.moz)
    usedPeaks += Peak(Double.MaxValue, 0f)

    // FIXME: is this really needed ?
    //intensities.put(m.get(0), intensities.get(m.get(1)))
    //intensities.put(m.get(m.size() - 1), intensities.get(m.get(m.size() - 2)))

    //    // Retrieve full spectrum peaks
    //    val allPeaksSorted = allPeaks.sortBy(_.moz)
    //
    //    // Define some vars
    //    var prevPeak: Peak = null
    //    var j = 0
    //
    //    // Iterate over matching peaks
    //    for (i <- 0 until usedPeaks.length) {
    //      val peak = usedPeaks(i)
    //      var intThresh = peak.intensity
    //
    //      // TODO: explain this computation
    //      if ((i > 0) && (i < (usedPeaks.length - 1)) && (prevPeak.intensity < peak.intensity)) {
    //        intThresh = prevPeak.intensity
    //      }
    //
    //      while ((j < allPeaksSorted.length) && (allPeaksSorted(j).moz < peak.moz)) {
    //        val newPeak = allPeaksSorted(j)
    //        if (newPeak.intensity >= intThresh) {
    //          usedPeaks += newPeak
    //        }
    //        j += 1
    //      }
    //      j += 1
    //
    //      prevPeak = peak
    //    }
    //
    //  
    //    // Sort peaks again because some peaks have been added since the first sorting
    //    usedPeaks = usedPeaks.sortBy(_.moz)

    val fragMatches = new ArrayBuffer[FragmentMatch]()

    var fragmentationTable: Array[TheoreticalFragmentSeries] = null

    var theoFragments: ms_fragmentvector = null

    try {
      theoFragments = _getTheoreticalFragments(mascotPep)

      for (k <- 0 until theoFragments.getNumberOfFragments) {

        val fragment = theoFragments.getFragmentByNumber(k)
        val theoFragMoz = fragment.getMass
        val mozTolInDa = calcMozTolInDalton(theoFragMoz, ms2ErrorTol, ms2ErrorTolUnit)
        var bestMatch: FragmentMatch = null

        breakable {
          for (ms2Peak <- usedPeaks) {

            val obsMoz = ms2Peak.moz
            val deltaMoz = obsMoz - theoFragMoz

            if (scala.math.abs(deltaMoz) <= mozTolInDa) {

              val fragType = if (fragment.isRegular) None
              else if (fragment.isInternal) Some(FragmentMatchType.INTERNAL.toString)
              else Some(FragmentMatchType.IMMONIUM.toString)

              val fragMatch = new FragmentMatch(
                label = fragment.getLabel,
                //ionSeries = fragment.getSeriesName,
                // TODO: check is this is the expected value
                //aaPosition = if( fragment.isInternal) fragment.getStart else fragment.getColumn,
                `type` = fragType,
                //charge = fragment.getCharge,
                moz = obsMoz,
                calculatedMoz = theoFragMoz,
                intensity = ms2Peak.intensity
              )

              val nl = fragment.getNeutralLoss
              if (nl > 0.0) fragMatch.neutralLossMass = Some(nl)

              if ((bestMatch == null) || (bestMatch.intensity < fragMatch.intensity)) {
                bestMatch = fragMatch
              }

            } else if (scala.math.signum(deltaMoz) > 0) break
          }
        }

        if (bestMatch != null) fragMatches += bestMatch
      }

      fragmentationTable = _buildFragmentationTable(pepSequence, theoFragments)
    } finally {

      if (FREE_MEMORY) {
        /* Free memory in finally block */

        if (theoFragments != null) {

          /* Delete elements then collection */
          for (i <- 0 until theoFragments.getNumberOfFragments) {
            try {
              theoFragments.getFragmentByNumber(i).delete()
            } catch {
              case t: Throwable => logger.error("Error deleting theoFragment #" + i, t)
            }
          }

          try {
            theoFragments.clearFragments()
            theoFragments.delete()
          } catch {
            case t: Throwable => logger.error("Error deleting theoFragments Vector", t)
          }

        }

      }

    } // End of try - finally block

    new SpectrumMatch(mascotQueryId, mascotPep.getRank, fragmentationTable, fragMatches.toArray)
    /*new PeptideMatchDetail(
          pep.getSequenceStr(),
          spectrum,
          massList,
          matches,
          new PeptideFragmentation(
                buildSeries(pep, allFragments),
                allFragments.getNumberOfFragments()
              )
         )*/
  }

  def clear() {
    doClear(false)
  }

  override def finalize() {

    try {

      try {
        doClear(true)
      } catch {
        case ex: Exception => logger.error("Error clearing MascotSpectrumMatcher", ex)
      }

    } finally {
      super.finalize()
    }

  }

  protected def parseMascotQueryIonsStr(mascotQueryPeaksStr: String): Array[Peak] = {
    val peaks = mascotQueryPeaksStr.split(",")
    //val firstPeaks = if( maxNbPeaks > 0 ) peaks.take(maxNbPeaks) else peaks

    val result = peaks.map { peakAsStr =>
      val values = peakAsStr.split(":")
      Peak(values(0).toDouble, values(1).toFloat)
    }
    result
  }

  private def _getTheoreticalFragments(ms_pep: ms_peptide): ms_fragmentvector = {
    /* Returned value DO NOT delete ! */
    val all_fragments = new ms_fragmentvector() // Keep a list of fragments from all series

    /* Allocated MS objetcs to delete */
    val mascotErrs = new ms_errs() // mascotAAHelper.getErrorHandler()
    val fragments = new ms_fragmentvector()
    var new_ms_pep: ms_peptide = null

    try {
      val mascotAAHelper = getMascotAAHelper(ms_pep, ptmHelper)
      val varModString = ms_pep.getVarModsStr()

      val hasMod = varModString matches "0+"
      var nlString = ms_pep.getPrimaryNlStr()

      //    	 logger.debug("seq = "+ms_pep.getPeptideStr())
      //         logger.debug("mod String = "+ varModString)
      //         logger.debug("nl String = "+ nlString)

      // Test if empty nlString can influence the call to calcFragment ?? NO
      val hasNL = if (nlString.isEmpty) {
        //generate a trail of c
        nlString = new String(Array.fill(varModString.length)('0'))
        ms_pep.setPrimaryNlStr(nlString)
        //      logger.debug("Modified nl String = "+ nlString)
        false
      } else true

      if (mascotVersion.startsWith("2.2")) {
        // swap 1 and 2 indices
        nlString = nlString.replace('2', 'X').replace('1', '2').replace('X', '1')
        ms_pep.setPrimaryNlStr(nlString)

        //      logger.debug("### Mascot 2.2 file :  modified nlString = "+ nlString)
      }

      // Note: for modified ms_pep (var mod or NL) we need to create a new ms_peptide object since they
      // can cause msparser JVM crash such as EXCEPTION_ACCESS_VIOLATION (see redmine defect #7550).
      new_ms_pep = if (hasMod == false && hasNL == false) ms_pep
      else _createPeptideFrom(ms_pep.getPeptideStr, ms_pep.getCharge, varModString, nlString, mascotAAHelper)

      //    logger.debug("### calc fragments for each serie configured in fragmentation rules")
      for (series <- ms_fragmentationrules.getFirstSeries to ms_fragmentationrules.getLastSeries) {

        if (mascotFragRules.isSeriesUsed(series)) {

          def calcFragments(isDoublyCharged: Boolean) {

            mascotAAHelper.calcFragments(
              new_ms_pep,
              series, // ions series ID
              isDoublyCharged, // double-charged ions allowed
              25.0, // minimal fragment mass to return
              new_ms_pep.getMrExperimental(), // maximal fragment mass to return
              msparserConstants.MASS_TYPE_MONO,
              fragments,
              mascotErrs
            )

            if (mascotErrs.getNumberOfErrors > 0) {
              logger.error(mascotErrs.getNumberOfErrors + " error(s) in fragmentation of " + new_ms_pep.getPeptideStr)
              MascotErrorsHelper.logErrors(mascotErrs)
            }

            // Append calculated fragments to the list of all fragments
            for (i <- 0 until fragments.getNumberOfFragments) {
              all_fragments.appendFragment(fragments.getFragmentByNumber(i))
            }
          }

          // calculate 1+ fragments
          calcFragments(isDoublyCharged = false)

          // clear tmp fragments
          fragments.clearFragments()

          // If peptide is doubly charged
          // TODO: why charge > 1 ? this should be > 2 to have fragments doubly charged

          if ((new_ms_pep.getCharge > 1) && mascotFragRules.isCharged2Plus) {
            //mascotFragRules.isCharged2Plus  means cahrge2+ specified in fragrules file for the used instrument.
            //calculate 2+ fragments
            // AW: we need to make it impossible to calculate 2+ charged for ion serie 4. 
            // issue #9689: https://bioproj.extra.cea.fr/redmine/issues/9689
            // And issue #10682 https://bioproj.extra.cea.fr/redmine/issues/10682 : Same error with INTERNAL_YB & INTERNAL_YA series 
            if (series != ms_fragmentationrules.FRAG_IMMONIUM && series != ms_fragmentationrules.FRAG_INTERNAL_YA && series != ms_fragmentationrules.FRAG_INTERNAL_YB)
              calcFragments(isDoublyCharged = true)
            else
              calcFragments(isDoublyCharged = false)
          }
        }
      }

      if (hasNL) {

        //      logger.debug("List of "+all_fragments.getNumberOfFragments()+" fragments will be completed with Neutral Losses")

        for (m <- 0 until mascotAAHelper.getVarMods.getNumberOfModifications) {

          val mod = mascotAAHelper.getVarMods.getModificationByNumber(m)
          val modificationTitle = mod.getTitle
          //           logger.debug("Search NL for modification "+modificationTitle)

          val modificationCharacterCode = String.valueOf(m + 1)
          val modificationNLVector = mod.getNeutralLoss(msparserConstants.MASS_TYPE_MONO)

          if (modificationNLVector.size > 1) {

            for (f <- 0 until all_fragments.getNumberOfFragments) {

              val frag = all_fragments.getFragmentByNumber(f)
              val isReverseSeries = Fragmentation.isReverseSeries(frag.getSeriesName)

              val fragmentVarModStr =
                if (isReverseSeries)
                  varModString.substring(new_ms_pep.getPeptideStr.length - frag.getColumn + 1, varModString.length - 1)
                else varModString.substring(1, frag.getColumn + 1)

              val modCount = StringUtils.countMatches(fragmentVarModStr, modificationCharacterCode)
              if (modCount > 0) {

                val appliedNLIdx =
                  if (isReverseSeries) nlString.charAt(varModString.lastIndexOf(modificationCharacterCode)) - 48
                  else nlString.charAt(varModString.indexOf(modificationCharacterCode)) - 48

                for (nlIdx <- 0 until modificationNLVector.size.toInt) {

                  if ((appliedNLIdx != 0) && (nlIdx != (appliedNLIdx - 1))) {

                    // cancel the already applied NL and apply the current NL
                    val delta = modCount * (modificationNLVector.get(nlIdx) - modificationNLVector.get(appliedNLIdx - 1))

                    val newFragment = new ms_fragment(
                      frag.getSeriesName,
                      frag.getMass - (delta / frag.getCharge.toDouble),
                      scala.math.round(modCount * modificationNLVector.get(nlIdx)),
                      frag.getColumn,
                      frag.getCharge
                    )
                    all_fragments.appendFragment(newFragment)
                  }
                }
              }
            }
          }
        }
      } // End of if hasNL

    } finally {

      if (FREE_MEMORY) {
        /* Free memory in finally block (reverse order) */

        if ((new_ms_pep != null) && (new_ms_pep ne ms_pep)) {
          try {
            new_ms_pep.delete() // delete only newly created peptides
          } catch {
            case t: Throwable => logger.error("Error deleting new_ms_pep", t)
          }
        }

        /* Delete elements then collection */
        for (i <- 0 until fragments.getNumberOfFragments) {
          try {
            fragments.getFragmentByNumber(i).delete()
          } catch {
            case t: Throwable => logger.error("Error deleting fragment #" + i, t)
          }
        }

        try {
          fragments.clearFragments()
          fragments.delete()
        } catch {
          case t: Throwable => logger.error("Error deleting fragments Vector", t)
        }

        if (mascotErrs != null) {
          try {
            mascotErrs.clearAllErrors()
            mascotErrs.delete()
          } catch {
            case t: Throwable => logger.error("Error deleting mascotErrs", t)
          }
        }

      }

    } // End of try -finally block

    //     logger.debug("Total number of fragments (with neutral losses): " + all_fragments.getNumberOfFragments )

    all_fragments
  }

  private def _createPeptideFrom(peptideStr: String,
                                 charge: Int,
                                 modStr: String,
                                 nlStr: String,
                                 aaHelper: ms_aahelper): ms_peptide = {

    var mascotPep: ms_peptide = null

    val mascotErrs = new ms_errs()
    val numModded = new vectori()
    val whichNL = new vectori()

    try {
      // Fill numModded and whichNL vectors
      for (k <- 0 until modStr.length) {
        numModded.add(modStr.charAt(k) - 48)
        whichNL.add(nlStr.charAt(k) - 48)
      }

      //     logger.debug("Creating peptide of sequence '"+peptideStr+"'...")

      mascotPep = aaHelper.createPeptide(
        peptideStr,
        peptideStr.length(),
        1,
        peptideStr.length(), // end positions
        numModded, // modification string-like vector
        whichNL, // which neutral loss to use
        charge, // no charge
        msparserConstants.MASS_TYPE_MONO,
        mascotErrs // collect errors in it
      )

      if (!mascotErrs.isValid) {
        logger.error(mascotErrs.getNumberOfErrors + " error(s) during creation of peptide with sequence '" + peptideStr + "'")
        MascotErrorsHelper.logErrors(mascotErrs)
      }

    } finally {
      // Free memory
      if (FREE_MEMORY) {
        /* Free memory in finally block (reverse order) */

        try {
          whichNL.clear()
          whichNL.delete()
        } catch {
          case t: Throwable => logger.error("Error deleting whichNL", t)
        }

        try {
          numModded.clear()
          numModded.delete()
        } catch {
          case t: Throwable => logger.error("Error deleting numModded", t)
        }

        try {
          mascotErrs.clearAllErrors()
          mascotErrs.delete()
        } catch {
          case t: Throwable => logger.error("Error deleting mascotErrs", t)
        }

      }

    } // End of try -finally block

    //     logger.debug("ms_peptide created")

    mascotPep
  }

  protected def _buildFragmentationTable(pepSequence: String, mascotTheoFragments: ms_fragmentvector): Array[TheoreticalFragmentSeries] = {

    val mascotTheoFragsBySeriesName = new collection.mutable.HashMap[String, ArrayBuffer[ms_fragment]]()

    // Index fragments by serie's name
    for (k <- 0 until mascotTheoFragments.getNumberOfFragments) {

      val mascotFrag = mascotTheoFragments.getFragmentByNumber(k)
      val key = if (mascotFrag.getCharge == 2) mascotFrag.getSeriesName + "++" //throw new Exception(mascotFrag.getSeriesName)
      else mascotFrag.getSeriesName

      mascotTheoFragsBySeriesName.getOrElseUpdate(key, new ArrayBuffer[ms_fragment]) += mascotFrag
    }

    val pepSeqLength = pepSequence.length
    val rules = mascotFragRules
    var columnIdx = 1

    val fragTable = new ArrayBuffer[TheoreticalFragmentSeries]()
    for (k <- ms_fragmentationrules.getFirstSerie to ms_fragmentationrules.getLastSeries) {

      var charge = 1
      var mascotFragSeriesName = ms_fragmentationrules.getSeriesName(k)

      if (rules.isSeriesUsed(k)) {

        def addCurrentSeriesToTable() {

          val theoMasses = Array.fill(pepSeqLength)(0.0)

          for (mascotFragment <- mascotTheoFragsBySeriesName(mascotFragSeriesName)) {
            // Test if the theoMasses[i] had already been affected (ie not null), since due to neutral loss it can happen that 
            // multiple fragment exists at the same position for the same serie (with the default NL value and the alternative NL).
            val idx = if (Fragmentation.isReverseSeries(mascotFragSeriesName)) {
              pepSeqLength - mascotFragment.getColumn
            } else {
              mascotFragment.getColumn - 1
            }
            if (theoMasses(idx) == 0.0) theoMasses(idx) = mascotFragment.getMass
          }

          val fragSeriesName = fragSeriesByMascotFragSeries.getOrElse(mascotFragSeriesName, mascotFragSeriesName)

          fragTable += new TheoreticalFragmentSeries(fragSeriesName, theoMasses)
          columnIdx += 1
        }

        // Check singly charged series
        if (mascotTheoFragsBySeriesName.contains(mascotFragSeriesName)) addCurrentSeriesToTable()

        // Check doubly charged series
        charge = 2
        mascotFragSeriesName += "++"
        if (mascotTheoFragsBySeriesName.contains(mascotFragSeriesName)) addCurrentSeriesToTable()

      }
    }

    fragTable.toArray
  }

  /*protected def buildSeries( pep: IPeptide, allFragments: ms_fragmentvector): Array[PeptideFragmentation.Serie] = {
    
    val series = new collection.mutable.HashMap[String, ListBuffer[ms_fragment]]()
    
    // Index fragments by serie's name
    for ( k <- 0 until allFragments.getNumberOfFragments ) {
      
      val f = allFragments.getFragmentByNumber(k)
      var key = f.getSeriesName
      
      if ( f.getCharge == 2 ) key += "++"
      
      series.getOrElseUpdate(key,new ListBuffer[ms_fragment]) += f
    }
    
    val rules =  fragRules
    var columnIdx = 1
    
    val result = new ArrayBuffer[PeptideFragmentation.Serie]()
    for ( k <- ms_fragmentationrules.getFirstSerie to ms_fragmentationrules.getLastSeries ) {
      
      var seriesName = ms_fragmentationrules.getSeriesName(k)
      if ( rules.isSeriesUsed(k) ) {
        
        def addCurrentSerieToResult() {
          val serie = new PeptideFragmentation.Serie(seriesName)
          serie.isReverse = Fragmentation.isReverseSeries(seriesName)
          serie.values = new Array[Object](pep.getSequenceStr.length)
          
          for( fragment <- series(seriesName) ) {
            // Test if the serie.values[i] had already been affected (ie not null), since due to neutral loss it can happen that 
            // multiple fragment exists at the same position for the same serie (with the default NL value and the alternative NL).
            if (serie.isReverse) {
              if ( serie.values(pep.getSequenceStr.length - fragment.getColumn) == null )
                   serie.values(pep.getSequenceStr.length - fragment.getColumn) = fragment.getMass.asInstanceOf[Object]
            } else {
              if ( serie.values(fragment.getColumn - 1) == null )
                   serie.values(fragment.getColumn - 1) = fragment.getMass.asInstanceOf[Object]
            }
          }
        
          result += serie
          columnIdx += 1
        }
        
        // Check singly charged series
        if ( series.contains(seriesName) ) addCurrentSerieToResult()
        
        // Check doubly charged series
        seriesName += "++"
        if ( series.contains(seriesName) ) addCurrentSerieToResult()
          
      }
    }
    
    result.toArray
  }
    
*/
  protected def getMascotAAHelper(ms_pep: ms_peptide, ptmHelper: MascotPTMHelper): ms_aahelper = {

    val msQuantComp = _getMascotQuantComponent(ms_pep)

    if (!aaHelpersByQuantComp.contains(msQuantComp.getOrElse(DEFAULT_QUANT_COMP_KEY))) {
      var masses = mascotConfig.massesFile
      masses = new ms_masses(masses)
      if (msQuantComp.isDefined)
        masses.applyIsotopes(mascotConfig.unimodFile, quantitationMethod.getComponentByName(msQuantComp.get))
      aaHelpersByQuantComp(msQuantComp.getOrElse(DEFAULT_QUANT_COMP_KEY)) = _buildMascotAAHelper(ms_pep, ptmHelper, masses)
    }

    aaHelpersByQuantComp(msQuantComp.getOrElse(DEFAULT_QUANT_COMP_KEY))
  }

  /* Created ms_aahelper are deleted by calling clear() */
  private def _buildMascotAAHelper(ms_pep: ms_peptide, ptmHelper: MascotPTMHelper, masses: ms_masses): ms_aahelper = {

    val aahelper = new ms_aahelper()
    aahelper.setMasses(masses)

    if (mascotVersion.startsWith("1.") || mascotVersion.startsWith("2.1")) {

      // 
      // Mascot version 2.1 : use datFile to build ms_modifications, however mod description
      // is not complete : ModificationType and Residue modification is not set 

      logger.debug("Build ms_aahelper for Mascot version <= 2.1")
      val anotherHelper = new ms_aahelper(mascotResFile, null)
      val vecFixed = anotherHelper.getFixedMods()

      for (m <- 1 until vecFixed.getNumberOfModifications) {

        val mod = vecFixed.getModificationByNumber(m)
        val ptms = ptmHelper.fixedPtmDefsByModName(mod.getTitle())

        for (ptmDef <- ptms) {

          if (ptmDef.location.endsWith("N-term")) {
            mod.setModificationType(msparserConstants.MOD_TYPE_N_TERM)
          } else if (ptmDef.location.endsWith("C-term")) {
            mod.setModificationType(msparserConstants.MOD_TYPE_C_TERM)
          } else {
            mod.setModificationType(msparserConstants.MOD_TYPE_RESIDUE)

            val mascotMasses = mascotConfig.massesFile
            val r = ptmDef.residue
            mod.appendModifiedResidue(
              r,
              mascotMasses.getResidueMass(msparserConstants.MASS_TYPE_MONO, r),
              mascotMasses.getResidueMass(msparserConstants.MASS_TYPE_AVE, r)
            )
          }
        }
      }
      val vecVariable = anotherHelper.getVarMods()
      aahelper.setAvailableModifications(vecFixed, vecVariable)

    } else {

      //
      // Mascot version 2.2 
      //       - Use modfile to build modification list
      logger.debug("Build ms_aahelper for Mascot version >= 2.2")

      val modsFile = mascotConfig.modificationsFile

      // create a list of fixed modifications
      val vecFixed = new ms_modvector()
      for (fixedPtms <- ptmHelper.fixedPtmIndexed) {
        
        var mod = modsFile.getModificationByName(fixedPtms)
        var foundModif = false
        
        if (mod != null) {
          //Modification found in mascot modification file 
          logger.debug("+++ Fixed Modification " + mod.getTitle() + " added")
          foundModif = true
          
        } else {
          
          // Modif doesn't exist in mascot modification file, get it from dat file.    
          //TODO extract ms_umod_configfile to create once !
          val unimodFile = getMascotUnimodXSDFile() 
          if(unimodFile != null){
         
            val umodFile = new ms_umod_configfile()
            umodFile.setSchemaFileName(unimodFile.getAbsolutePath())
            val foundUnimod = mascotResFile.getUnimod(umodFile)
            if(foundUnimod){
            	mod = new ms_modification()
            	foundModif =  mod.getFromUnimod(fixedPtms, umodFile)
            	if(foundModif)
		    	logger.debug("+++ Fixed Modification " + mod.getTitle() + " added (from dat file)")
            }
          }
          
        }//Modification not found in modFile
          
        if(!foundModif){
          logger.error("Fixed Modification " + fixedPtms + " cannot be created")
        }  else { 
    	  vecFixed.appendModification(mod)    	  
        }  
      }

      // create a list of variable modifications :
      val vecVariable = new ms_modvector()
      for (varPtms <- ptmHelper.varPtmIndexed) {

        var mod = modsFile.getModificationByName(varPtms)
        var foundModif = false
        
        if (mod != null) {
          //Modification found in mascot modification file 
          logger.debug("+++ Var Modification " + mod.getTitle() + " added")
          foundModif = true 
          
        } else {
          
          // Modif doesn't exist in mascot modification file, get it from dat file.
          val unimodFile = getMascotUnimodXSDFile() 
          if(unimodFile != null){

            val umodFile = new ms_umod_configfile()
            umodFile.setSchemaFileName(unimodFile.getAbsolutePath())
            val foundUnimod = mascotResFile.getUnimod(umodFile)
          
            if(foundUnimod){
            	mod = new ms_modification()
            	foundModif =  mod.getFromUnimod(varPtms, umodFile)		   
            	if(foundModif)
            		logger.debug("+++ Var Modification " + mod.getTitle() + " added (from dat file)")    	
            }
          }
        } //Modification not found in modFile
        
        if(!foundModif ) {
    	  logger.error("Variable Modification " + varPtms + " cannot be created")
 		} else {
		  vecVariable.appendModification(mod)	    		   		 
     	}
        
      } //End Variable Modifs

      aahelper.setAvailableModifications(vecFixed, vecVariable)

    }

    // for Mascot 2.1 files, it is necessary to update mods ... 
    if (mascotVersion.startsWith("2.1")) {
      val vecVariable = aahelper.getVarMods()

      for (m <- 0 until vecVariable.getNumberOfModifications) {
        val mod = vecVariable.getModificationByNumber(m)
        _updateVarModification(mod, ms_pep, ptmHelper.indexOfVarPtm(mod.getTitle()))
      }
      aahelper.setAvailableModifications(aahelper.getFixedMods(), vecVariable)

      //       logger.debug(" ###### Modified Mascot 2.1 var mods")
      //      MascotErrorsHelper.logMods(vecVariable)
    }

    m_createdAAHelpers.synchronized {
      m_createdAAHelpers += aahelper
    } // End of synchronized block on m_createdAAHelpers

    aahelper
  }

  private def getMascotUnimodXSDFile(): File = {
    
    val localUnimodSchemaPath = "mascot_config/unimod_2.xsd"

    var unimodShemaFile: File = null

    try {
      val cl = Thread.currentThread.getContextClassLoader

      val unimodSchemaURL = cl.getResource(localUnimodSchemaPath)

      if (unimodSchemaURL == null) {
        logger.warn("Unable to locate [" + localUnimodSchemaPath + "] resource from ClassLoader")
      } else {
        val f = new File(unimodSchemaURL.toURI())

        if (f.isFile()) {
          unimodShemaFile = f
        }

      }

    } catch {
      case ex: Exception => logger.error("Unable to locate [" + localUnimodSchemaPath + "] file", ex)
    }

    unimodShemaFile
    
  }
  
  private def _getMascotQuantComponent(ms_pep: ms_peptide): Option[String] = {

    if (ms_pep.getComponentStr != null && !(ms_pep.getComponentStr.length == 0)) {

      val quantMethod = quantitationMethod
      if (quantMethod != null && quantMethod.getName != mascotConfig.NONE_QUANTI_METHOD_NAME) {

        val pepComponentStr = ms_pep.getComponentStr
        logger.debug("ms_pep componentStr = " + pepComponentStr)

        val msQuantComp = quantMethod.getComponentByName(pepComponentStr)
        logger.debug("ms_quant_component name = " + msQuantComp.getName)
        Some(msQuantComp.getName)
      }
    }
    None
  }

  private def _updateVarModification(mod: ms_modification, ms_pep: ms_peptide, index: Int): ms_modification = {

    val mascotMasses = mascotConfig.massesFile
    val modStr = ms_pep.getVarModsStr
    var isModTypeSet = false

    for (k <- 0 until modStr.length) {

      val modIdx = Integer.valueOf(modStr.substring(k, k + 1))
      if (modIdx == index) {

        val title = mod.getTitle()

        if (k == 0) { //N-term modification

          if (title matches "Protein")
            mod.setModificationType(msparserConstants.MOD_TYPE_N_PROTEIN)
          else mod.setModificationType(msparserConstants.MOD_TYPE_N_TERM)

          isModTypeSet = true

        } else if (k == modStr.length() - 1) { //C-term modification

          if (title matches "Protein")
            mod.setModificationType(msparserConstants.MOD_TYPE_C_PROTEIN)
          else mod.setModificationType(msparserConstants.MOD_TYPE_C_TERM)

          isModTypeSet = true
        } else {
          mod.setModificationType(msparserConstants.MOD_TYPE_RESIDUE)
          isModTypeSet = true

          val r = ms_pep.getPeptideStr().charAt(k - 1)
          val delta = mod.getDelta(msparserConstants.MASS_TYPE_MONO)
          val r_mono = delta + mascotMasses.getResidueMass(msparserConstants.MASS_TYPE_MONO, r)
          val r_avg = delta + mascotMasses.getResidueMass(msparserConstants.MASS_TYPE_AVE, r)
          mod.appendModifiedResidue(r, r_mono, r_avg)
        }
      }
    }

    if (!isModTypeSet) {
      if (mod.getTitle matches "Protein")
        mod.setModificationType(msparserConstants.MOD_TYPE_N_PROTEIN)
      else
        mod.setModificationType(msparserConstants.MOD_TYPE_C_PROTEIN)
    }

    mod
  }

  private def doClear(fromFinalize: Boolean) {

    m_createdAAHelpers.synchronized {

      if (!m_createdAAHelpers.isEmpty) {

        if (fromFinalize) {
          logger.warn("Clearing MascotSpectrumMatcher ms_aahelper from finalize")
        } else {
          logger.debug("Clearing MascotSpectrumMatcher ms_aahelper ...")
        }

        for (aahelper <- m_createdAAHelpers) {

          try {
            aahelper.delete()
          } catch {
            case t: Throwable => logger.error("Error deleting aahelper", t)
          }

        } // End loop for each aahelper

      } // End if (m_createdAAHelpers is not empty)

      m_createdAAHelpers.clear()
    } // End of synchronized block on m_createdAAHelpers

  }

}
