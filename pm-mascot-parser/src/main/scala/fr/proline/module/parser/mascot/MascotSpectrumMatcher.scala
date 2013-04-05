package fr.proline.module.parser.mascot

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.break
import scala.util.control.Breaks.breakable

import org.apache.commons.lang3.StringUtils

import com.weiglewilczek.slf4s.Logging

import fr.proline.core.om.model.msi.Fragmentation
import fr.proline.core.om.model.msi.FragmentMatch
import fr.proline.core.om.model.msi.FragmentMatchType
import fr.proline.core.om.model.msi.SpectrumMatch
import fr.proline.core.om.model.msi.TheoreticalFragmentSeries
import fr.proline.util.ms.{calcMozTolInDalton,MassTolUnit}

import matrix_science.msparser.ms_aahelper
import matrix_science.msparser.ms_errs
import matrix_science.msparser.ms_fragment
import matrix_science.msparser.ms_fragmentationrules
import matrix_science.msparser.ms_fragmentvector
import matrix_science.msparser.ms_mascotresfile
import matrix_science.msparser.ms_peptide
import matrix_science.msparser.ms_quant_method
import matrix_science.msparser.msparserConstants
import matrix_science.msparser.vectori

trait LoggingFake {
  object logger {
    def info(s: String) {}
    def debug(s: String) {}
    def error(s: String) {}
  }
}

case class Peak( moz: Double, intensity: Float )

class MascotSpectrumMatcher( mascotResFile: ms_mascotresfile, mascotConfig: IMascotConfig ) extends LoggingFake {
  
  val mascotSearchParams = mascotResFile.params
  val mascotVersion = mascotResFile.getMascotVer  
  val mascotFragRules = mascotConfig.fragmentationRulesFile.getInstrumentByName(mascotSearchParams.getINSTRUMENT())
  val mascotAAHelper = new ms_aahelper( mascotResFile, "" )
  
  // Map Mascot fragmentation series names by internal ones (handle doubly charged state)
  val fragSeriesByMascotFragSeries = Map()++
                                     Fragmentation.defaultIonTypeByMascotSeriesName.map( p => p._1 -> p._2.toString ) ++
                                     Fragmentation.defaultIonTypeByMascotSeriesName.map( p => (p._1+"++") -> (p._2.toString+"++") )
  
  val quantitationMethod: ms_quant_method = {

    val quantMethodName = mascotSearchParams.getQUANTITATION
    val quantCfgFile = mascotConfig.quantitationFile
    
    if (quantMethodName == null || quantCfgFile == null) null
    else  quantCfgFile.getMethodByName(quantMethodName)
  }
  
  def getSpectrumMatch( mascotPep: ms_peptide ): SpectrumMatch = { //, spectrum: Spectrum
    
    val pepSequence = mascotPep.getPeptideStr
    val mascotQueryId = mascotPep.getQuery
    
    this.logger.debug("generate fragments and matches for query="+ mascotQueryId +" and seq="+pepSequence )
    val usedPeaksCount = mascotPep.getPeaksUsedFromIons1() // was numUsed    
    
    val(ms2ErrorTol,ms2ErrorTolUnitStr) = (mascotSearchParams.getITOL,mascotSearchParams.getITOLU)
    val ms2ErrorTolUnit = MassTolUnit.withName(ms2ErrorTolUnitStr)
    //val ms2ErrorTolInDa = calcMozTolInDalton( )
    
    // To find the matching peaks we need to have them in the correct order
    // Mascot store peaks in a custom order so we have to parse again the data
    val ionsStr = mascotResFile.getQuerySectionValueStr(mascotQueryId, "Ions1")
    val allPeaks = this.parseMascotQueryIonsStr(ionsStr)
    
    var usedPeaks = new ArrayBuffer[Peak]( usedPeaksCount + 2 )
    
    // Fill usedPeaks with firstSortedPeaks data  
    usedPeaks += Peak(0.0,0f)
    usedPeaks ++= allPeaks.take(usedPeaksCount).sortBy( _.moz )
    usedPeaks += Peak(Double.MaxValue,0f)
    
    // FIXME: is this really needed ?
    //intensities.put(m.get(0), intensities.get(m.get(1)))
    //intensities.put(m.get(m.size() - 1), intensities.get(m.get(m.size() - 2)))
    
    // Retrieve full spectrum peaks
    val allPeaksSorted = allPeaks.sortBy( _.moz )
    //val allMozList = spectrum.mozList.get
    //val allIntList = spectrum.intensityList.get
    
    // Define some vars
    var prevPeak: Peak = null
    var j = 0
    
    // Iterate over matching peaks
    for ( val i <- 0 until usedPeaks.length ) {
      val peak = usedPeaks(i)
      var intThresh = peak.intensity
      
      // TODO: explain this computation
      if ((i > 0) && (i < (usedPeaks.length - 1)) && (prevPeak.intensity < peak.intensity) ) {
        intThresh = prevPeak.intensity
      }
      
      while ( (j < allPeaksSorted.length) && (allPeaksSorted(j).moz < peak.moz) ) {
        val newPeak = allPeaksSorted(j)
        if (newPeak.intensity >= intThresh) {
          usedPeaks += newPeak
        }
        j += 1
      }
      j += 1
      
      prevPeak = peak
    }
    
    // Sort peaks again because some peaks have been added since the first sorting
    usedPeaks = usedPeaks.sortBy( _.moz )    
    //val mozList = sortedPeaks.map(_._1).sortBy(_.asInstanceOf[Double])    
    
    val theoFragments = this._getTheoreticalFragments(mascotPep)    
    val fragMatches = new ArrayBuffer[FragmentMatch]()
    
    for ( k <- 0 until theoFragments.getNumberOfFragments ) {
      
      val fragment = theoFragments.getFragmentByNumber(k)
      val theoFragMoz = fragment.getMass
      val mozTolInDa = calcMozTolInDalton( theoFragMoz, ms2ErrorTol, ms2ErrorTolUnit )
      var bestMatch: FragmentMatch = null
      
      breakable {
        for ( ms2Peak <- usedPeaks ) {
          
          val obsMoz = ms2Peak.moz
          val deltaMoz = obsMoz- theoFragMoz
          
          if ( Math.abs(deltaMoz) <= mozTolInDa ) {
            
            val fragType = if(fragment.isRegular) FragmentMatchType.REGULAR
                           else if (fragment.isInternal) FragmentMatchType.INTERNAL 
                           else FragmentMatchType.IMMONIUM
                 
            val fragMatch = new FragmentMatch(
              label = fragment.getLabel,
              //ionSeries = fragment.getSeriesName,
              // TODO: check is this is the expected value
              //aaPosition = if( fragment.isInternal) fragment.getStart else fragment.getColumn,
              `type` = fragType.toString,
              //charge = fragment.getCharge,
              moz = obsMoz,
              calculatedMoz = theoFragMoz,
              intensity = ms2Peak.intensity
            )
            
            val nl = fragment.getNeutralLoss
            if ( nl > 0.0) fragMatch.neutralLossMass = Some(nl)
            
            if ( (bestMatch == null) || (bestMatch.intensity < fragMatch.intensity) ) {
              bestMatch = fragMatch
            }
            
          } else if ( Math.signum(deltaMoz) > 0 ) break
        }
      }
        
      if ( bestMatch != null ) fragMatches += bestMatch
    }
    
    val fragmentationTable = this._buildFragmentationTable( pepSequence, theoFragments )
    
    new SpectrumMatch( mascotQueryId, fragmentationTable, fragMatches.toArray )
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
  
  protected def parseMascotQueryIonsStr( mascotQueryPeaksStr: String ): Array[Peak] = {
    val peaks = mascotQueryPeaksStr.split(",")
    //val firstPeaks = if( maxNbPeaks > 0 ) peaks.take(maxNbPeaks) else peaks
    
    peaks.map { peakAsStr =>
      val values = peakAsStr.split(":")
      Peak( values(0).toDouble, values(1).toFloat )
    }
  }
  
  private def _getTheoreticalFragments( ms_pep: ms_peptide ): ms_fragmentvector = {
    
    val mascotErrs = new ms_errs()
    val varModString = ms_pep.getVarModsStr()
    var nlString = ms_pep.getPrimaryNlStr()
    this.logger.debug("mod String = "+ varModString)
    this.logger.debug("nl String = "+ nlString)
    
    // Test if empty nlString can influence the call to calcFragment ?? NO
    if ( nlString.isEmpty ) {
      //generate a trail of c
      nlString = new String( Array.fill(varModString.length)('0') )
      ms_pep.setPrimaryNlStr(nlString)
      logger.debug("Modified nl String = "+ nlString)
    }
    
    if ( mascotVersion.startsWith("2.2") ) {
      
      nlString = nlString.replace('2', 'X').replace('1', '2').replace('X', '1')      
      ms_pep.setPrimaryNlStr(nlString)
      
      logger.debug("### Mascot 2.2 file :  modified nlString = "+ nlString)
    }
    
    // Need to create a new ms_peptide object since some ms_pep objects
    // extracted from the resultfile can cause msparser JVM crash.
    // TODO: try to explain why => is this still needed ?
    //val new_ms_pep = this._createPeptideFrom(ms_pep.getPeptideStr, ms_pep.getCharge, varModString, nlString, mascotAAHelper )
    val new_ms_pep = ms_pep
    val fragments = new ms_fragmentvector()
    val all_fragments = new ms_fragmentvector() // Keep a list of fragments from all series
    
    logger.debug("### calc fragments for each serie configured in fragmentation rules")
    for ( series <- ms_fragmentationrules.getFirstSeries to ms_fragmentationrules.getLastSeries ) {
      
      if ( mascotFragRules.isSeriesUsed(series) ) {
        
        def calcFragments( isDoublyCharged: Boolean ) {
          
          mascotAAHelper.calcFragments(
            new_ms_pep,
            series, // ions series ID
            isDoublyCharged, // double-charged ions allowed
            25.0, // minimal fragment mass to return
            new_ms_pep.getMrExperimental(), // maximal fragment mass to return
            msparserConstants.MASS_TYPE_MONO,
            fragments,
            mascotAAHelper.getErrorHandler
          )
  
          if ( mascotErrs.getNumberOfErrors > 0 ) {
            this.logger.error( mascotErrs.getNumberOfErrors + " error(s) in fragmentation of " + new_ms_pep.getPeptideStr )
            MascotErrorsHelper.logErrors(mascotErrs)
          }
          
          // Append calculated fragments to the list of all fragments
          for ( i <- 0 until fragments.getNumberOfFragments ) {
            all_fragments.appendFragment( fragments.getFragmentByNumber(i) )
          }
        }
        
        // calculate 1+ fragments
        calcFragments( isDoublyCharged = false )
        
        // clear tmp fragments
        fragments.clearFragments()
        
        // If peptide is doubly charged
        // TODO: why charge > 1 ? this should be > 2 to have fragments doubly charged
        if ( (new_ms_pep.getCharge > 1) && mascotFragRules.isCharged2Plus ) {
          //calculate 2+ fragments
          calcFragments( isDoublyCharged = true )
        }
      }
    }

    if ( (nlString != null) && (nlString.length > 0) ) {
      logger.debug("List of "+all_fragments.getNumberOfFragments()+" fragments will be completed with Neutral Losses")

      for ( m <- 0 until mascotAAHelper.getVarMods.getNumberOfModifications ) {
        
          val mod = mascotAAHelper.getVarMods.getModificationByNumber(m)
          val modificationTitle = mod.getTitle
          this.logger.debug("Search NL for modification "+modificationTitle)
          
          val modificationCharacterCode = String.valueOf(m+1)
          val modificationNLVector = mod.getNeutralLoss(msparserConstants.MASS_TYPE_MONO)
          
          if ( modificationNLVector.size > 1 ) {
            
            for ( f <- 0 until all_fragments.getNumberOfFragments ) {
              
              val frag = all_fragments.getFragmentByNumber(f)
              val isReverseSeries = Fragmentation.isReverseSeries(frag.getSeriesName)
              
              val fragmentVarModStr =
                if ( isReverseSeries )
                     varModString.substring( new_ms_pep.getPeptideStr.length - frag.getColumn + 1, varModString.length - 1 )
                else varModString.substring( 1, frag.getColumn + 1 )
              
              val modCount = StringUtils.countMatches(fragmentVarModStr, modificationCharacterCode)
              if ( modCount > 0 ) {
                
                val appliedNLIdx =
                  if ( isReverseSeries ) nlString.charAt(varModString.lastIndexOf(modificationCharacterCode)) - 48
                  else nlString.charAt(varModString.indexOf(modificationCharacterCode)) - 48
                
                for ( nlIdx <- 0 until modificationNLVector.size.toInt ) {
                  
                  if ( (appliedNLIdx != 0) && (nlIdx != (appliedNLIdx-1)) ) {
                    
                    // cancel the already applied NL and apply the current NL
                    val delta = modCount * (modificationNLVector.get(nlIdx) - modificationNLVector.get(appliedNLIdx-1))
                    
                    val newFragment = new ms_fragment(
                                            frag.getSeriesName,
                                            frag.getMass - (delta / frag.getCharge.toDouble),
                                            Math.round( modificationNLVector.get(nlIdx) / frag.getCharge.toDouble ),
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
    }
    
    this.logger.debug("Total number of fragments (with neutral losses): " + all_fragments.getNumberOfFragments )
    
    all_fragments
  }

  private def _createPeptideFrom( peptideStr: String,
                                  charge: Int,
                                  modStr: String,
                                  nlStr: String,
                                  aaHelper: ms_aahelper ): ms_peptide = {
    
    val mascotErrs = new ms_errs()
    val numModded = new vectori()
    val whichNL = new vectori()
    
    // Fill numModded and whichNL vectors
    for( k <- 0 until modStr.length ) {
      numModded.add( modStr.charAt(k) - 48 )
      whichNL.add( nlStr.charAt(k) - 48 )
    }
    
    this.logger.debug("Creating peptide of sequence '"+peptideStr+"'...")
    
    val mascotPep = aaHelper.createPeptide(
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

    if ( !mascotErrs.isValid ) {
      this.logger.error(mascotErrs.getNumberOfErrors+" error(s) during creation of peptide with sequence '"+ peptideStr+"'")
      MascotErrorsHelper.logErrors(mascotErrs)
    }
    
    this.logger.debug("Peptide created")
    
    mascotPep
  }
  
  protected def _buildFragmentationTable( pepSequence: String, mascotTheoFragments: ms_fragmentvector): Array[TheoreticalFragmentSeries] = {
    
    val mascotTheoFragsBySeriesName = new collection.mutable.HashMap[String, ArrayBuffer[ms_fragment]]()
    
    // Index fragments by serie's name
    for ( k <- 0 until mascotTheoFragments.getNumberOfFragments ) {
      
      val mascotFrag = mascotTheoFragments.getFragmentByNumber(k)
      val key = if ( mascotFrag.getCharge == 2 ) mascotFrag.getSeriesName + "++" //throw new Exception(mascotFrag.getSeriesName)
                else mascotFrag.getSeriesName
      
      mascotTheoFragsBySeriesName.getOrElseUpdate(key,new ArrayBuffer[ms_fragment]) += mascotFrag
    }
    
    val pepSeqLength = pepSequence.length
    val rules = this.mascotFragRules
    var columnIdx = 1
    
    val fragTable = new ArrayBuffer[TheoreticalFragmentSeries]()
    for ( k <- ms_fragmentationrules.getFirstSerie to ms_fragmentationrules.getLastSeries ) {
      
      var charge = 1
      var mascotFragSeriesName = ms_fragmentationrules.getSeriesName(k)
      
      if ( rules.isSeriesUsed(k) ) {
        
        def addCurrentSeriesToTable() {
          
          val theoMasses = Array.fill(pepSeqLength)(0.0)
          
          for( mascotFragment <- mascotTheoFragsBySeriesName(mascotFragSeriesName) ) {
            // Test if the theoMasses[i] had already been affected (ie not null), since due to neutral loss it can happen that 
            // multiple fragment exists at the same position for the same serie (with the default NL value and the alternative NL).
            if ( Fragmentation.isReverseSeries(mascotFragSeriesName) ) {
              val idx = pepSeqLength - mascotFragment.getColumn
              if ( theoMasses(idx) == 0.0 ) theoMasses(idx) = mascotFragment.getMass
            } else {
              val idx = mascotFragment.getColumn - 1
              if ( theoMasses(idx) == 0.0 ) theoMasses(idx) = mascotFragment.getMass
            }
          }
          
          val fragSeriesName = fragSeriesByMascotFragSeries.getOrElse(mascotFragSeriesName,mascotFragSeriesName)
          
          fragTable += new TheoreticalFragmentSeries(fragSeriesName, theoMasses)
          columnIdx += 1
        }
        
        // Check singly charged series
        if ( mascotTheoFragsBySeriesName.contains(mascotFragSeriesName) ) addCurrentSeriesToTable()
        
        // Check doubly charged series
        charge = 2
        mascotFragSeriesName += "++"
        if ( mascotTheoFragsBySeriesName.contains(mascotFragSeriesName) ) addCurrentSeriesToTable()
        
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
    
    val rules = this.fragRules
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
    

  protected def getMascotAAHelper( ptmHelper: PTMHelper, ms_pep: ms_peptide ): ms_aahelper = {
    
    val aahelper = new ms_aahelper()
    var masses = this.mascotConfig.massesFile
    
    if (ms_pep.getComponentStr() != null && ! (ms_pep.getComponentStr().length() == 0)) { 
      masses = new ms_masses(masses)
      
      val quantMethod = this.quantitationMethod
      if ( quantMethod != null && quantMethod.getName != this.mascotConfig.NONE_QUANTI_METHOD_NAME ) {
       
        val pepComponentStr = ms_pep.getComponentStr()
        this.logger.debug("ms_pep componentStr = "+ pepComponentStr)
        
        val msQuantComp = quantMethod.getComponentByName(pepComponentStr)       
        this.logger.debug("ms_quant_component name = "+ msQuantComp.getName())
        
        masses.applyIsotopes(this.mascotConfig.unimodFile, msQuantComp)
      }
    }
    aahelper.setMasses(masses)
    
    if (mascotVersion.startsWith("2.2") || mascotVersion.startsWith("2.3")) {
      //
      // Mascot version 2.2 
      //       - Use modfile to build modification list
      
      val modsFile = this.mascotConfig.modificationsFile
      
      // create a list of fixed modifications
      val vecFixed = new ms_modvector()
      for ( fixedPtms <- ptmHelper.getAllFixedPTMs() ) {
        this.logger.debug(" --- add fixed modifications IPTM : "+fixedPtms.getName())
        
        modsFile.
        val mod = modsFile.getModificationByName(fixedPtms.getName())
        
        if (mod != null) {
            vecFixed.appendModification(mod)
        } else {
            logger.error("Modification " + fixedPtms.getName() + " cannot be created")
        }                   
      }
      
      // create a list of variable modifications :
      val vecVariable = new ms_modvector()
      for ( varPtms <- ptmHelper.getAllVariablePTMs ) {
          logger.debug(" --- add variable modifications IPTM : "+varPtms.getName())
          
          val mod = modsFile.getModificationByName(varPtms.getName())
          if (mod != null) {
              vecVariable.appendModification(mod)
          } else {
              logger.error("Modification " + varPtms.getName() + " cannot be created")
          }
      }                           
      
      aahelper.setAvailableModifications(vecFixed, vecVariable)
    } else {
      // 
      // Mascot version 2.1 : use datFile to build ms_modifications, however mod description
      // is not complete : ModificationType and Residue modification is not set 
      
      val anotherHelper = new ms_aahelper(mascotResFile, null)
      val vecFixed = anotherHelper.getFixedMods()
      
      for( val m <- 1 until vecFixed.getNumberOfModifications ) {
        
        val mod = vecFixed.getModificationByNumber(m)
        val ptm: PTM = ptmHelper.getFixedPTM(mod.getTitle(), null)
        val residues = mascotSearchParams.getFixedModsResidues(ptm.getIndex())
        
        if (residues.startsWith("N-term")) {
            mod.setModificationType(msparserConstants.MOD_TYPE_N_TERM)
        } else if (residues.startsWith("C-term")) {
            mod.setModificationType(msparserConstants.MOD_TYPE_C_TERM)
        } else {
          mod.setModificationType(msparserConstants.MOD_TYPE_RESIDUE)
          
          val mascotMasses = this.mascotConfig.massesFile
          for (  k <- 0 until residues.length ) {
            val r = residues.charAt(k)
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
    }
    this.logger.debug("###### Original Mascot var mods")
    MascotLoggingHelper.logMods(aahelper.getVarMods())
    
    // update masses if necessary 
    masses = this.mascotConfig.massesFile
    if (ms_pep.getComponentStr() != null && ! (ms_pep.getComponentStr().length() == 0)) { 
      masses = new ms_masses(masses)
      
      val quantMethod = this.quantitationMethod
      if ( quantMethod != null && quantMethod.getName != this.mascotConfig.NONE_QUANTI_METHOD_NAME ) {
        
        val pepComponentStr = ms_pep.getComponentStr()
        this.logger.debug("ms_pep componentStr = "+ pepComponentStr)
        
        val msQuantComp = quantMethod.getComponentByName(pepComponentStr)
        this.logger.debug("ms_quant_component name = "+msQuantComp.getName())
        
        masses.applyIsotopes(this.mascotConfig.unimodFile, msQuantComp)
        aahelper.setMasses(masses)
      }
    }
    
    // for Mascot 2.1 files, it is necessary to update mods ... 
    if (mascotVersion.startsWith("2.1")) {
      val vecVariable = aahelper.getVarMods()
      
      for(  m <- 0 until vecVariable.getNumberOfModifications ) {
        val mod = vecVariable.getModificationByNumber(m)
        val ptm: PTM = ptmHelper.getVariablePTM(mod.getTitle(), null)
        this._updateVarModification(mod, ms_pep, ptm.getIndex())
      }
      aahelper.setAvailableModifications(aahelper.getFixedMods(), vecVariable)
      
      this.logger.debug(" ###### Modified Mascot 2.1 var mods")
      MascotLoggingHelper.logMods(vecVariable)
    } 

    aahelper
  }
  
  private def _updateVarModification( mod: ms_modification, ms_pep: ms_peptide, index: Int ): ms_modification = {
    
    val mascotMasses = this.mascotConfig.massesFile
    val modStr = ms_pep.getVarModsStr
    var isModTypeSet = false
    
    for ( k <- 0 until modStr.length ) {
      
      val modIdx = Integer.valueOf(modStr.substring(k,k+1))
      if (modIdx == index) {
        
        val title = mod.getTitle()
        
        if (k == 0) { //N-term modification
          
          if ( title matches "Protein" )
               mod.setModificationType(msparserConstants.MOD_TYPE_N_PROTEIN)
          else mod.setModificationType(msparserConstants.MOD_TYPE_N_TERM)
          
          isModTypeSet = true
          
        } else if (k == modStr.length()-1) { //C-term modification
            
            if ( title matches "Protein" )
                 mod.setModificationType(msparserConstants.MOD_TYPE_C_PROTEIN)
            else mod.setModificationType(msparserConstants.MOD_TYPE_C_TERM)
            
            isModTypeSet = true
        } else {
          mod.setModificationType(msparserConstants.MOD_TYPE_RESIDUE)
          isModTypeSet = true
          
          val r = ms_pep.getPeptideStr().charAt(k-1)
          val delta = mod.getDelta(msparserConstants.MASS_TYPE_MONO)
          val r_mono = delta + mascotMasses.getResidueMass(msparserConstants.MASS_TYPE_MONO, r)
          val r_avg = delta + mascotMasses.getResidueMass(msparserConstants.MASS_TYPE_AVE, r)
          mod.appendModifiedResidue(r, r_mono, r_avg)
        }
      }
    }
    
    if ( !isModTypeSet ) {
      if ( mod.getTitle matches "Protein" )
        mod.setModificationType(msparserConstants.MOD_TYPE_N_PROTEIN)
      else
        mod.setModificationType(msparserConstants.MOD_TYPE_C_PROTEIN)
    }
    
    mod
  }*/
  
}

/*
trait IPeptide {
  def getSequenceStr: String = null
}

object PeptideFragmentation {
  class Serie( seriesName: String ) {
    var isReverse: Boolean = false
    var values: Array[Object] = null
  }
}*/

/*
class PTM {
  def getIndex() = 0
}

class IPTM() {
  def getName(): String = null
}
class PTMHelper() {
  def getFixedPTM(str:String,a: Any): PTM = null
  def getVariablePTM(str:String,a: Any): PTM = null
  def getAllFixedPTMs(): Array[IPTM] = null
  def getAllVariablePTMs(): Array[IPTM] = null
}*/