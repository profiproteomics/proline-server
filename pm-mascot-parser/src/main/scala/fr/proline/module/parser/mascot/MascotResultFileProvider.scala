package fr.proline.module.parser.mascot

import java.io.BufferedReader
import java.io.ByteArrayInputStream
import java.io.File
import java.io.FileReader
import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.core.om.model.msi._
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IResultFileProvider
import fr.proline.core.om.provider.msi.IResultFileVerifier
import fr.proline.unimod.UnimodUnmarshaller
import fr.profi.util.StringUtils
import scala.collection.mutable.ArrayBuffer
import java.io.InputStreamReader
import java.io.FileInputStream

import fr.proline.module.parser.mascot.MascotDataParser.LATIN_1_CHARSET

object MascotResultFileProviderType {
  final val fileType: String = "mascot.dat"
}

class MascotResultFileProvider extends IResultFileProvider with IResultFileVerifier with Logging {

  final val mascotMultipartBoundary: String = "--gc0p4Jq0M2Yt08jU534c0p"
  val fileType: String = MascotResultFileProviderType.fileType

  def getResultFile(fileLocation: File, importProperties: Map[String, Any], parserContext: ProviderDecoratedExecutionContext): IResultFile = {
    new MascotResultFile(fileLocation, importProperties, parserContext)
  }

  val resultFileProperties: Map[String, Class[_]] = {
    val propertiedBuilder = Map.newBuilder[String, Class[_]]
    propertiedBuilder += (MascotParseParams.ION_SCORE_CUTOFF.toString -> Double.getClass())
    propertiedBuilder += (MascotParseParams.SUBSET_THRESHOLD.toString -> Double.getClass())
    propertiedBuilder += (MascotParseParams.PROTEIN_CUTOFF_PVALUE.toString -> Double.getClass())
    propertiedBuilder += (MascotParseParams.MASCOT_SERVER_URL.toString -> classOf[String])
    propertiedBuilder.result
  }

  def getResultFileVerifier: IResultFileVerifier = {
    this
  }

  def getPtmDefinitions(fileLocation: File, importProperties: Map[String, Any]): Seq[PtmDefinition] = {

    logger.info("Search PTMs definition for" +fileLocation.getAbsolutePath())
    val ptmDefs = Array.newBuilder[PtmDefinition]

    //try { // oté en attendant de gérer tout les cas de figure non précédemment traités, ce role est renvoyé à la classe appelante.
    val unimodText = extractUnimodSection(fileLocation)

    /* Unimod xml part expected UTF-8 stream : <?xml version="1.0" encoding="UTF-8" ?> */
    val is = new ByteArrayInputStream(unimodText.getBytes("UTF-8"))
    val unimod = UnimodUnmarshaller.unmarshal(is)
    // No need to close a ByteArrayInputStream

    val modificationsElement = unimod.getModifications
    if (modificationsElement != null) {

      val mods = modificationsElement.getMods()

      if ((mods != null) && !mods.isEmpty) {
        //		    var classifications = new HashMap<String, PtmClassification>();
        val it = mods.iterator

        while (it.hasNext) {

          val mod = it.next()
          val ptmNames = new PtmNames(shortName = mod.getTitle, fullName = mod.getFullName)
          val ptmEvidences = Array.newBuilder[PtmEvidence]

          val delta = mod.getDelta()
          if (delta != null) {
            ptmEvidences += new PtmEvidence(
              ionType = IonTypes.Precursor,
              composition = delta.getComposition,
              monoMass = delta.getMonoMass,
              averageMass = delta.getAvgeMass,
              isRequired = true
            )
          }

          // then build PtmDefinitions for each specificity
          val itSp = mod.getSpecificities().iterator()
          while (itSp.hasNext) {
            val specificity = itSp.next

            //build evidences
            val specificityEvidences = Array.newBuilder[PtmEvidence]

            // FIXME: do we need to copy PTM evidence in evidences specific to the specificity ???
            specificityEvidences ++= ptmEvidences.result

            val neutralLosses = specificity.getNeutralLosses
            if ((neutralLosses != null) && !neutralLosses.isEmpty()) {
              val nlIt = neutralLosses.iterator()
              while (nlIt.hasNext) {
                val neutralLoss = nlIt.next()
                val nlComp = neutralLoss.getComposition()

                // FIXME: workaround for NL without composition => should we import them ???
                if (StringUtils.isNotEmpty(nlComp) && neutralLoss.getMonoMass != null) {
                  specificityEvidences += new PtmEvidence(
                    ionType = IonTypes.NeutralLoss,
                    composition = neutralLoss.getComposition,
                    monoMass = neutralLoss.getMonoMass,
                    averageMass = neutralLoss.getAvgeMass,
                    isRequired = !neutralLoss.isFlag()
                  )
                }
              } // End loop for each neutralLoss
            } // End if (neutralLosses is not empty)

            val pepNeutralLosses = specificity.getPepNeutralLosses()
            if ((pepNeutralLosses != null) && !pepNeutralLosses.isEmpty()) {
              val pnlIt = pepNeutralLosses.iterator
              while (pnlIt.hasNext) {
                val pepNeutralLoss = pnlIt.next
                specificityEvidences += new PtmEvidence(
                  ionType = IonTypes.PepNeutralLoss,
                  averageMass = pepNeutralLoss.getAvgeMass(),
                  composition = pepNeutralLoss.getComposition(),
                  isRequired = pepNeutralLoss.isRequired(),
                  monoMass = pepNeutralLoss.getMonoMass()
                )
              }
            }

            // then build PtmDefinitions for each specificity
            ptmDefs += new PtmDefinition(
              id = -1,
              location = specificity.getPosition().value(),
              names = ptmNames,
              ptmEvidences = specificityEvidences.result,
              residue = {
                if (specificity.getSite != null) {
                  val normSite = specificity.getSite().trim
                  if (normSite.length() == 1)
                    normSite.charAt(0)
                  else
                    '\0'
                } else {
                  '\0'
                }
              },
              classification = specificity.getClassification,
              unimodId = if (mod.getRecordId != null) mod.getRecordId.intValue else 0
            )
          }
           logger.debug("Create PTM "+ptmNames)

        } // End loop for each mod

      } // End if (mods is not empty)

    } // End if (modificationsElement is not null)

    //} catch {
    //  case e: Exception => {}
    //}

    ptmDefs.result
  }

  def isValid(fileLocation: File, importProperties: Map[String, Any]): Boolean = {
    // TODO : check kind of search (ex: MSMS search, error tolerant search, n15 search, PMF, ...)  
    true
  }

  def extractUnimodSection(f: File): String = { 
    require((f != null) && f.isFile, "Invalid file")

    val absolutePathname = f.getAbsolutePath

    var section: String = ""

    // Force ANSI ISO-8859-1 to read Mascot .dat files
    val bfr = new BufferedReader(new InputStreamReader(new FileInputStream(f), LATIN_1_CHARSET))

    try {
      var line: String = ""
      val maxCount = 4
      var count = 0
      line = bfr.readLine()
      while ((count < maxCount) && (line != null)) {
        if (line == mascotMultipartBoundary) {
          count += 1
          if (count == (maxCount - 1))
            bfr.readLine() //skip first line following the boundary mark
        } else {
          if (count == (maxCount - 1)) section += line
        }
        line = bfr.readLine()
      }

    } finally {

      try {
        bfr.close()
      } catch {
        case exClose: Exception => logger.error("Error closing [" + absolutePathname + ']', exClose)
      }

    }

    section
  }

  // TODO ABU
  def getEnzyme(fileLocation: File, importProperties: Map[String, Any]): Array[Enzyme] = {

    val mascotEnzymes = MascotEnzymeParser.getEnzymeDefinitions(fileLocation)
    val enzymes = new ArrayBuffer[Enzyme]
    mascotEnzymes.foreach(e => {
      val enzymeCleavages = new ArrayBuffer[fr.proline.core.om.model.msi.EnzymeCleavage]
      e.cleavages.foreach(ec => {
        enzymeCleavages += new fr.proline.core.om.model.msi.EnzymeCleavage(id = -1, residues = ec.residues, restrictiveResidues = ec.restrict, site = if (ec.isNterm) "N-term" else "C-term")
      })
      enzymes += new Enzyme(id = -1,
        name = e.name,
        enzymeCleavages = enzymeCleavages.toArray,
        cleavageRegexp = None,
        isIndependant = e.independent,
        isSemiSpecific = e.semiSpecific,
        properties = None)
    })
    enzymes.toArray
  }
}
