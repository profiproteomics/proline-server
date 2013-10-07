package fr.proline.module.parser.mascot

import java.io.File
import com.weiglewilczek.slf4s.Logging
import fr.proline.core.om.model.msi.IResultFile
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IResultFileProvider
import fr.proline.core.om.provider.msi.IResultFileVerifier
import fr.proline.core.om.model.msi.PtmDefinition
import scala.io.Source
import java.io.BufferedReader
import java.io.FileReader
import fr.proline.unimod.UnimodUnmarshaller
import java.io.ByteArrayInputStream
import fr.proline.core.om.model.msi.PtmSpecificity
import fr.proline.core.om.model.msi.PtmNames
import fr.proline.core.om.model.msi.UnimodEntry
import fr.proline.core.om.model.msi.PtmEvidence
import fr.proline.core.om.model.msi.IonTypes

class MascotResultFileProvider extends IResultFileProvider with IResultFileVerifier with Logging {

  final val mascotMultipartBoundary: String = "--gc0p4Jq0M2Yt08jU534c0p"
  val fileType: String = "MascotMSParser"

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

  def getPtmDefinitions(fileLocation: File): Seq[PtmDefinition] = {
    var ptmDefs = Seq.empty[PtmDefinition]
    try {
      val unimodText = extractUnimodSection(fileLocation)
      val is = new ByteArrayInputStream(unimodText.getBytes)
      val unimod = UnimodUnmarshaller.unmarshal(is);

      val modificationsElement = unimod.getModifications;
      if (modificationsElement != null) {

        val mods = modificationsElement.getMods();
        if ((mods != null) && !mods.isEmpty) {
          //		    var classifications = new HashMap<String, PtmClassification>();
          val it = mods.iterator
          while (it.hasNext) {
            val mod = it.next()
            val ptmNames = new PtmNames(shortName = mod.getTitle, fullName = mod.getFullName)
            var ptmEvidences = Seq.empty[PtmEvidence]
            
            val delta = mod.getDelta()
            if (delta != null) {

              ptmEvidences = ptmEvidences :+ new PtmEvidence(ionType = IonTypes.Precursor,
                    composition = delta.getComposition,
                    monoMass = delta.getMonoMass,
                    averageMass = delta.getAvgeMass,
                    isRequired = true);
            }
            
            // then build PtmDefinitions for each specificity
            val itSp = mod.getSpecificities().iterator()
            while (itSp.hasNext) {
              val specificity = itSp.next

              //build evidences
              var specificityEvidences = ptmEvidences
              
              val neutralLosses = specificity.getNeutralLosses;
              if ((neutralLosses != null) && !neutralLosses.isEmpty()) {
                val nlIt = neutralLosses.iterator()
                while (nlIt.hasNext) {
                  val neutralLoss = nlIt.next
                  specificityEvidences = ptmEvidences :+ new PtmEvidence(ionType = IonTypes.NeutralLoss,
                    composition = neutralLoss.getComposition,
                    monoMass = neutralLoss.getMonoMass,
                    averageMass = neutralLoss.getAvgeMass,
                    isRequired = !neutralLoss.isFlag());
                } // End loop for each neutralLoss
              } // End if (neutralLosses is not empty)

              val pepNeutralLosses = specificity.getPepNeutralLosses();
              if ((pepNeutralLosses != null) && !pepNeutralLosses.isEmpty()) {
                val pnlIt = pepNeutralLosses.iterator
                while (pnlIt.hasNext) {
                  val pepNeutralLoss = pnlIt.next
                  specificityEvidences = ptmEvidences :+ new PtmEvidence(ionType = IonTypes.PepNeutralLoss,
                    averageMass = pepNeutralLoss.getAvgeMass(),
                    composition = pepNeutralLoss.getComposition(),
                    isRequired = pepNeutralLoss.isRequired(),
                    monoMass = pepNeutralLoss.getMonoMass())
                }
              }

              // then build PtmDefinitions for each specificity
              ptmDefs = ptmDefs :+ (new PtmDefinition(
                id = -1,
                location = specificity.getPosition().value(),
                names = ptmNames,
                ptmEvidences = specificityEvidences.toArray,
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
                unimodId = mod.getRecordId.intValue))
            }

          } // End loop for each mod

        } // End if (mods is not empty)

      } // End if (modificationsElement is not null)

    } catch {
      case e: Exception => {}
    }
    ptmDefs
  }

  def isValid(fileLocation: File): Boolean = {
    // TODO : check kind of search (ex: MSMS search, error tolerant search, n15 search, PMF, ...)  
    true
  }

  def extractUnimodSection(f: File): String = {
    val bfr = new BufferedReader(new FileReader(f))
    var line: String = ""
    var section: String = ""
    val maxCount = 4
    var count = 0
    line = bfr.readLine()
    while ((count < maxCount) && (line != null)) {
      if (line == mascotMultipartBoundary) {
        count += 1
        if (count == (maxCount -1))
          bfr.readLine() //skip first line following the boundary mark
      } else {
        if (count == (maxCount - 1)) section += line
      }
      line = bfr.readLine()
    }
    bfr.close
    section
  }
}
