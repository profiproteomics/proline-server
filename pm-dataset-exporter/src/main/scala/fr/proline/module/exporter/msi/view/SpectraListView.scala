package fr.proline.module.exporter.msi.view

import fr.proline.module.exporter.api.view.IFixedTableView
import fr.proline.module.exporter.api.view._
import fr.proline.core.om.model.msi.ProteinMatch
import fr.proline.core.om.model.msi.PeptideMatch
import fr.proline.core.om.model.msi.FragmentMatch
import fr.proline.core.om.model.msi.Peptide

import scala.collection.mutable.{ArrayBuffer, HashMap}
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.ms.massToMoz
import fr.proline.module.exporter.commons.config.ExportConfig
import fr.proline.module.exporter.commons.config.ExportConfigConstant

/**
 * @author VD225637
 */

object PeakViewFields extends IViewFieldEnumeration {
  val PREC_MASS = Field("Q1")
  val PREC_CHARGE = Field("prec_z")
  val FRAG_MASS = Field("Q3")
  val ISOTYPE = Field("isotype")
  val RT = Field("RT_detected")
  val UNIPROT_ID = Field("uniprot_id")
  val PROT_DESCRIPTION = Field("protein_name")
  val FRAG_INTENSITY = Field("relative_intensity")
  val PREC_INTENSITY = Field("prec_y")
  val PEP_SEQ = Field("stripped_sequence")
  val PEP_MODIFIED_SEQ = Field("modification_sequence")
  val FRAG_TYPE = Field("frg_type")
  val FRAG_LOSS_TYPE = Field("frg_loss_type")
  val FRAG_Z = Field("frg_z")
  val FRAG_NBR = Field("frg_nr")
  val SHARED = Field("shared")
  val CONFIDENCE = Field("confidence")
  val PROT_INDEX = Field("N")
  val RT_SOURCE = Field("RT_source")
}

object SpectronautFields extends IViewFieldEnumeration {
  val PREC_MASS = Field("Q1")
  val PREC_CHARGE = Field("prec_z")
  val FRAG_MASS = Field("Q3")
  val RT = Field("RT_detected")
  val PROT_NAME = Field("ProteinId")
  val FASTA_NAME = Field("fasta_name")
  val UNIPROT_ID = Field("uniprot_id")
  val Q1_ABUNDANCE = Field("Q1_abundance")
  val FRAG_INTENSITY = Field("relative_intensity")
  val PEP_SEQ = Field("stripped_sequence")
  val PEP_MODIFIED_SEQ = Field("modification_sequence")
  val FRAG_TYPE = Field("frg_type")
  val FRAG_LOSS_TYPE = Field("frg_loss_type")
  val FRAG_Z = Field("frg_z")
  val FRAG_NBR = Field("frg_nr")
  val SHARED = Field("shared")
  val IS_SPECIFIC= Field("is_specific")
  val RT_SOURCE = Field("RT_source")
}

case class MyBuildingContext(typicalProtMatch: ProteinMatch,
                             peptideMatch: PeptideMatch,
                             fragMatch: FragmentMatch,
                             peptide: Peptide,
                             proteinMatches: Array[ProteinMatch],
                             protIndex: Int) extends IRecordBuildingContext


abstract class AbstractSpectraListView(val identDS: IdentWithSpectrumDataSet, exportConfig: ExportConfig) extends IFixedTableView with LazyLogging {

  def generateDisambiguatedSeq(peptideMatch: PeptideMatch, peptide: Peptide): String = {
    val ambiguityStringOpt = peptideMatch.properties.flatMap(_.getMascotProperties).flatMap(_.ambiguityString)
    val seq = peptide.sequence.toCharArray
    if (ambiguityStringOpt.isDefined) {
      ambiguityStringOpt.get.split(',').sliding(3,3).foreach(tuple =>
        seq(tuple(0).toInt - 1) = tuple(2).charAt(0)
      )
      seq.mkString("")
    } else {
      peptide.sequence
    }
  }

  def generateSeqWithPtms(peptideMatch: PeptideMatch, peptide: Peptide) : String = {
    if (peptide.ptms.isEmpty)
      return generateDisambiguatedSeq(peptideMatch, peptide)

    val seq = generateDisambiguatedSeq(peptideMatch, peptide)
    val ptmByLoc : HashMap[Int, String] = new HashMap()
    val seqWithPtmBuilder = new StringBuilder()

    try {
      exportConfig.modificationFormat match {
        //          case ExportConfigConstant.MODIFICATION_FORMAT_FULLNAME => // not sure this case is really useful...
        //            peptide.ptms.foreach(ptm => {
        //              ptmByLoc += (ptm.seqPosition -> ptm.definition.names.shortName)
        //            })
        case ExportConfigConstant.MODIFICATION_FORMAT_ROUNDED_MONOMASS =>
          peptide.ptms.foreach(ptm => {
            ptmByLoc += (ptm.seqPosition -> (if(ptm.monoMass > 0) "+" else "").+(math.round(ptm.monoMass).toString))
          })
        case ExportConfigConstant.MODIFICATION_FORMAT_FIRST_THREE_LETTERS =>
          peptide.ptms.foreach(ptm => {
            ptmByLoc += (ptm.seqPosition -> ptm.toReadableString().substring(0, 3))
          })
      }
    } catch {
      /*
       *  A NullPointerException is raised when no exportConfig is given
       *  In this case, use a default behavior
       */
      case e: NullPointerException =>
        peptide.ptms.foreach(ptm => {
          ptmByLoc += (ptm.seqPosition -> ptm.toReadableString().substring(0, 3))
        })
    }

    val sortedKey = ptmByLoc.keySet.toSeq.sorted
    var lastIndex = 0
    sortedKey.foreach( location => {
      if(location >= 0){
        seqWithPtmBuilder.append(seq.substring(lastIndex, location))
        seqWithPtmBuilder.append("[").append(ptmByLoc(location)).append("]")
        lastIndex = location
      }
    })
    if(lastIndex<seq.size)
      seqWithPtmBuilder.append(seq.substring(lastIndex, seq.size))
    if(ptmByLoc.contains(-1)){ //Add Cterm modif
      seqWithPtmBuilder.append("[").append(ptmByLoc(-1)).append("]")
    }
    seqWithPtmBuilder.toString()
  }

}


class PeakViewSpectraListView(identDS: IdentWithSpectrumDataSet, exportConfig: ExportConfig) extends AbstractSpectraListView(identDS, exportConfig) {

  val rtByPepMatchId = new HashMap[Long,(Float,String)]
  val fields = PeakViewFields
  val isQuantiData = identDS.masterQuantPepIonByPepMatchId.isDefined
  val masterQuantPepIonByPepMatchId = identDS.masterQuantPepIonByPepMatchId
  val spectrumByPepMatchID = identDS.spectrumByPepMatchId
  var viewName = "spectrum_list"

  def buildRecord( buildingContext: IRecordBuildingContext ): Map[String,Any] = {

    val recordContext : MyBuildingContext = buildingContext.asInstanceOf[MyBuildingContext]
    val pepMatch = recordContext.peptideMatch
    val shared = recordContext.proteinMatches.size > 1
    val spectrum = spectrumByPepMatchID(pepMatch.id)
    val seq = generateDisambiguatedSeq(pepMatch, recordContext.peptide)
    val pepId = recordContext.peptide.id

    //Calculate precursor mass
    val precMoz = massToMoz(recordContext.peptide.calculatedMass, pepMatch.charge)

    //Get RT information
    var rt : Float = Float.NaN;
    var sourceRT = "";

    if (rtByPepMatchId.contains(pepMatch.id)) {
      rt = rtByPepMatchId(pepMatch.id)._1
      sourceRT = rtByPepMatchId(pepMatch.id)._2
    } else {
      if (isQuantiData && masterQuantPepIonByPepMatchId.get.contains(pepMatch.id)) {
        rt = masterQuantPepIonByPepMatchId.get(pepMatch.id).elutionTime / 60 // Export in minutes
        sourceRT = "Apex"
      } else {
        if (isQuantiData)
          logger.warn("*** *** Did not found MasterQuantPep for peptide : " + pepId)
        rt = spectrum.firstTime
        sourceRT = "MS/MS"
      }
      rtByPepMatchId += (pepId -> (rt, sourceRT))
    }

    // Build the full record
    Map(
      fields.PREC_MASS.toString -> precMoz,
      fields.FRAG_MASS.toString -> recordContext.fragMatch.calculatedMoz,
      fields.RT.toString -> rt,
      fields.PROT_DESCRIPTION.toString -> recordContext.typicalProtMatch.description,
      fields.ISOTYPE.toString -> "",
      fields.UNIPROT_ID.toString -> recordContext.typicalProtMatch.accession,
      fields.FRAG_INTENSITY.toString -> recordContext.fragMatch.intensity,
      fields.PREC_INTENSITY.toString -> spectrum.precursorIntensity,
      fields.PEP_SEQ.toString -> seq,
      fields.PEP_MODIFIED_SEQ.toString -> generateSeqWithPtms(pepMatch, recordContext.peptide),
      fields.PREC_CHARGE.toString -> recordContext.peptideMatch.charge,
      fields.FRAG_TYPE.toString -> recordContext.fragMatch.ionSeries,
      fields.FRAG_Z.toString -> recordContext.fragMatch.charge,
      fields.FRAG_NBR.toString -> recordContext.fragMatch.aaPosition,
      fields.SHARED.toString -> shared,
      fields.CONFIDENCE.toString() -> '1',
      fields.PROT_INDEX.toString() -> recordContext.protIndex,
      fields.RT_SOURCE.toString() -> sourceRT
    ).map(r => r._1.toString -> r._2)

  }

  def formatView(recordFormatter: Map[String,Any] => Unit ) {

    val rsm = identDS.resultSummary
    val rs = rsm.resultSet.get
    val pepMatchById = rs.getPeptideMatchById()
    val spectrumMatchesByPepMatchID = identDS.spectrumMatchesByPeptMatchId

    var protIndex = 0
    val sortedProtSet = rsm.proteinSets.sortBy(_.peptideSet.score).reverse

    val protMatches = for (protSet <- sortedProtSet; item <- protSet.peptideSet.items) yield (protSet.getRepresentativeProteinMatch().get, item.peptideInstance.peptide.id)
    val protMatchesByPeptideId = protMatches.groupBy(_._2).mapValues(_.map(_._1))

    protIndex += 1
    identDS.bestPeptideMatchesByPeptideAndCharge.foreach { case (key, pepMatch) =>
      val peptide = key._1
      val spectrumMatch = spectrumMatchesByPepMatchID(pepMatch.id)
      if (spectrumMatch != null && !spectrumMatch.fragMatches.isEmpty) {
        for (protMatch <- protMatchesByPeptideId(peptide.id)) {
          spectrumMatch.fragMatches.foreach(fragMatch => {
            this.formatRecord(MyBuildingContext(protMatch, pepMatch, fragMatch, peptide, protMatchesByPeptideId(peptide.id), protIndex), recordFormatter)
          })
        }
      } else {
        logger.debug("spectrum match for " + peptide.toString)
      }
    }

  }

}


class SpectronautSpectraListView(identDS: IdentWithSpectrumDataSet, exportConfig: ExportConfig) extends AbstractSpectraListView(identDS, exportConfig) {
  
    val rtByPepMatchId = new HashMap[Long,(Float,String)]
    val fields = SpectronautFields
    val isQuantiData = identDS.masterQuantPepIonByPepMatchId.isDefined
    val masterQuantPepIonByPepMatchId = identDS.masterQuantPepIonByPepMatchId
    val spectrumByPepMatchID = identDS.spectrumByPepMatchId
    var viewName = "spectrum_list"

    def buildRecord( buildingContext: IRecordBuildingContext ): Map[String,Any] = {

      val recordContext : MyBuildingContext = buildingContext.asInstanceOf[MyBuildingContext]
      val pepMatch = recordContext.peptideMatch
      val shared = recordContext.proteinMatches.size > 1
      val spectrum = spectrumByPepMatchID(pepMatch.id)
      val seq = generateDisambiguatedSeq(pepMatch, recordContext.peptide)

      val pepId = recordContext.peptide.id
      val (ionSerie, lossType) = if (recordContext.fragMatch.ionSeries.toString.size == 1) {
        (recordContext.fragMatch.ionSeries.toString, "noloss")
      } else {
        val items = recordContext.fragMatch.ionSeries.toString.split("-")
        (items(0), if (items.size > 1) { items(1) } else { "noloss" })
      }

      //Calculate precursor mass
      val precMoz = massToMoz(recordContext.peptide.calculatedMass, pepMatch.charge)

      //Get RT information
      var rt : Float = Float.NaN;
      var sourceRT = "";
      var abundance = Float.NaN

    if (rtByPepMatchId.contains(pepMatch.id)) {
      rt = rtByPepMatchId(pepMatch.id)._1
      sourceRT = rtByPepMatchId(pepMatch.id)._2
    } else {
      if (isQuantiData && masterQuantPepIonByPepMatchId.get.contains(pepMatch.id)) {
        val quantPeptideIon = masterQuantPepIonByPepMatchId.get(pepMatch.id)
        rt = quantPeptideIon.elutionTime / 60 // Export in minutes
        sourceRT = "Apex"
        abundance = quantPeptideIon.calcAbundanceSum()
      } else {
        if (isQuantiData)
          logger.warn("*** *** Did not found MasterQuantPep for peptide : " + pepId)
        rt = spectrum.firstTime
        sourceRT = "MS/MS"
      }
      rtByPepMatchId += (pepId -> (rt, sourceRT))
    }

      val uniprotAccPattern = "^..\\|(.*)".r
      val uniprotDescPattern = "^(.*?) ..=".r

      var descriptions = ArrayBuffer[String]()
      var accessions = ArrayBuffer[String]()

      recordContext.proteinMatches.foreach{ pm =>
        val idx = pm.description.lastIndexOf('|')
        if (idx != -1) {
          val head = pm.description.substring(0, idx)
          val tail = pm.description.substring(idx + 1)

          val accMatch = uniprotAccPattern.findFirstMatchIn(head)
          if (accMatch.isDefined) {
            accessions += accMatch.get.group(1)
          } else {
            accessions += head
          }

          val descMatch = uniprotDescPattern.findFirstMatchIn(tail)
          if (descMatch.isDefined) {
            descriptions += descMatch.get.group(1)
          } else {
            descriptions += tail
          }
        } else {
          descriptions += pm.description
        }
      }

       // Build the full record
      Map(
        fields.PREC_MASS.toString -> precMoz,
        fields.FRAG_MASS.toString -> recordContext.fragMatch.calculatedMoz,
        fields.RT.toString -> rt,
        fields.UNIPROT_ID.toString -> accessions.mkString("|"),
        fields.FASTA_NAME.toString -> descriptions.mkString("|"),
        fields.PROT_NAME.toString -> recordContext.proteinMatches.map(_.accession).mkString("|"),
        fields.FRAG_INTENSITY.toString -> recordContext.fragMatch.intensity,
        fields.Q1_ABUNDANCE.toString -> abundance,
        fields.PEP_SEQ.toString -> seq,
        fields.PEP_MODIFIED_SEQ.toString -> generateSeqWithPtms(pepMatch, recordContext.peptide),
        fields.PREC_CHARGE.toString -> recordContext.peptideMatch.charge,
        fields.FRAG_TYPE.toString -> ionSerie,
        fields.FRAG_LOSS_TYPE.toString -> lossType,
        fields.FRAG_Z.toString -> recordContext.fragMatch.charge,
        fields.FRAG_NBR.toString -> recordContext.fragMatch.aaPosition,
        fields.SHARED.toString -> shared,
        fields.IS_SPECIFIC.toString -> !shared,
        fields.RT_SOURCE.toString() -> sourceRT
      ).map(r => r._1.toString -> r._2)

    }

  def formatView(recordFormatter: Map[String,Any] => Unit ) {

    val rsm = identDS.resultSummary
    val rs = rsm.resultSet.get
    val pepMatchById = rs.getPeptideMatchById()
    val spectrumMatchesByPepMatchID = identDS.spectrumMatchesByPeptMatchId

    var protIndex = 0
    val sortedProtSet = rsm.proteinSets.sortBy(_.peptideSet.score).reverse

      val protMatches = for(protSet <- sortedProtSet; item <- protSet.peptideSet.items) yield (protSet.getRepresentativeProteinMatch().get, item.peptideInstance.peptide.id)
      val protMatchesByPeptideId =  protMatches.groupBy(_._2).mapValues(_.map(_._1))
            
      protIndex+=1
      identDS.bestPeptideMatchesByPeptideAndCharge.foreach { case(key, pepMatch) =>
        val peptide = key._1
        val spectrumMatch = spectrumMatchesByPepMatchID(pepMatch.id)
        if (spectrumMatch != null && !spectrumMatch.fragMatches.isEmpty) {
          val maxAbondance = spectrumMatch.fragMatches.map(_.intensity).max
          spectrumMatch.fragMatches.foreach(fragMatch => {
            //TODO : do we allowed this or do we need to pass maxAbundance to the record ?
            fragMatch.intensity = 100.0f*fragMatch.intensity/maxAbondance
            this.formatRecord(MyBuildingContext(protMatchesByPeptideId(peptide.id).head, pepMatch, fragMatch, peptide, protMatchesByPeptideId(peptide.id), protIndex), recordFormatter)
          })
        }
      }
  }

}