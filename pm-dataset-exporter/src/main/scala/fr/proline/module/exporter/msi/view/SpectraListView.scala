package fr.proline.module.exporter.msi.view

import fr.proline.module.exporter.api.view.IFixedDatasetView
import fr.proline.module.exporter.dataset.view.IdentDataSet
import fr.proline.module.exporter.api.view._
import fr.proline.core.om.model.msi.MSISearch
import fr.proline.core.om.model.msi.ProteinSet
import fr.proline.core.om.model.msi.ProteinMatch
import fr.proline.core.om.model.msi.PeptideMatch
import fr.proline.core.om.model.msi.FragmentMatch
import fr.proline.core.om.model.msi.Peptide
import scala.collection.mutable.HashMap
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.ms.massToMoz


/**
 * @author VD225637
 */
object SpectraListViewFields extends IViewFieldEnumeration {
  val PREC_MASS = Field("Q1")
  val PREC_CHARGE = Field("prec_z")
  val FRAG_MASS = Field("Q3")
  val ISOTYPE = Field("isotype")
  val RT = Field("RT_detected")
  val PROT_NAME = Field("uniprot_id")
  val PROT_DESCRIPTION = Field("protein_name")
  val FRAG_INTENSITY = Field("relative_intensity")
  val PREC_INTENSITY = Field("prec_y")
  val PEP_SEQ = Field("stripped_sequence")
  val PEP_MODIFIED_SEQ = Field("modification_sequence")
  val FRAG_TYPE = Field("frg_type")
  val FRAG_Z = Field("frg_z")
  val FRAG_NBR = Field("frg_nr")
  val SHARED = Field("shared")
  val CONFIDENCE = Field("confidence")
  val PROT_INDEX = Field("N")
  val RT_SOURCE = Field("RT Source")
    
}

class SpectraListView( val identDS: IdentWithSpectrumDataSet ) extends IFixedDatasetView with LazyLogging {
  
    val rtByPepId = new HashMap[Long,(Float,String)]
    var viewName = "spectrum_list"
    val fields = SpectraListViewFields 
    val isQuantiData = identDS.masterQuantPepByPepId.isDefined
    val masterQuantPepByPepId = identDS.masterQuantPepByPepId
    val spectrumByPepMatchID = identDS.spectrumByPepMatchId
    
    case class MyBuildingContext( proteinset : ProteinSet, typicalProtMatch : ProteinMatch , peptideMatch : PeptideMatch, fragMatch : FragmentMatch , peptide : Peptide, protIndex: Int) extends IRecordBuildingContext
    
    def buildRecord( buildingContext: IRecordBuildingContext ): Map[String,Any] = {
      
      val recordContext : MyBuildingContext = buildingContext.asInstanceOf[MyBuildingContext]      
      val protSet = recordContext.proteinset
      val pepMatch = recordContext.peptideMatch
      val spectrum = spectrumByPepMatchID(pepMatch.id)
      val shared = identDS.sharedPepMatchIds.contains(pepMatch.id)
      val seq = recordContext.peptide.sequence     
      val pepId = recordContext.peptide.id
      
      //Calculate precursor mass
      val precMoz = massToMoz(recordContext.peptide.calculatedMass, pepMatch.charge)

      //Get RT information
      var rt : Float = Float.NaN;
      var sourceRT = "";

    if (rtByPepId.contains(pepId)) {
      rt = rtByPepId(pepId)._1
      sourceRT = rtByPepId(pepId)._2
    } else {
      if (isQuantiData && masterQuantPepByPepId.get.contains(pepId)) {
        rt = masterQuantPepByPepId.get(pepId).getBestQuantPeptide.elutionTime / 60 // Export in minutes
        sourceRT = "Apex elution time"
      } else {
        if (isQuantiData)
          logger.warn("*** *** Did not found MasterQuantPep for peptide : " + pepId)
        rt = spectrum.firstTime
        sourceRT = "MS/MS elution time"
      }
      rtByPepId += (pepId -> (rt, sourceRT))
    }
      
       // Build the full record
      Map(
        fields.PREC_MASS.toString -> precMoz,
        fields.FRAG_MASS.toString -> recordContext.fragMatch.moz,
        fields.RT.toString -> rt,
        fields.PROT_DESCRIPTION.toString -> recordContext.typicalProtMatch.description,
        fields.ISOTYPE.toString -> "",
        fields.PROT_NAME.toString -> recordContext.typicalProtMatch.accession,
        fields.FRAG_INTENSITY.toString -> recordContext.fragMatch.intensity,
        fields.PREC_INTENSITY.toString -> spectrum.precursorIntensity,
        fields.PEP_SEQ.toString -> seq,
        fields.PEP_MODIFIED_SEQ.toString -> genertateSeqWithPtms(recordContext.peptide),       
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
          
    private def genertateSeqWithPtms(peptide: Peptide) : String = {
      if (peptide.ptms.isEmpty)
        return peptide.sequence
        
      val seq = peptide.sequence
      val readablePTM = peptide.readablePtmString
      val ptms = readablePTM.trim.split(";")
      val ptmByLoc : HashMap[Int, String] = new HashMap()
      val seqWithPtmBuilder = new StringBuilder()
      ptms.foreach( ptm => {
            if(!ptm.isEmpty()) {
              val locIndex = ptm.indexOf("(")+1
              val locEndIndex = ptm.indexOf(")")
              val ptmString = ptm.substring(0,locIndex-1).trim()
              val locStr = ptm.substring(locIndex,locEndIndex)
  
              var location = -2
              if( locStr matches """.+N-term""" ) {
                location = 0
              } else if( locStr matches """.+C-term"""  ){ 
                location = -1
              } else {
                try {
                  location = locStr.substring(1, locStr.size).toInt // skip AA
                }catch {
                  case e: NumberFormatException => location = -2
                }
              }
              ptmByLoc += (location->ptmString.substring(0,3))
            }
      })
      
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
    
        
    def onEachRecord( recordFormatter: Map[String,Any] => Unit ) {
    
    val rsm = identDS.resultSummary
    val rs = rsm.resultSet.get
    val protMatchById = rs.getProteinMatchById
    val pepMatchById = rs.getPeptideMatchById()
    val spectrumMatchesByPepMatchID = identDS.spectrumMatchesByPeptMatchId
    
    var protIndex = 0
    val sortedProtSet =rsm.proteinSets.sortBy(_.peptideSet.score).reverse  
    for( protSet <- sortedProtSet) {
      // Note that we export only protein sets which are loaded with the RSM
      // The result will depend of provider which have been used
     // Typical Protein Match is put first
      var typicalProtMatch = if(protSet.getRepresentativeProteinMatch() != null && protSet.getRepresentativeProteinMatch().isDefined) protSet.getRepresentativeProteinMatch().get
        else {
          val typicalProtMatchId =protSet.getRepresentativeProteinMatchId()  
          if( typicalProtMatchId != 0 ) { 
            protMatchById(typicalProtMatchId)
          } else {
            protMatchById( protSet.getSameSetProteinMatchIds.head )
          }        
      }
            
      protIndex+=1
      protSet.peptideSet.items.foreach(pepSetItem => {
        val peptide = pepSetItem.peptideInstance.peptide
        val pepMatchId = pepSetItem.peptideInstance.bestPeptideMatchId
        //Export only one PSM (best one)           
        val pepMatch = pepMatchById(pepMatchId)
        val spectrumMatch = spectrumMatchesByPepMatchID(pepMatchId)
        if (spectrumMatch != null && !spectrumMatch.fragMatches.isEmpty)
          spectrumMatch.fragMatches.foreach(fragMatch => {
            if(fragMatch.ionSeries.size == 1) //Only export y, b ... ion series, not b0, b* etc
              this.formatRecord(MyBuildingContext(protSet, typicalProtMatch, pepMatch, fragMatch, peptide, protIndex), recordFormatter)
          })
      })
      
    }
  }
}