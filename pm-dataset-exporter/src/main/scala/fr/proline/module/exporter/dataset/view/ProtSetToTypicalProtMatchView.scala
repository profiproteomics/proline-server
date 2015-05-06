package fr.proline.module.exporter.dataset.view


import fr.proline.core.om.model.msi._
import fr.proline.module.exporter.api.view.IFixedDatasetView
import java.text.SimpleDateFormat
import fr.proline.module.exporter.commons.config._
import fr.proline.module.exporter.commons.config.view.SheetViewFieldsConfig
import fr.proline.module.exporter.api.view.IRecordBuildingContext
import java.text.DecimalFormat
import scala.collection.immutable.ListMap
import fr.proline.core.om.model.msq.MasterQuantProteinSet
import scala.collection.mutable.ArrayBuffer
import com.typesafe.scalalogging.slf4j.Logging
import java.util.NoSuchElementException


abstract class AbstractProtSetToTypicalProtMatchView  extends IFixedDatasetView with Logging{
  val identDS: IdentDataSet
  val sheetConfig : ExportConfigSheet
  val dateFormat : SimpleDateFormat
  val decimalFormat: DecimalFormat
  val exportAllProteinSet: Boolean
  
  val isQuanti: Boolean = identDS.isInstanceOf[QuantiDataSet]
  val quantiDS: QuantiDataSet = if (isQuanti) identDS.asInstanceOf[QuantiDataSet] else null
   
   var listFields: ArrayBuffer[String] = new ArrayBuffer()
   for ( f <- sheetConfig.fields ) {
     f.id match{
       case ExportConfigConstant.FIELD_PROTEIN_SETS_QUANTI_STATUS => {
          if (isQuanti) {
            quantiDS.qcIds.foreach(qcId => {
          	listFields += f.title+"_"+quantiDS.nameByQchId(qcId)
          })
          }
        }
       case ExportConfigConstant.FIELD_PROTEIN_SETS_QUANTI_PEPTIDE_NUMBER => {
          if (isQuanti) {
            quantiDS.qcIds.foreach(qcId => {
          	listFields += f.title+"_"+quantiDS.nameByQchId(qcId)
          })
          }
        }
        case ExportConfigConstant.FIELD_PROTEIN_SETS_QUANTI_ABUNDANCE => {
          if (isQuanti) {
            quantiDS.qcIds.foreach(qcId => {
          	listFields += f.title+"_"+quantiDS.nameByQchId(qcId)
          })
          }
        }
        case ExportConfigConstant.FIELD_PROTEIN_SETS_QUANTI_RAW_ABUNDANCE => {
          if (isQuanti) {
          quantiDS.qcIds.foreach(qcId => {
          	listFields += f.title+"_"+quantiDS.nameByQchId(qcId)
          })
          }
        }
        case ExportConfigConstant.FIELD_PROTEIN_SETS_QUANTI_PSM_COUNT => {
          if (isQuanti) {
          quantiDS.qcIds.foreach(qcId => {
          	listFields += f.title+"_"+quantiDS.nameByQchId(qcId)
          })
          }
        }
        case other => {
    	 listFields += f.title
        }
     }
   }
  val fields = new SheetViewFieldsConfig(listFields.toArray)
    
    def buildRecord( buildingContext: IRecordBuildingContext ): Map[String,Any] = {
    
    var dcf1 : DecimalFormat = new DecimalFormat("0.#")
    dcf1.setDecimalFormatSymbols(decimalFormat.getDecimalFormatSymbols())
    var dcf2 : DecimalFormat = new DecimalFormat("0.##")
    dcf2.setDecimalFormatSymbols(decimalFormat.getDecimalFormatSymbols())
    var dcf4 : DecimalFormat = new DecimalFormat("0.####")
    dcf4.setDecimalFormatSymbols(decimalFormat.getDecimalFormatSymbols())
    
    val isProtSetCtx: Boolean =  (buildingContext.isInstanceOf[ProtMatchBuildingContext]) 
    val isPepSetCtx: Boolean = (buildingContext.isInstanceOf[PepMatchBuildingContext])
    val isProtSetQuantiCtx: Boolean = (buildingContext.isInstanceOf[ProtMatchQuantiBuildingContext]) 
    val isProtSetCtxDefinedForPep: Boolean = (isPepSetCtx && buildingContext.asInstanceOf[PepMatchBuildingContext].protMatchBuildingCtx.isDefined)
    
    
    val protSetBuildingCtxOpt: ProtMatchBuildingContext = if (isProtSetCtx) buildingContext.asInstanceOf[ProtMatchBuildingContext] else  (if (isProtSetCtxDefinedForPep)buildingContext.asInstanceOf[PepMatchBuildingContext].protMatchBuildingCtx.get else null)
    val allPepMatchesBuildingCtx: PepMatchBuildingContext = if (isProtSetCtx) null else buildingContext.asInstanceOf[PepMatchBuildingContext]
    val protSetQuantiBuildingCtx: ProtMatchQuantiBuildingContext = if(isProtSetQuantiCtx) buildingContext.asInstanceOf[ProtMatchQuantiBuildingContext] else null
    
    var protSet :ProteinSet = null
    var protMatch: ProteinMatch = null
    var peptideSet: PeptideSet = null
     if (protSetBuildingCtxOpt != null) {
        protSet = protSetBuildingCtxOpt.protSet
        protMatch = protSetBuildingCtxOpt.protMatch
        peptideSet = protSetBuildingCtxOpt.peptideSet
     }
    
    var protSetId = -1l
    var protSetScore = -1d
    var protSetValid = "false"
    if (protSetBuildingCtxOpt != null) {
      protSetId = protSetBuildingCtxOpt.protSet.id
      protSetScore = dcf1.format(protSetBuildingCtxOpt.protSet.peptideSet.score).toDouble
      protSetValid = protSetBuildingCtxOpt.protSet.isValidated.toString
    }
    
    var pepMatch : PeptideMatch = null
    var seqMatch: SequenceMatch = null
    if (allPepMatchesBuildingCtx != null){
      pepMatch = allPepMatchesBuildingCtx.pepMatch
      seqMatch=  allPepMatchesBuildingCtx.seqMatch;
    }

    val peptide = if (pepMatch == null) null else pepMatch.peptide
    val initialQueryId = if (pepMatch == null) null else Option(pepMatch.msQuery).map(_.initialId).getOrElse(null)
    val experimentalMoz = if (pepMatch == null) null else decimalFormat.format(Option(pepMatch.msQuery).map(_.moz).getOrElse(null)).toDouble

    val resBefore = if (pepMatch == null) null else if (seqMatch.residueBefore == '\0') '-' else seqMatch.residueBefore
    val resAfter = if (pepMatch == null) null else if (seqMatch.residueAfter == '\0') '-' else seqMatch.residueAfter

    val dbProtMatchesCount = {
      if (identDS.allProtMatchSetByPepId == null || pepMatch == null) null
      else
      if (identDS.allProtMatchSetByPepId.get(pepMatch.peptideId).isDefined) {
        identDS.allProtMatchSetByPepId.get(pepMatch.peptideId).get.size
      } else
        0
    }
   
    
    var exportMap:ListMap[String,Any] = ListMap()
    val listFields :Array[ExportConfigField] = sheetConfig.fields
    for ( f <- listFields ) {
      f.id match{
        case ExportConfigConstant.FIELD_PROTEIN_SETS_ID => {
          	exportMap += ( fields.addField(f.title) -> protSet.id)
        }
        case ExportConfigConstant.FIELD_PROTEIN_SETS_ACCESSION => {
          	exportMap += ( fields.addField(f.title) -> protMatch.accession)
        }
        case ExportConfigConstant.FIELD_PROTEIN_SETS_DESCRIPTION => {
          	exportMap += ( fields.addField(f.title) -> protMatch.description)
        }
        case ExportConfigConstant.FIELD_PROTEIN_SETS_SCORE => {
          	exportMap += ( fields.addField(f.title) -> dcf1.format(protSet.peptideSet.score).toDouble)
        }
        case ExportConfigConstant.FIELD_PROTEIN_SETS_IS_VALIDATED => {
          	exportMap += ( fields.addField(f.title) -> protSet.isValidated.toString)
        }
        case ExportConfigConstant.FIELD_PROTEIN_SETS_SELECTION_LEVEL => {
          	exportMap += ( fields.addField(f.title) -> protSet.selectionLevel)
        }
        case ExportConfigConstant.FIELD_PROTEIN_SETS_NB_SAMESET_PROTEIN_MATCHES => {
          	exportMap += ( fields.addField(f.title) -> protSet.getSameSetProteinMatchIds.length)
        }
        case ExportConfigConstant.FIELD_PROTEIN_SETS_NB_SUBSET_PROTEIN_MATCHES => {
          	exportMap += ( fields.addField(f.title) -> protSet.getSubSetProteinMatchIds.length)
        }
        case ExportConfigConstant.FIELD_PROTEIN_SETS_COVERAGE => {
          	exportMap += ( fields.addField(f.title) -> dcf1.format(protMatch.coverage).toDouble)
        }
        case ExportConfigConstant.FIELD_PROTEIN_SETS_MW => {
          	exportMap += ( fields.addField(f.title) -> Option(protMatch.protein).flatMap( _.map( _.mass ) ).getOrElse(0.0))
        }
        case ExportConfigConstant.FIELD_PROTEIN_SETS_NB_SEQUENCES => {
          	exportMap += ( fields.addField(f.title) -> protSetBuildingCtxOpt.allSeqs.distinct.length)
        }
        case ExportConfigConstant.FIELD_PROTEIN_SETS_NB_SPECIFIC_SEQUENCES => {
          	exportMap += ( fields.addField(f.title) -> protSetBuildingCtxOpt.specificSeqs.distinct.length)
        }
        case ExportConfigConstant.FIELD_PROTEIN_SETS_NB_PEPTIDES => {
          	exportMap += ( fields.addField(f.title) -> protSetBuildingCtxOpt.peptideCount)
        }
        case ExportConfigConstant.FIELD_PROTEIN_SETS_NB_SPECIFIC_PEPTIDES => {
          	exportMap += ( fields.addField(f.title) -> protSetBuildingCtxOpt.specificPeps.length)
        }
        case ExportConfigConstant.FIELD_PROTEIN_SETS_NB_PEPTIDE_MATCHES => {
          	exportMap += ( fields.addField(f.title) -> protSet.peptideSet.peptideMatchesCount)
        }
        case ExportConfigConstant.FIELD_PROTEIN_SETS_NB_SPECIFIC_PEPTIDE_MATCHES => {
          	exportMap += ( fields.addField(f.title) -> protSetBuildingCtxOpt.specificPepMatchIds.length)
        }
        case ExportConfigConstant.FIELD_PROTEIN_MATCH_IS_TYPICAL_PROTEIN => {
          	exportMap += ( fields.addField(f.title) -> (protSet.getTypicalProteinMatchId == protMatch.id))
        }
        case ExportConfigConstant.FIELD_PROTEIN_MATCH_IS_SAMESET => {
          	exportMap += ( fields.addField(f.title) -> !peptideSet.isSubset)
        }
        case ExportConfigConstant.FIELD_PROTEIN_MATCH_PEPTIDE_SET_SCORE => {
          	exportMap += ( fields.addField(f.title) -> dcf1.format(peptideSet.score).toDouble)
        }
        case ExportConfigConstant.FIELD_PSM_PEPTIDE_ID => {
          	exportMap += ( fields.addField(f.title) -> peptide.id )
        }
        case ExportConfigConstant.FIELD_PSM_SEQUENCE => {
          	exportMap += ( fields.addField(f.title) -> peptide.sequence)
        }
        case ExportConfigConstant.FIELD_PSM_MODIFICATIONS => {
          	exportMap += ( fields.addField(f.title) -> peptide.readablePtmString)
        }
        case ExportConfigConstant.FIELD_PSM_SCORE => {
          	exportMap += ( fields.addField(f.title) -> dcf2.format(pepMatch.score).toDouble)
        }
        case ExportConfigConstant.FIELD_PSM_CALCULATED_MASS => {
          	exportMap += ( fields.addField(f.title) -> dcf4.format(peptide.calculatedMass).toDouble)
        }
        case ExportConfigConstant.FIELD_PSM_CHARGE => {
          	exportMap += ( fields.addField(f.title) -> Option(pepMatch.msQuery).map(_.charge).getOrElse(null))
        }
        case ExportConfigConstant.FIELD_PSM_EXPERIMENTAL_MOZ => {
          	exportMap += ( fields.addField(f.title) -> experimentalMoz )
        }
        case ExportConfigConstant.FIELD_PSM_DELTA_MOZ => {
          	exportMap += ( fields.addField(f.title) -> decimalFormat.format(pepMatch.deltaMoz).toDouble)
        }
        case ExportConfigConstant.FIELD_PSM_RT => {
          	exportMap += ( fields.addField(f.title) -> "-")
        }
        case ExportConfigConstant.FIELD_PSM_PEPTIDE_LENGTH => {
          	exportMap += ( fields.addField(f.title) -> peptide.sequence.length)
        }
        case ExportConfigConstant.FIELD_PSM_INITIAL_QUERY_ID => {
          	exportMap += ( fields.addField(f.title) -> initialQueryId)
        }
        case ExportConfigConstant.FIELD_PSM_MISSED_CLEAVAGES => {
          	exportMap += ( fields.addField(f.title) -> pepMatch.missedCleavage )
        }
        case ExportConfigConstant.FIELD_PSM_RANK => {
          	exportMap += ( fields.addField(f.title) -> pepMatch.rank)
        }
        case ExportConfigConstant.FIELD_PSM_CD_PRETTY_RANK => {
          	exportMap += ( fields.addField(f.title) -> pepMatch.cdPrettyRank)
        }
        case ExportConfigConstant.FIELD_PSM_FRAGMENT_MATCHES_COUNT => {
          	exportMap += ( fields.addField(f.title) -> pepMatch.fragmentMatchesCount)
        }
        case ExportConfigConstant.FIELD_PSM_SPECTRUM_TITLE => {
          	exportMap += ( fields.addField(f.title) -> Option(pepMatch.getMs2Query).map(_.spectrumTitle).getOrElse(""))
        }
        case ExportConfigConstant.FIELD_PSM_NB_PROTEIN_SETS => {
          	exportMap += ( fields.addField(f.title) -> identDS.validProtSetIdSetByPepMatchId.get(pepMatch.id).map(_.size).getOrElse(0) )
        }
        case ExportConfigConstant.FIELD_PSM_NB_PROTEIN_MATCHES => {
          	exportMap += ( fields.addField(f.title) -> identDS.validProtMatchIdSetByPepMatchId.get(pepMatch.id).map(_.size).getOrElse(0))
        }
        case ExportConfigConstant.FIELD_PSM_NB_DATABANK_PROTEIN_MATCHES => {
          	exportMap += ( fields.addField(f.title) -> dbProtMatchesCount)
        }
        case ExportConfigConstant.FIELD_PSM_START => {
          	exportMap += ( fields.addField(f.title) -> seqMatch.start)
        }
        case ExportConfigConstant.FIELD_PSM_END => {
          	exportMap += ( fields.addField(f.title) -> seqMatch.end)
        }
        case ExportConfigConstant.FIELD_PSM_RESIDUE_BEFORE => {
          	exportMap += ( fields.addField(f.title) -> resBefore)
        }
        case ExportConfigConstant.FIELD_PSM_RESIDUE_AFTER => {
          	exportMap += ( fields.addField(f.title) -> resAfter)
        }
        case ExportConfigConstant.FIELD_PROTEIN_SETS_QUANTI_STATUS => {
          	protSetQuantiBuildingCtx.qcIds.foreach(qcId => {
          	  var pmId: Long = -1
          	  //Get QuantProteinSet for current ProteinSet in current QuantChannel
          		val protQuant = protSetQuantiBuildingCtx.masterQuantProteinSet.quantProteinSetMap.get(qcId)
          	    if (protQuant.isDefined) {
          	    	if (protQuant.get.proteinSetId.isDefined) {
          	    	  pmId = protQuant.get.proteinMatchId.getOrElse(-1)
          	    	}
          	  }
          	  var qcStatus :String= if (quantiDS.protMatchStatusByIdPepMatchByQCId.contains(qcId)){
          	    val protMatchStatusById: Map[Long, String] = quantiDS.protMatchStatusByIdPepMatchByQCId.get(qcId).get
          	    if (protMatchStatusById.contains(pmId)){
          	      protMatchStatusById.get(pmId).get
          	    }else{""}
          	  }else{
          	    ""
          	  }
            exportMap += (fields.addField(f.title+"_"+protSetQuantiBuildingCtx.nameByQchId(qcId)) -> qcStatus)
          })
        }
        case ExportConfigConstant.FIELD_PROTEIN_SETS_QUANTI_PEPTIDE_NUMBER => {
          	protSetQuantiBuildingCtx.qcIds.foreach(qcId => {
          	var pmId: Long = -1
          	  //Get QuantProteinSet for current ProteinSet in current QuantChannel
          		val protQuant = protSetQuantiBuildingCtx.masterQuantProteinSet.quantProteinSetMap.get(qcId)
          	    if (protQuant.isDefined) {
          	    	if (protQuant.get.proteinSetId.isDefined) {
          	    	  pmId = protQuant.get.proteinMatchId.getOrElse(-1)
          	    	}
          	  }
            var qcPeptideNumber :Int= if (quantiDS.protMatchPeptideNumberByPepMatchIdByQCId.contains(qcId)){
          	    val protMatchPeptideNumberByPepMatchId: Map[Long, Int] = quantiDS.protMatchPeptideNumberByPepMatchIdByQCId.get(qcId).get
          	    if (protMatchPeptideNumberByPepMatchId.contains(pmId)){
          	      protMatchPeptideNumberByPepMatchId.get(pmId).get
          	    }else{0}
          	  }else{
          	    0
          	  }
            exportMap += (fields.addField(f.title+"_"+protSetQuantiBuildingCtx.nameByQchId(qcId)) -> qcPeptideNumber)
          })
        }
        case ExportConfigConstant.FIELD_PROTEIN_SETS_QUANTI_RAW_ABUNDANCE => {
          protSetQuantiBuildingCtx.qcIds.foreach(qcId => {
            var qcRawAbun = if (protSetQuantiBuildingCtx.masterQuantProteinSet.quantProteinSetMap.contains(qcId)) {
              protSetQuantiBuildingCtx.masterQuantProteinSet.quantProteinSetMap(qcId).rawAbundance
            } else {
              ""
            }
            exportMap += (fields.addField(f.title+"_"+protSetQuantiBuildingCtx.nameByQchId(qcId)) -> qcRawAbun)
          })
        }
        case ExportConfigConstant.FIELD_PROTEIN_SETS_QUANTI_ABUNDANCE => {
          protSetQuantiBuildingCtx.qcIds.foreach(qcId => {
            var qcAbun = if (protSetQuantiBuildingCtx.masterQuantProteinSet.quantProteinSetMap.contains(qcId)) {
              protSetQuantiBuildingCtx.masterQuantProteinSet.quantProteinSetMap(qcId).abundance
            } else {
              ""
            }
            exportMap += (fields.addField(f.title+"_"+protSetQuantiBuildingCtx.nameByQchId(qcId)) -> qcAbun)
          })
        }
         case ExportConfigConstant.FIELD_PROTEIN_SETS_QUANTI_PSM_COUNT => {
          protSetQuantiBuildingCtx.qcIds.foreach(qcId => {
            var qcPSMCount = if (protSetQuantiBuildingCtx.masterQuantProteinSet.quantProteinSetMap.contains(qcId)) {
              protSetQuantiBuildingCtx.masterQuantProteinSet.quantProteinSetMap(qcId).peptideMatchesCount
            } else {
              ""
            }
            exportMap += (fields.addField(f.title+"_"+protSetQuantiBuildingCtx.nameByQchId(qcId)) -> qcPSMCount)
          })
        }
        case other => {
          // should not happen
        }
      }
    }
    
    //exportMap.map( r => r._1.toString -> r._2)
    exportMap
  }
  
  
  def onEachRecord( recordFormatter: Map[String,Any] => Unit ) {
    
    val rsm = identDS.resultSummary
    val rs = rsm.resultSet.get
    val protMatchById = rs.getProteinMatchById
    
    // Go through protein sets
    for( protSet <- rsm.proteinSets ) {
      if (exportAllProteinSet || protSet.isValidated){ // filter on validated proteinSet
      // Note that we export only protein matches which are loaded with the RSM
      // The result will depend of provider which have been used
      
      // Typical Protein Match is put first
      val typicalProtMatchId = protSet.getTypicalProteinMatchId
      
      val typicalProtMatch = if( typicalProtMatchId != 0 ) { 
        try{
          protMatchById(typicalProtMatchId)
        }catch{
          case e: NoSuchElementException => {
          // "old"SC, proteinSetSC.typicalProteinMatchId refers to identRS.proteinMatch, instead of quantiRS.proteinMatch cf Issue #12421  
            logger.error("Exception while retrieving typicalProteinMatchId on a Spectral Count. Spectral Count must be relaunched before export.")
            throw new Exception("Exception while retrieving typicalProteinMatchId on a Spectral Count. Spectral Count must be relaunched to be exported.")
          }
        }
      } else {
        protMatchById( protSet.getSameSetProteinMatchIds.head )
      }
      
      var buildingContext = new ProtMatchBuildingContext(
        protSet,
        protSet.peptideSet,
        typicalProtMatch
      )
      if (isQuanti){
        var masterQuantProteinSet : MasterQuantProteinSet = null
        for( mqProtSet <- quantiDS.quantRSM.masterQuantProteinSets ) {
          if (mqProtSet.proteinSet.id == protSet.id) {
            masterQuantProteinSet  =mqProtSet
          }
        }
        buildingContext = new ProtMatchQuantiBuildingContext(
        protSet,
        protSet.peptideSet,
        typicalProtMatch, 
        masterQuantProteinSet, 
        quantiDS.qcIds, 
        quantiDS.nameByQchId
      )
      }
      
      this.formatRecord( buildingContext, recordFormatter )
      }
    }
  }
}




class ProtSetToTypicalProtMatchView ( val identDS: IdentDataSet, val sheetConfig : ExportConfigSheet, val dateFormat : SimpleDateFormat, val decimalFormat: DecimalFormat, val exportAllProteinSet :Boolean  ) extends AbstractProtSetToTypicalProtMatchView {
  var viewName = "prot_set_to_typical_prot_match"
  
}