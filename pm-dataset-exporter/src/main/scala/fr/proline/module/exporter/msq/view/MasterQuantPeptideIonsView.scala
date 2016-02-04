package fr.proline.module.exporter.msq.view

import fr.proline.module.exporter.api.view._
import fr.proline.core.om.model.msi._
import fr.proline.core.om.model.msq.QuantResultSummary
import fr.proline.core.om.model.msq.RatioDefinition
import fr.proline.core.om.model.msq.ComputedRatio
import fr.proline.core.om.model.msq.MasterQuantPeptide
import fr.proline.core.om.model.msq.MasterQuantPeptideIon
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

class MasterQuantPeptideIonsViewFields (qcIds: Array[Long], ratioDefs: Array[RatioDefinition], nameByQchId:  Map[Long,String]) extends IViewFieldEnumeration {
  // PROT SET HEADER
  val AC = Field("AC")
  val DESCRIPTION = Field("Description")
  val SELECTION_LEVEL = Field("selection_level")
  val PROT_SET_ID = Field("protein_set_id")
  val ALL_AC = Field("ACs")
  
  // PEP HEADER
  val PEPTIDE_ID = Field("peptide_id")
  val SEQUENCE = Field("sequence")
  val PTMS = Field("ptms")
  val PROT_SET_COUNT = Field("prot_set_count")
  val CALC_MASS = Field("calc_mass")
  
  
  // MQ PEP HEADER
  val QUANT_PEPTIDE_ID = Field("quant_peptide_id")
  val BEST_PEPTIDE_MATCH_SCORE = Field("best_peptide_match_score")
  val ELUTION_TIME = Field("elution_time")
  val SELECTION_LEVEL_MQ_PEP = Field("selection_level_peptide")
  
   // fields completed with dat name
  qcIds.foreach( qcId => {
    Field("raw_abundance_"+nameByQchId(qcId))
  })
  qcIds.foreach( qcId => {
    Field("abundance_"+nameByQchId(qcId))
  })
  qcIds.foreach( qcId => {
    Field("psm_count_"+nameByQchId(qcId))
  })
  
  def addAbundanceField(qcId: Long):  Field= {
    return Field("abundance_"+nameByQchId(qcId))
  }
  
  def addPsmCountField(qcId: Long):  Field= {
    return Field("psm_count_"+nameByQchId(qcId))
  }
  
   def addRawAbundanceField(qcId: Long):  Field= {
    return Field("raw_abundance_"+nameByQchId(qcId))
  }
  
   
  // STAT HEADER
  for(r <- ratioDefs ) {
      Field("ratio"+ ("_g" + r.numeratorGroupNumber +" _vs_g"+ r.denominatorGroupNumber))
      Field("t-test_pvalue"+ ("_g" + r.numeratorGroupNumber +" _vs_g"+ r.denominatorGroupNumber))
      Field("z-test_pvalue"+ ("_g" + r.numeratorGroupNumber +" _vs_g"+ r.denominatorGroupNumber))
  }
  
  
  def addRatioField(r: RatioDefinition ): Field= {
    return Field("ratio" + ("_g" + r.numeratorGroupNumber +" _vs_g"+ r.denominatorGroupNumber))
  }
  def addTTestValueField(r: RatioDefinition): Field= {
    return Field("t-test_pvalue" + ("_g" + r.numeratorGroupNumber +" _vs_g"+ r.denominatorGroupNumber))
  }
  def addZTestValueField(r: RatioDefinition): Field= {
    return Field("z-test_pvalue" + ("_g" + r.numeratorGroupNumber +" _vs_g"+ r.denominatorGroupNumber))
  }

  
  // MQ PEP ION HEADER
  val CHARGE = Field("charge")
  val MASTER_ELUTION_TIME = Field("master_elution_time")
  val MASTER_FEATURE_ID = Field("master_feature_id")
  
  // Q PEP ION HEADER
  qcIds.foreach( qcId => {
    Field("moz_"+nameByQchId(qcId))
    Field("elution_time_"+nameByQchId(qcId))
    Field("correct_elution_time_"+nameByQchId(qcId))
    Field("duration_"+nameByQchId(qcId))
    Field("raw_abundance_"+nameByQchId(qcId))
    Field("abundance_"+nameByQchId(qcId))
    Field("psm_count_"+nameByQchId(qcId))
    Field("ms2_matching_freq_"+nameByQchId(qcId))
    Field("feature_id_"+nameByQchId(qcId))
  })
   
  def addMozIonField(qcId: Long):  Field= {
    return Field("moz_"+nameByQchId(qcId))
  }
  def addElutionTimeIonField(qcId: Long):  Field= {
    return Field("elution_time_"+nameByQchId(qcId))
  }
  def addCorrectElutionTimeIonField(qcId: Long):  Field= {
    return Field("correct_elution_time_"+nameByQchId(qcId))
  }
  def addDurationIonField(qcId: Long):  Field= {
    return Field("duration_"+nameByQchId(qcId))
  }
  def addRawAbundanceIonField(qcId: Long):  Field= {
    return Field("raw_abundance_"+nameByQchId(qcId))
  }
  def addAbundanceIonField(qcId: Long):  Field= {
    return Field("abundance_"+nameByQchId(qcId))
  }
  def addPsmCountIonField(qcId: Long):  Field= {
    return Field("psm_count_"+nameByQchId(qcId))
  }
  def addMs2MatchingFreqIonField(qcId: Long):  Field= {
    return Field("ms2_matching_freq_"+nameByQchId(qcId))
  }
  def addFeatureIdIonField(qcId: Long):  Field= {
    return Field("feature_id_"+nameByQchId(qcId))
  }

}

class MasterQuantPeptideIonsView( val quantiDS: QuantiDataSet ) extends IFixedTableView {
  
  
  val fields = new MasterQuantPeptideIonsViewFields(quantiDS.qcIds, quantiDS.ratioDefs, quantiDS.nameByQchId)
  var viewName = "exportQuantPeptideIons"
  
  case class MyBuildingContext(mqPepIon: MasterQuantPeptideIon,  mqPep: MasterQuantPeptide, pepMatchById: Map[Long, PeptideMatch],
      protSetsOpt: Option[ArrayBuffer[ProteinSet]], protSetCellsById: HashMap[Long,ProtSetCells], 
      pepInstOpt: Option[PeptideInstance], qcIds: Array[Long], ratioDefs: Array[RatioDefinition]) extends IRecordBuildingContext
      
  def buildRecord( buildingContext: IRecordBuildingContext ): Map[String,Any] = {
    val myBuildingContext = buildingContext.asInstanceOf[MyBuildingContext]
    // Cast the building context
    val mqPepIon = myBuildingContext.mqPepIon
    val mqPep = myBuildingContext.mqPep
    val allProtSetOpt = myBuildingContext.protSetsOpt
    val protSetCellsById = myBuildingContext.protSetCellsById
    val pepInstOpt = myBuildingContext.pepInstOpt
    val qcIds = myBuildingContext.qcIds
    val pepMatchById =  myBuildingContext.pepMatchById
    val ratioDefs = myBuildingContext.ratioDefs
    
    // TODO: check if this is really the best score
    val scoreOpt = mqPep.peptideInstance.map( pi => pepMatchById(pi.bestPeptideMatchId).score )
    val bestQPep = mqPep.getBestQuantPeptide
    
    //TODO: retrieve the right value
    val groupSetupNumber = 1
    
    var exportMap:Map[Any,Any] = Map()
    
    // protein set data
    if (allProtSetOpt.isDefined && allProtSetOpt.size>0) {
      val firstId = allProtSetOpt.get.apply(0).id   
      val protSetCell = protSetCellsById(firstId)
      exportMap += (
		  fields.AC -> protSetCell.accession,
		  fields.DESCRIPTION -> protSetCell.description,
		  fields.SELECTION_LEVEL -> protSetCell.selectionLevel,
		  fields.PROT_SET_ID -> protSetCell.proteinSetId
      )
      
      val accsBuilder = new StringBuilder(protSetCell.accession)
      for( i <- 1  to (allProtSetOpt.get.size-1)){       
        accsBuilder.append(", ").append(protSetCellsById(allProtSetOpt.get.apply(i).id ).accession)       
      }
      exportMap += ( fields.ALL_AC -> accsBuilder.toString())
      
    }
    
    
    // Peptide Instance data
    if( pepInstOpt.isDefined ) {
    	  //Ajouter le m/z du pepUnst
    	  val pepInst = pepInstOpt.get
    	  val peptide = pepInst.peptide
    	  exportMap += ( fields.PEPTIDE_ID -> peptide.id)
    	  exportMap += ( fields.SEQUENCE -> peptide.sequence)
    	  exportMap += ( fields.PTMS -> peptide.readablePtmString)
    	  exportMap += ( fields.PROT_SET_COUNT -> pepInst.proteinSetsCount)
    	  exportMap += ( fields.CALC_MASS -> pepInst.peptide.calculatedMass)
    }
    
    // master quant peptide data
    val elutionTime = if (bestQPep.elutionTime.isNaN()) "" else bestQPep.elutionTime
    exportMap += ( fields.QUANT_PEPTIDE_ID -> mqPep.id)
    exportMap += ( fields.BEST_PEPTIDE_MATCH_SCORE -> scoreOpt.getOrElse(""))
    exportMap += ( fields.ELUTION_TIME -> elutionTime)
    exportMap += ( fields.SELECTION_LEVEL_MQ_PEP -> mqPep.selectionLevel)
    
    
    val quantPepMap = mqPep.quantPeptideMap
    val abundanceBuffer = new ArrayBuffer[Any]
    val rawAbundanceBuffer = new ArrayBuffer[Any]
    val psmCountBuffer = new ArrayBuffer[Any]
      
    for(qcId <- qcIds) {
        val qPepOpt = quantPepMap.get(qcId)
        
        if( qPepOpt.isDefined ) {
          val qPep = qPepOpt.get
          abundanceBuffer += (if( qPep.abundance.isNaN ) "" else qPep.abundance)
          rawAbundanceBuffer += (if( qPep.rawAbundance.isNaN ) "" else qPep.rawAbundance)
          psmCountBuffer += qPep.peptideMatchesCount
        } else {
          abundanceBuffer += ""
          rawAbundanceBuffer += ""
          psmCountBuffer += ""
        }
    }
    
    // Add raw abundances
      qcIds.foreach( qcId => {
          val qPepOpt = quantPepMap.get(qcId)
          if( qPepOpt.isDefined ) {
        	  val qPep = qPepOpt.get
        	  var rawAbundance = (if( qPep.rawAbundance.isNaN ) "" else qPep.rawAbundance)
        	  exportMap += ( fields.addRawAbundanceField(qcId) -> rawAbundance)
          }
        })
      
      // Add abundances
      qcIds.foreach( qcId => {
          val qPepOpt = quantPepMap.get(qcId)
          if( qPepOpt.isDefined ) {
        	  val qPep = qPepOpt.get
        	  var abundance = (if( qPep.abundance.isNaN ) "" else qPep.abundance)
        	  exportMap += ( fields.addAbundanceField(qcId) -> abundance)
          }
          
        })
      
      // Add PSM counts
       qcIds.foreach( qcId => {
          val qPepOpt = quantPepMap.get(qcId)
          if( qPepOpt.isDefined ) {
        	  val qPep = qPepOpt.get
        	  var psmCount = qPep.peptideMatchesCount
        	  exportMap += ( fields.addPsmCountField(qcId) -> psmCount)
          }
          
        })
        
     // Add some statistics
     if( mqPep.properties.isDefined && mqPep.properties.get.getMqPepProfileByGroupSetupNumber.isDefined ) {
        val mqPepProfile = mqPep.properties.get.getMqPepProfileByGroupSetupNumber.get(groupSetupNumber)
        val stats = this.stringifyRatiosStats(mqPepProfile.getRatios)    
        val nbS = stats.size
        var i=0;
        for(r <- ratioDefs ) {
          if (nbS > i+0) {
            exportMap += ( fields.addRatioField(r) -> stats(i+0))
          } 
          if (nbS > i+1) {
            exportMap += ( fields.addTTestValueField(r) -> stats(i+1))
          }  
          if (nbS > i+2) {
            exportMap += ( fields.addZTestValueField(r) -> stats(i+2))
          }  
          i=i+1
        }
     } else {
        //row ++= Array.fill(this.ratioDefs.length * 3)("")
     }
    
    // Append master quant peptide ion data
    exportMap += ( fields.CHARGE ->mqPepIon.charge)
    exportMap += ( fields.MASTER_ELUTION_TIME ->mqPepIon.elutionTime)
    exportMap += ( fields.MASTER_FEATURE_ID ->mqPepIon.lcmsMasterFeatureId.getOrElse(""))
    
    // Append quant peptide data for each condition
    val qPepIonCellsByQcId = new HashMap[Long,Seq[Any]]
    for( (qcId, qPepIon) <- mqPepIon.quantPeptideIonMap ) {
       val qPepIonCells = List(
          qPepIon.moz,
          qPepIon.elutionTime,
          qPepIon.correctedElutionTime,
          qPepIon.duration,
          if( qPepIon.rawAbundance.isNaN ) "" else qPepIon.rawAbundance,
          if( qPepIon.abundance.isNaN ) "" else qPepIon.abundance,
          qPepIon.peptideMatchesCount,
          qPepIon.ms2MatchingFrequency.getOrElse(0),
          qPepIon.lcmsFeatureId
       )
        
       qPepIonCellsByQcId += qcId -> qPepIonCells
    }
    
    for(qcId <- qcIds) {
        val qPepIonCellsOpt = qPepIonCellsByQcId.get(qcId)
        if( qPepIonCellsOpt.isDefined ) {
           val listF =  qPepIonCellsOpt.get
           exportMap += ( fields.addMozIonField(qcId) -> listF(0))
           exportMap += ( fields.addElutionTimeIonField(qcId) -> listF(1))
           exportMap += ( fields.addCorrectElutionTimeIonField(qcId) -> listF(2))
           exportMap += ( fields.addDurationIonField(qcId) -> listF(3))
           exportMap += ( fields.addRawAbundanceIonField(qcId) -> listF(4))
           exportMap += ( fields.addAbundanceIonField(qcId) -> listF(5))
           exportMap += ( fields.addPsmCountIonField(qcId) -> listF(6))
           exportMap += ( fields.addMs2MatchingFreqIonField(qcId) -> listF(7))
           exportMap += ( fields.addFeatureIdIonField(qcId) -> listF(8))
        }
        else {
          //row ++= Array.fill(qPepIonHeaders.length)("")
        }
        
    }
    
    
    // 
    exportMap.map( r => r._1.toString -> r._2)
  }
  
  
  private def _getRatioStats(r: ComputedRatio) = Array(r.getState, r.getTTestPValue.getOrElse(""), r.getZTestPValue.getOrElse(""))
  
  protected def stringifyRatiosStats(ratios: List[Option[ComputedRatio]]): List[String] = {
    ratios.flatMap(_.map( this._getRatioStats(_).map(_.toString) ).getOrElse(Array.fill(3)("")) )
  }
   
   
  def onEachRecord( recordFormatter: Map[String,Any] => Unit ) {
    val quantRsm = quantiDS.quantRSM
    val protSetCellsById = quantiDS.protSetCellsById
    val qcIds = quantiDS.qcIds
    val ratioDefs = quantiDS.ratioDefs
    
    val pepMatchById = quantRsm.resultSummary.resultSet.get.getPeptideMatchById()
    
    // Create some mappings
    val mqPepById = Map() ++ quantRsm.masterQuantPeptides.map( mqPep => mqPep.id -> mqPep ) 
  
//    val protSetByPepInst = Map()++ quantRsm.resultSummary.proteinSets.flatMap( protSet => protSet.peptideSet.getPeptideInstances.map( pi => pi.id -> protSet ) )
    
    val protSetsByPepInstID = new HashMap[Long, ArrayBuffer[ProteinSet]]
    quantRsm.resultSummary.proteinSets.foreach(protSet => {
      protSet.peptideSet.getPeptideInstances.foreach(pepInst => {
        val protSetList = protSetsByPepInstID.getOrElseUpdate(pepInst.id, new ArrayBuffer[ProteinSet]())
        protSetList += protSet        
      })            
    })
    
    // Iterate over master quant peptides to export them
    quantRsm.masterQuantPeptideIons.foreach { mqPepIon =>
      val mqPep = mqPepById(mqPepIon.masterQuantPeptideId)
      // Append protein set and peptide data if they are defined
      if( mqPep.peptideInstance.isDefined) {
        val pepInstId = mqPep.peptideInstance.get.id
         this.formatRecord(MyBuildingContext( mqPepIon, mqPep, pepMatchById, protSetsByPepInstID.get(pepInstId), 
            protSetCellsById, mqPep.peptideInstance,  qcIds, ratioDefs), recordFormatter)
      } else {
         this.formatRecord(MyBuildingContext( mqPepIon, mqPep, pepMatchById, None, protSetCellsById,None, qcIds, ratioDefs), recordFormatter)
      }
    }
  }

}