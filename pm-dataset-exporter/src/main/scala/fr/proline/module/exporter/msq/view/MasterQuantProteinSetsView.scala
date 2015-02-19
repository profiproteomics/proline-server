package fr.proline.module.exporter.msq.view

import fr.proline.module.exporter.api.view._
import fr.proline.core.om.model.msi._
import fr.proline.core.om.model.msq.QuantResultSummary
import fr.proline.core.om.model.msq.MasterQuantProteinSet
import fr.proline.core.om.model.msq.RatioDefinition
import fr.proline.core.om.model.msq.MasterQuantProteinSetProfile
import fr.proline.core.om.model.msq.MasterQuantPeptide
import fr.proline.core.om.model.msq.ComputedRatio
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

class MasterQuantProteinSetsViewFields(qcIds: Array[Long], ratioDefs: Array[RatioDefinition], nameByQchId:  Map[Long,String]) extends IViewFieldEnumeration {
  // PROT SET HEADER
  val AC = Field("AC")
  val DESCRIPTION = Field("Description")
  val SELECTION_LEVEL = Field("selection_level")
  val PROT_SET_ID = Field("protein_set_id")
  
  // MQ PROT SET PROFILE HEADER 
  val PEPTIDE_COUNT = Field("peptides_count")
  
  qcIds.foreach( qcId => {
    Field("raw_abundance_"+nameByQchId(qcId))
  })
  
  qcIds.foreach( qcId => {
    Field("abundance_"+nameByQchId(qcId))
  })

  qcIds.foreach( qcId => {
    Field("psm_count_"+nameByQchId(qcId))
  })
  
  def addRawAbundanceField(qcId: Long):  Field= {
    return Field("raw_abundance_"+nameByQchId(qcId))
  }
  
  def addAbundanceField(qcId: Long):  Field= {
    return Field("abundance_"+nameByQchId(qcId))
  }
  

  def addPsmCountField(qcId: Long):  Field= {
    return Field("psm_count_"+nameByQchId(qcId))
  }
  
  // STAT HEADER
  for(r <- ratioDefs ) {
      Field("ratio"+ ("_g" + r.numeratorGroupNumber +" _vs_g"+ r.denominatorGroupNumber))
      Field("t-test_pvalue"+ ("_g" + r.numeratorGroupNumber +" _vs_g"+ r.denominatorGroupNumber))
      Field("z-test_pvalue"+ ("_g" + r.numeratorGroupNumber +" _vs_g"+ r.denominatorGroupNumber))
      Field("z-score"+ ("_g" + r.numeratorGroupNumber +" _vs_g"+ r.denominatorGroupNumber))
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
  def addZScoreField(r: RatioDefinition): Field = {
    return Field("z-score" + ("_g" + r.numeratorGroupNumber +" _vs_g"+ r.denominatorGroupNumber))
  }
  
}

class MasterQuantProteinSetsView (val quantiDS: QuantiDataSet ) extends IFixedDatasetView {
  
  val fields  = new MasterQuantProteinSetsViewFields(quantiDS.qcIds, quantiDS.ratioDefs, quantiDS.nameByQchId)
  var viewName = "exportQuantProteinSetsProfile"
  
  case class MyBuildingContext( mqProtSet: MasterQuantProteinSet, protSetCellsById: HashMap[Long,ProtSetCells], qcIds: Array[Long], profile: MasterQuantProteinSetProfile, 
      mqPepById: Map[Long, MasterQuantPeptide], ratioDefs: Array[RatioDefinition]) extends IRecordBuildingContext
      
  def buildRecord( buildingContext: IRecordBuildingContext ): Map[String,Any] = {
    val myBuildingContext = buildingContext.asInstanceOf[MyBuildingContext]
    // Cast the building context
    
    val mqProtSet = myBuildingContext.mqProtSet
    val protSetCellsById = myBuildingContext.protSetCellsById
    val qcIds = myBuildingContext.qcIds
    val profile = myBuildingContext.profile
    val mqPepById = myBuildingContext.mqPepById
    val ratioDefs = myBuildingContext.ratioDefs
    
    var exportMap:Map[Any,Any] = Map()
    
     // protein set data
     // Add number of peptides
    if (Some(mqProtSet.proteinSet).isDefined) {
      val protSetCell = protSetCellsById(Some(mqProtSet.proteinSet).get.id)
      exportMap += (
    		fields.AC -> protSetCell.accession,
    		fields.DESCRIPTION -> protSetCell.description,
    		fields.SELECTION_LEVEL -> protSetCell.selectionLevel,
    		fields.PROT_SET_ID-> protSetCell.proteinSetId,
    		fields.PEPTIDE_COUNT -> profile.mqPeptideIds.length
      )
    }
    
    // rawabundance
    var k=0;
    qcIds.foreach( qcId => {
        var qcRawAbun = if (profile.rawAbundances.isEmpty || profile.rawAbundances(k).isNaN ) "" else profile.rawAbundances(k)
          exportMap += ( fields.addRawAbundanceField(qcId) -> qcRawAbun)
          k = k+1
        })
     
    // abundance
    k=0;
    qcIds.foreach( qcId => {
    	  var qcAbun = if (profile.abundances(k).isNaN ) "" else profile.abundances(k)
          exportMap += ( fields.addAbundanceField(qcId) -> qcAbun)
          k = k+1
        })
           
        
     // Sum the PSM count for each quant channel of this quantitative profile
     val pepMatchCountByQcId = new HashMap[Long,Int]
        profile.getMqPeptideIds().foreach { mqPepId =>
          val mqPep = mqPepById(mqPepId)
          for( (qcId,qPep) <- mqPep.quantPeptideMap ) {
            val count = pepMatchCountByQcId.getOrElseUpdate(qcId, 0)
            pepMatchCountByQcId(qcId) = count + qPep.peptideMatchesCount
          }
     }
    
    // Add PSM_Count
    qcIds.foreach( qcId => {
      var qcPSMCount = pepMatchCountByQcId.getOrElse(qcId, 0)
      exportMap += ( fields.addPsmCountField(qcId) -> qcPSMCount)
    })
     
    // Add some statistics
    val stats = this.stringifyRatiosStats(profile.getRatios)
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
          if (nbS > i+3) {
            exportMap += ( fields.addZScoreField(r) -> stats(i+3))
          }  
          i=i+1
     }
        
     exportMap.map( r => r._1.toString -> r._2)
    
  }
  
  private def _getRatioStats(r: ComputedRatio) = Array(
    r.getState, r.getTTestPValue.getOrElse(""), r.getZTestPValue.getOrElse(""), r.getZScore().getOrElse("")
  )
  
  protected def stringifyRatiosStats(ratios: List[Option[ComputedRatio]]): List[String] = {
    ratios.flatMap(_.map( this._getRatioStats(_).map(_.toString) ).getOrElse(Array.fill(3)("")) )
  }
  
  
   def onEachRecord( recordFormatter: Map[String,Any] => Unit ) {
    val exportBestProfile: Boolean = true
     
    val quantRsm = quantiDS.quantRSM
    val protSetCellsById = quantiDS.protSetCellsById
    val qcIds = quantiDS.qcIds
    val ratioDefs = quantiDS.ratioDefs
    
    // TODO: retrieve the right value
    val groupSetupNumber = 1
    
    val mqPepById = quantRsm.masterQuantPeptides.map( mqPep => mqPep.id -> mqPep ).toMap
    
    // Iterate over master quant peptides to export them
    quantRsm.masterQuantProteinSets.foreach { mqProtSet =>
    
     if( mqProtSet.proteinSet.isValidated ) {
        if( exportBestProfile ) {
          val bestProfile = mqProtSet.getBestProfile(groupSetupNumber)
          if( bestProfile.isDefined ) this.formatRecord(MyBuildingContext(mqProtSet, protSetCellsById, qcIds,  bestProfile.get, mqPepById, ratioDefs), recordFormatter) 
        } else {
          
          // Iterate over all profiles to export them
          for( props <- mqProtSet.properties;
               profileByGSNum <- props.getMqProtSetProfilesByGroupSetupNumber;
               profiles <- profileByGSNum.get(groupSetupNumber);
               profile <- profiles
             ) {
             this.formatRecord(MyBuildingContext(mqProtSet, protSetCellsById, qcIds,  profile, mqPepById, ratioDefs), recordFormatter) 
          }
        }
      }
    }
    
    
  }
}