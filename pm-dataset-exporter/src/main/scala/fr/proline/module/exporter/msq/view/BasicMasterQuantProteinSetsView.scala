package fr.proline.module.exporter.msq.view

import fr.proline.module.exporter.api.view._
import fr.proline.core.om.model.msi._
import fr.proline.core.om.model.msq.QuantResultSummary
import fr.proline.core.om.model.msq.MasterQuantProteinSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

class BasicMasterQuantProteinSetsViewFields (qcIds: Array[Long], nameByQchId:  Map[Long,String]) extends IViewFieldEnumeration {
  // PROT SET HEADER
  val AC = Field("AC")
  val DESCRIPTION = Field("Description")
  val SELECTION_LEVEL = Field("selection_level")
  val PROT_SET_ID = Field("protein_set_id")
  
  // fields completed with dat name
  qcIds.foreach( qcId => {
    Field("rawAbundance_"+nameByQchId(qcId))
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

  def addRawAbundanceField(qcId: Long):  Field= {
    return Field("rawAbundance_"+nameByQchId(qcId))
  }
  def addPsmCountField(qcId: Long):  Field= {
    return Field("psm_count_"+nameByQchId(qcId))
  }
  
  
}


class BasicMasterQuantProteinSetsView (val quantiDS: QuantiDataSet ) extends IFixedDatasetView {
  var viewName = "exportQuantProteinSets"
  val fields  = new BasicMasterQuantProteinSetsViewFields(quantiDS.qcIds, quantiDS.nameByQchId)
  
  case class MyBuildingContext( mqProtSet: MasterQuantProteinSet, protSetCellsById: HashMap[Long, ProtSetCells], qcIds: Array[Long]) extends IRecordBuildingContext

  def buildRecord( buildingContext: IRecordBuildingContext ): Map[String,Any] = {
    val myBuildingContext = buildingContext.asInstanceOf[MyBuildingContext]
    // Cast the building context
    
    val mqProtSet = myBuildingContext.mqProtSet
    val protSetCellsById = myBuildingContext.protSetCellsById
    val qcIds = myBuildingContext.qcIds
    
    
    var exportMap:Map[Any,Any] = Map()
    
     // protein set data
    if (Some(mqProtSet.proteinSet).isDefined) {
      val protSetCell = protSetCellsById(Some(mqProtSet.proteinSet).get.id)
       exportMap += (
    		fields.AC -> protSetCell.accession,
    		fields.DESCRIPTION -> protSetCell.description,
    		fields.SELECTION_LEVEL -> protSetCell.selectionLevel,
    		fields.PROT_SET_ID -> protSetCell.proteinSetId
      )
    }
    
   
      
    // abundance
    qcIds.foreach( qcId => {
          var qcAbun = if(mqProtSet.quantProteinSetMap.contains(qcId)) {
            if (mqProtSet.quantProteinSetMap(qcId).abundance.isNaN()) "" else mqProtSet.quantProteinSetMap(qcId).abundance
          } else {
            ""
          }
          exportMap += ( fields.addAbundanceField(qcId) -> qcAbun)
        })
       
    // rawAbundance
    qcIds.foreach( qcId => {
          var qcRawAbun = if(mqProtSet.quantProteinSetMap.contains(qcId)) {
            mqProtSet.quantProteinSetMap(qcId).rawAbundance
          } else {
            ""
          }
          exportMap += ( fields.addRawAbundanceField(qcId) -> qcRawAbun)
        })
        
    // Add PSM_Count
    qcIds.foreach( qcId => {
      var qcPSMCount = if(mqProtSet.quantProteinSetMap.contains(qcId)) {
        mqProtSet.quantProteinSetMap(qcId).peptideMatchesCount
      } else {
        ""
      }
      exportMap += ( fields.addPsmCountField(qcId) -> qcPSMCount)
    })
     
    
     exportMap.map( r => r._1.toString -> r._2)
     
  }
  
   def onEachRecord( recordFormatter: Map[String,Any] => Unit ) {
    val quantRsm = quantiDS.quantRSM
    val protSetCellsById = quantiDS.protSetCellsById
    val qcIds = quantiDS.qcIds
    for( mqProtSet <- quantRsm.masterQuantProteinSets ) {  
      if( mqProtSet.proteinSet.isValidated ) {
    	this.formatRecord(MyBuildingContext(mqProtSet, protSetCellsById, qcIds), recordFormatter)
      }
    }
  }
  
}