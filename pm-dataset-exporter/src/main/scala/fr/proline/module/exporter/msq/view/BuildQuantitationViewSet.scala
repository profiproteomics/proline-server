package fr.proline.module.exporter.msq.view

import fr.profi.util.primitives.toLong
import fr.proline.module.exporter.api.template._
import fr.proline.context.IExecutionContext
import fr.proline.core.dal.helper.UdsDbHelper
import fr.proline.core.dal.DoJDBCReturningWork
import fr.proline.core.dal.tables.SelectQueryBuilder1
import fr.proline.core.dal.tables.SelectQueryBuilder3
import fr.proline.core.dal.tables.uds.UdsDbQuantChannelTable
import fr.proline.core.dal.tables.msi.MsiDbResultSetTable
import fr.proline.core.dal.tables.msi.MsiDbResultSummaryTable
import fr.proline.core.dal.tables.msi.MsiDbMsiSearchTable
import fr.proline.core.dal.tables.SelectQueryBuilder.any2ClauseAdd
import fr.proline.core.om.provider.msq.impl.SQLQuantResultSummaryProvider
import fr.proline.core.om.provider.msq.impl.SQLExperimentalDesignProvider
import fr.proline.core.om.model.msq.ExperimentalDesign
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap


object BuildQuantitationViewSet {

  def apply(ds: QuantiDataSet, viewSetName: String, viewSetTemplate: IViewSetTemplate): QuantitationViewSet = {

    val templatedViews = viewSetTemplate.templatedViewTypes.map { templatedViewType =>
      val viewWithTpl = ViewWithTemplate(BuildQuantitationView(ds, templatedViewType.viewType), templatedViewType.template)
      if (templatedViewType.viewName.isDefined) viewWithTpl.dataView.viewName = templatedViewType.viewName.get

      viewWithTpl
    }

    new QuantitationViewSet(viewSetName, templatedViews)
  }
  
  def apply(
    executionContext: IExecutionContext,
    projectId: Long,
    quantDSId: Long,
    masterQuantChannelId: Long,
    expDesign: ExperimentalDesign,
    viewSetName: String,
    viewSetTemplate: IViewSetTemplate
  ): QuantitationViewSet = {

    val udsSQLCtx = executionContext.getUDSDbConnectionContext()
    val psSQLCtx = executionContext.getPSDbConnectionContext()
    val msiSQLCtx = executionContext.getMSIDbConnectionContext()
    

    val udsDbHelper = new UdsDbHelper(udsSQLCtx)
    
    val quantRsmId = udsDbHelper.getMasterQuantChannelQuantRsmId( masterQuantChannelId )
    val qcIds = udsDbHelper.getQuantChannelIds(masterQuantChannelId)
  
    lazy val nameByQchId : Map[Long,String] = {
    
	val qChIdByRsmId : Map[Long, Long]= { DoJDBCReturningWork.withEzDBC(udsSQLCtx, { ezDBC =>
   
	 	val rsmIdForquantChannelQuery = new SelectQueryBuilder1(UdsDbQuantChannelTable).mkSelectQuery(
		  (t1, c1) => List( t1.IDENT_RESULT_SUMMARY_ID, t1.ID) ->
		  " WHERE " ~ t1.ID ~ " IN(" ~ qcIds.mkString(",") ~ ")"          
		)
  
		ezDBC.select(rsmIdForquantChannelQuery){ r =>
        	toLong(r.nextAny) -> toLong(r.nextAny)
	 	} toMap
       
	 })  
	}
	
  DoJDBCReturningWork.withEzDBC(msiSQLCtx, { ezDBC =>
              
       val rsNameForRsmIdQuery = new SelectQueryBuilder3(MsiDbResultSetTable,MsiDbResultSummaryTable,MsiDbMsiSearchTable).mkSelectQuery(
        (t1,c1,t2,c2, t3, c3) =>List(t2.ID, t3.RESULT_FILE_NAME) ->
          " WHERE " ~ t2.ID ~ " IN(" ~ qChIdByRsmId.keys.mkString(",") ~ ") " ~
          "AND " ~ t1.ID ~ "=" ~ t2.RESULT_SET_ID ~ " AND " ~ t3.ID ~" = "~ t1.MSI_SEARCH_ID 
      )
  
      val resultBuilder = Map.newBuilder[Long,String]
      ezDBC.selectAndProcess( rsNameForRsmIdQuery ) { r => 
        
        resultBuilder += qChIdByRsmId(toLong(r.nextAny)) -> r.nextString     
        ()
      }
       
       resultBuilder.result
       
    })
  }
    
    
    // quant RSM
    val quantRSM = {
    		val quantRsmProvider = new SQLQuantResultSummaryProvider(msiSQLCtx,psSQLCtx ,udsSQLCtx)
    		quantRsmProvider.getQuantResultSummary(quantRsmId.get, qcIds, true).get
   }
    
    // 
    val protMatchById = quantRSM.resultSummary.resultSet.get.proteinMatchById
    
    //
    val protSetCellsById = {
    
    val tmpProtSetCellsById = new HashMap[Long,ProtSetCells]
    for( mqProtSet <- quantRSM.masterQuantProteinSets ) {
      val protMatch = protMatchById( mqProtSet.proteinSet.getTypicalProteinMatchId )
      val protSetCells = new ProtSetCells(accession = protMatch.accession, description =  protMatch.description, selectionLevel = mqProtSet.selectionLevel, proteinSetId = mqProtSet.proteinSet.id)
      
      tmpProtSetCellsById += mqProtSet.id -> protSetCells
    }
    
    tmpProtSetCellsById
  }
    
    //TODO: retrieve the right value
    val groupSetupNumber = 1
	val ratioDefs = expDesign.groupSetupByNumber(groupSetupNumber).ratioDefinitions
	
    return apply(QuantiDataSet(masterQuantChannelId, quantRSM, protMatchById, protSetCellsById, qcIds, expDesign, ratioDefs, nameByQchId), viewSetName, viewSetTemplate)
  }
  
}