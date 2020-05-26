package fr.proline.module.exporter.dataset

import fr.profi.util.collection._
import fr.proline.core.om.model.msi._
import fr.proline.core.om.model.msq._

import scala.collection.mutable.LongMap

class QuantDataset(
                    override val projectName: String,
                    val quantRSM: LazyQuantResultSummary,
                    val expDesign: Option[ExperimentalDesign],
                    val quantConfigAndMethod: Option[(String,IQuantMethod)],
                    val profilizerConfig: Option[String],
                    val masterQuantChannel: MasterQuantChannel,
                    val groupSetupNumber: Int,
                    override protected val loadLeaveResultSummaries: () => Array[LazyResultSummary],
                    override protected val loadLeaveResultSets: () => Array[LazyResultSet],
                    override protected val loadBioSequences: () => Array[BioSequence],
                    override protected val loadSpectraDescriptors: (Array[Long]) => Array[Spectrum],
                    override val loadPepMatches: (Array[Long]) => Array[PeptideMatch],
                    val loadPepMatchesByMsQIs: (Array[Long]) => Array[PeptideMatch],
                    // val qcNameById: LongMap[String], //  update and use the QuantChannel name in the UDSdb
                    //val protMatchStatusByIdPepMatchByQCId: Map[Long, Map[Long, String]],
                    val peptideCountByProtMatchIdByQCId: Option[LongMap[LongMap[Int]]],
                    val peptideCountByMqProtSetByQCId: Option[LongMap[LongMap[Int]]],
                    override val loadPtmDataset: () => Option[PtmDataSet]
  
) extends IdentDataset(projectName,quantRSM.lazyResultSummary,loadLeaveResultSummaries,loadLeaveResultSets, loadBioSequences,loadSpectraDescriptors, loadPepMatches, loadPtmDataset) {


  lazy val identRsmById = childResultSummaries.mapByLong(_.id)
  lazy val identRsmByQcId = {
    masterQuantChannel.quantChannels.toLongMapWith { qc =>
      qc.id -> identRsmById.getOrElse(qc.identResultSummaryId, null)
    }.filter(_._2 != null)
  }

  lazy val qcNameByQcId = {
    this.masterQuantChannel.quantChannels.toLongMapWith(qc => qc.id -> qc.name)
  }
  
  lazy val qcNameByRsId = {
    val qcNameById = this.masterQuantChannel.quantChannels.toLongMapWith(qc => qc.id -> qc.name)
    this.identRsmByQcId.map { case (qcId,identRsm) =>
      identRsm.lazyResultSet.id -> qcNameById(qcId)
    }
  }
  
  lazy val masterQuantChannelId: Long = masterQuantChannel.id
  lazy val qcIds: Array[Long] = masterQuantChannel.quantChannels.map(_.id)
  // TODO: find a better way to discriminate between XIC and SC expDesign
  lazy val ratioDefs: Option[Array[RatioDefinition]] = {
    expDesign.flatMap( _.groupSetupByNumber.get(groupSetupNumber).map(_.ratioDefinitions) )
  }

  
  lazy val mqPepByPepId: LongMap[MasterQuantPeptide] = {
    
    val longMap = new LongMap[MasterQuantPeptide](quantRSM.masterQuantPeptides.length)
    for( mqPep <- quantRSM.masterQuantPeptides;
        pepInst <- mqPep.peptideInstance
      ) {
        longMap += pepInst.peptide.id -> mqPep
    }
    
    longMap
  }
  
  lazy val mqProtSetByProtSetId: LongMap[MasterQuantProteinSet] = {
    quantRSM.masterQuantProteinSets.toLongMapWith { mqProtSet => mqProtSet.proteinSet.id -> mqProtSet }
  }
  

}
