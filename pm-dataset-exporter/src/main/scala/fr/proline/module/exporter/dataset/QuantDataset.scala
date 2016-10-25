package fr.proline.module.exporter.dataset

import scala.collection.JavaConversions._
import scala.collection.mutable.LongMap
import fr.profi.util.collection._
import fr.proline.core.om.model.msi._
import fr.proline.core.om.model.msq._
import fr.proline.core.algo.msq.config.IQuantConfig
import fr.proline.core.algo.msq.config.profilizer.ProfilizerConfig
import fr.proline.core.orm.uds.{ MasterQuantitationChannel => UdsMasterQuantChannel }

class QuantDataset(
  override val projectName: String,
  val quantRSM: LazyQuantResultSummary,
  val expDesign: Option[ExperimentalDesign],
  val quantConfigAndMethod: Option[(IQuantConfig,IQuantMethod)],
  val profilizerConfig: Option[ProfilizerConfig],
  val masterQuantChannel: MasterQuantChannel,
  val groupSetupNumber: Int,
  override protected val loadChildResultSummaries: () => Array[LazyResultSummary],
  override protected val loadLeaveResultSets: () => Array[LazyResultSet],
  override protected val loadBioSequences: () => Array[BioSequence],
  override protected val loadSpectraDescriptors: (Array[Long]) => Array[Spectrum],
  
  val qcNameById: LongMap[String], // TODO: update and use the QuantChannel name in the UDSdb
  //val protMatchStatusByIdPepMatchByQCId: Map[Long, Map[Long, String]],
  val peptideCountByProtMatchIdByQCId: Option[LongMap[LongMap[Int]]],
  val peptideCountByMqProtSetByQCId: Option[LongMap[LongMap[Int]]]
  
) extends IdentDataset(projectName,quantRSM.lazyResultSummary,loadChildResultSummaries,loadLeaveResultSets, loadBioSequences,loadSpectraDescriptors) {
  
  lazy val identRsmById = childResultSummaries.mapByLong(_.id)
  lazy val identRsmByQcId = masterQuantChannel.quantChannels.toLongMapWith { qc =>
    qc.id -> identRsmById(qc.identResultSummaryId)
  }
  
  lazy val masterQuantChannelId: Long = masterQuantChannel.id
  lazy val qcIds: Array[Long] = masterQuantChannel.quantChannels.map(_.id).toArray
  // TODO: find a better way to discriminate between XIC and SC expDesign
  lazy val ratioDefs: Option[Array[RatioDefinition]] = {
    expDesign.flatMap( _.groupSetupByNumber.get(groupSetupNumber).map(_.ratioDefinitions) )
  }
  
  // TODO: check this is correct
  /*lazy val mqPepByPepMatchId: LongMap[MasterQuantPeptide] = {
    val pepMatchesByPepId = resultSummary.lazyResultSet.peptideMatchesByPeptideId
    
    val longMap = new LongMap[MasterQuantPeptide](quantRSM.masterQuantPeptides.length)
    val pepMatchIdMqPepTuples = for(
      mqPep <- quantRSM.masterQuantPeptides;
      pepInst <- mqPep.peptideInstance;
      pepMatch <- pepMatchesByPepId(pepInst.peptide.id)
    ) {
      longMap += pepMatch.id -> mqPep
    }
    
    longMap
  }*/
  
  lazy val mqPepByPepId: LongMap[MasterQuantPeptide] = {
    
    val longMap = new LongMap[MasterQuantPeptide](quantRSM.masterQuantPeptides.length)
    val pepMatchIdMqPepTuples = for(
      mqPep <- quantRSM.masterQuantPeptides;
      pepInst <- mqPep.peptideInstance
    ) {
      longMap += pepInst.peptide.id -> mqPep
    }
    
    longMap
  }
  
  lazy val mqProtSetByProtSetId: LongMap[MasterQuantProteinSet] = {
    quantRSM.masterQuantProteinSets.toLongMapWith { mqProtSet => mqProtSet.proteinSet.id -> mqProtSet }
  }
  
  //lazy val mqPepIonsByMqPepId = quantRSM.masterQuantPeptideIons.groupByLong( _.masterQuantPeptideId )
  
}
