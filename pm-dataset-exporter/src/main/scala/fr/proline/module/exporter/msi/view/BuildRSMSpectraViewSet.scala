package fr.proline.module.exporter.msi.view

import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.serialization.{CustomDoubleJacksonSerializer, ProfiJSMSerialization}
import fr.proline.context.IExecutionContext
import fr.proline.core.dal.{DoJDBCReturningWork, DoJDBCWork}
import fr.proline.core.dal.tables.SelectQueryBuilder._
import fr.proline.core.dal.tables.SelectQueryBuilder2
import fr.proline.core.dal.tables.msi.{MsiDbObjectTreeTable, MsiDbPeptideMatchObjectTreeMapTable}
import fr.proline.core.om.model.msi.SpectrumMatch
import fr.proline.core.om.model.msq.MasterQuantPeptideIon
import fr.proline.core.om.provider.PeptideCacheExecutionContext
import fr.proline.core.om.model.msi._
import fr.proline.core.om.provider.msi.impl._
import fr.proline.core.om.provider.msq.impl.SQLMasterQuantPeptideProvider
import fr.proline.module.exporter.api.template.{ViewWithTemplate, _}
import fr.proline.module.exporter.commons.config.ExportConfig
import fr.proline.module.exporter.commons.view.ViewSet
import scala.collection.mutable._


object FormatCompatibility extends Enumeration {

  val PEAKVIEW: FormatCompatibility.Value = Value("peakview")
  val SPECTRONAUT = Value("spectronaut")

}

object BuildRSMSpectraViewSet extends LazyLogging {

  object CustomSerializer extends ProfiJSMSerialization with CustomDoubleJacksonSerializer

  def apply(ds:  IdentWithSpectrumDataSet, viewSetName: String, viewSetTemplate: IViewSetTemplate, exportConfig: ExportConfig, mode: FormatCompatibility.Value): ViewSet = {

    val templatedViews = viewSetTemplate.templatedViewTypes.map { templatedViewType =>
      val viewWithTpl = ViewWithTemplate(BuildRSMSpectraView(ds, templatedViewType.viewType, exportConfig, mode), templatedViewType.template)
      if (templatedViewType.viewName.isDefined) viewWithTpl.dataView.viewName = templatedViewType.viewName.get

      viewWithTpl
    }

    new ViewSet(viewSetName, viewSetTemplate, templatedViews, exportConfig)
  }

  def apply(
    executionContext: IExecutionContext,
    projectId: Long,
    rsmId: Long,
    viewSetName: String,
    viewSetTemplate: IViewSetTemplate,
    mode: FormatCompatibility.Value
  ): ViewSet = {

    val loadFullResultSet = false //See if needed !
    val msiSQLCtx = executionContext.getMSIDbConnectionContext

    // Load RSM and force RS loading if loadFullResultSet
    val rsmProvider = new SQLResultSummaryProvider(PeptideCacheExecutionContext(executionContext) )
    val rsm = if (!loadFullResultSet) rsmProvider.getResultSummary(rsmId, loadResultSet = true).get
    else {
      val tmpRsm = rsmProvider.getResultSummary(rsmId, loadResultSet = false).get
      val rsProvider = new SQLResultSetProvider(PeptideCacheExecutionContext(executionContext))
      tmpRsm.resultSet = rsProvider.getResultSet(tmpRsm.getResultSetId)
      tmpRsm
    }

    val isQuantiRSM = rsm.resultSet.get.isQuantified

    //************* Load PepMatch Info

    val peptideMatches = rsm.peptideInstances.filter(_.validatedProteinSetsCount > 0).flatMap(_.peptideMatches)

    val (bestPeptideMatchesByPeptideAndCharge, masterQuantPepIonByPepMatchId) = if (isQuantiRSM) {
      val masterQuantPepIonByPepMatchId = scala.collection.mutable.Map[Long, MasterQuantPeptideIon]()
      // The following map must be built manually because SQLMasterQuantPeptideProvider and ResultSummary Peptide object are different
      val peptideMatchesByPeptideAndCharge = scala.collection.mutable.Map[(Peptide, Int), PeptideMatch]()
      val peptideMatchesByPeptideIdAndCharge = peptideMatches.groupBy(pepMatch => (pepMatch.peptide.id, pepMatch.charge)).mapValues(_.maxBy(_.score))
      val masterQuantPepProvider = new SQLMasterQuantPeptideProvider(PeptideCacheExecutionContext(executionContext))
      val masterQuantPepIonsByPeptideInstance = masterQuantPepProvider.getQuantResultSummariesMQPeptides(Seq(rsm.id)).groupBy(_.peptideInstance.getOrElse(null)).mapValues(_.flatMap(_.masterQuantPeptideIons))
      for ((peptideInstance, quantPepIons) <- masterQuantPepIonsByPeptideInstance) {
        for (quantPepIon <- quantPepIons) {
          if (peptideMatchesByPeptideIdAndCharge.contains((peptideInstance.peptide.id, quantPepIon.charge))) {
            masterQuantPepIonByPepMatchId += (peptideMatchesByPeptideIdAndCharge((peptideInstance.peptide.id, quantPepIon.charge)).id -> quantPepIon)
            peptideMatchesByPeptideAndCharge += ((peptideInstance.peptide, quantPepIon.charge) -> peptideMatchesByPeptideIdAndCharge((peptideInstance.peptide.id, quantPepIon.charge)))
          }
        }
      }
      (peptideMatchesByPeptideAndCharge.toMap, Some(masterQuantPepIonByPepMatchId.toMap))
    } else {
      val peptideMatchesByPeptideAndCharge = peptideMatches.groupBy(pepMatch => (pepMatch.peptide, pepMatch.charge)).mapValues(_.maxBy(_.score))
      (peptideMatchesByPeptideAndCharge, None)
    }

    val pepMatchesAsStr = bestPeptideMatchesByPeptideAndCharge.values.map(_.id).mkString(",")

    //************* Load Spectrum Info 
    val spectrumProvider = new SQLSpectrumProvider(msiSQLCtx)
    val spectrumIdByPepMatchId = {
      val map = new HashMap[Long, Long]
      DoJDBCWork.withEzDBC(msiSQLCtx, { ezDBC =>
        ezDBC.selectAndProcess("SELECT peptide_match.id, ms_query.spectrum_id FROM peptide_match, ms_query WHERE peptide_match.ms_query_id = ms_query.id and peptide_match.id IN (" + pepMatchesAsStr + ")") { r =>
          val pepMatchId: Long = r.nextLong
          val spectrumId: Long = r.nextLong
          map += (pepMatchId -> spectrumId)
        }
      })
      Map() ++ map
    }

    logger.debug(" Found {} spectrum for {} pep matches.", spectrumIdByPepMatchId.size, bestPeptideMatchesByPeptideAndCharge.values.size)

    val spectra = spectrumProvider.getSpectra(spectrumIdByPepMatchId.values.toSeq)
    val spectrumByPepMatchID = spectrumIdByPepMatchId.map(entry => { entry._1 -> spectra.filter(_.id == entry._2).head }).toMap
    logger.debug(" spectrumByPepMatchID  " + spectrumByPepMatchID.size)

    //************* Load Spectrum Info    
    val spectrumMatchesByPeptMatchId = new HashMap[Long, SpectrumMatch]
    DoJDBCReturningWork.withEzDBC(msiSQLCtx, { msiEzDBC =>
      val pepMatchSpectrumMatchQuery = new SelectQueryBuilder2(MsiDbPeptideMatchObjectTreeMapTable, MsiDbObjectTreeTable).mkSelectQuery((pmT, pmC, otT, otC) =>
        List(pmT.PEPTIDE_MATCH_ID, otT.CLOB_DATA) -> " WHERE " ~ pmT.OBJECT_TREE_ID ~ "=" ~ otT.ID ~ " AND " ~ pmT.SCHEMA_NAME ~ "= 'peptide_match.spectrum_match' AND " ~
          pmT.PEPTIDE_MATCH_ID ~ " IN (" ~ pepMatchesAsStr ~ ")")

      msiEzDBC.selectAndProcess(pepMatchSpectrumMatchQuery) { r =>
        val id = r.nextLong
        val spectrumMatch = CustomSerializer.deserialize[SpectrumMatch](r.nextString)
        spectrumMatchesByPeptMatchId += (id -> spectrumMatch)
      }
    })

    logger.debug(" Found {} Spectrum Matches for {} pep matches.", spectrumMatchesByPeptMatchId.size, bestPeptideMatchesByPeptideAndCharge.values.size)
    if(spectrumMatchesByPeptMatchId.size < bestPeptideMatchesByPeptideAndCharge.values.size ) // or if(spectrumMatchesByPeptMatchId.isEmpty()) ??
      throw new RuntimeException("No annotation found for peptide matches'spectrum, run first Generate Spectrum Matches")

    return apply(IdentWithSpectrumDataSet( rsm, bestPeptideMatchesByPeptideAndCharge, spectrumByPepMatchID, spectrumMatchesByPeptMatchId, masterQuantPepIonByPepMatchId ), viewSetName, viewSetTemplate, null, mode)

  }
     
}
