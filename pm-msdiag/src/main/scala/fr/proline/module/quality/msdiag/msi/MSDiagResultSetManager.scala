package fr.proline.module.quality.msdiag.msi

import scala.Array.canBuildFrom
import scala.Array.fallbackCanBuildFrom
import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.context.IExecutionContext
import fr.proline.core.om.model.msi.MsQuery
import fr.proline.core.om.model.msi.Ms2Query
import fr.proline.core.om.model.msi.PeptideMatch
import fr.proline.core.om.model.msi.ResultSet
import fr.proline.core.om.model.msi.Spectrum
import fr.proline.core.om.provider.msi.impl.SQLMsQueryProvider
import fr.proline.core.om.provider.msi.impl.SQLResultSetProvider
import fr.proline.core.om.provider.msi.impl.SQLSpectrumProvider

class MSDiagResultSetManager(val parserContext: IExecutionContext, val rsId: Long) extends Logging {
  
  private val msQueryProvider = new SQLMsQueryProvider(parserContext.getMSIDbConnectionContext)
  private val spectrumProvider = new SQLSpectrumProvider(parserContext.getMSIDbConnectionContext)
  private val resultSetProvider = new SQLResultSetProvider(parserContext.getMSIDbConnectionContext, parserContext.getPSDbConnectionContext, parserContext.getUDSDbConnectionContext)

  private val rsTargetOpt = resultSetProvider.getResultSet(rsId)
  if(!rsTargetOpt.isDefined) {
    throw new Exception("ResultSet with id "+rsId+" does not exist")
  }
  private val rsTarget: ResultSet = rsTargetOpt.get
  private val rsDecoyOpt: Option[ResultSet] = rsTarget.decoyResultSet

  lazy val isTargetOnly: Boolean = !rsDecoyOpt.isDefined

  lazy val getUnassignedQueries: Array[MsQuery] = msQueryProvider.getUnassignedMsQueries(if(isTargetOnly) Seq(rsTarget.id) else Seq(rsTarget.id, rsDecoyOpt.get.id), Seq(rsTarget.getMSISearchId))
  
  lazy val getAllMsQueries: Array[MsQuery] = msQueryProvider.getMsiSearchesMsQueries(Seq(rsTarget.getMSISearchId))

  lazy val getTargetPeptideMatches: Array[PeptideMatch] = rsTarget.peptideMatches
  
  lazy val getDecoyPeptideMatches: Array[PeptideMatch] = if(rsDecoyOpt.isDefined) rsDecoyOpt.get.peptideMatches else Array[PeptideMatch]()
  
  lazy val getAllPeptideMatches: Array[PeptideMatch] = getTargetPeptideMatches ++ getDecoyPeptideMatches

  lazy val getAllSpectra: Array[Spectrum] = {
    val msqIds = getAllMsQueries.filter(_.isInstanceOf[Ms2Query]).map(_.asInstanceOf[Ms2Query])
    spectrumProvider.getSpectra(msqIds.map(_.spectrumId))
  }
  
  lazy val getUnassignedSpectra: Array[Spectrum] = {
    val msqIds = getUnassignedQueries.filter(_.isInstanceOf[Ms2Query]).map(_.asInstanceOf[Ms2Query])
    spectrumProvider.getSpectra(msqIds.map(_.spectrumId))
  }
  
  lazy val getSpectraPerPeptideMatches: Map[PeptideMatch, Spectrum] = {
    // TODO: this method is not optimized, there is one request per peptidematch, there should not be more than one request !
    getAllPeptideMatches.map(pm => pm -> spectrumProvider.getSpectra(Seq(pm.msQueryId)).head).toMap
  }
}
