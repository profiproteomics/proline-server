package fr.proline.module.quality.msdiag.msi

import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.core.om.model.msi.PeptideMatch
import fr.proline.core.om.model.msi.MsQuery
import fr.proline.core.om.model.msi.ResultSet
import scala.collection.mutable.ArrayBuffer
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IMsQueryProvider
import fr.proline.core.om.provider.msi.IResultSetProvider

class MSDiagResultSetManager(val parserContext: ProviderDecoratedExecutionContext, val rsId: Long) extends Logging {
  
  private val msQueryProvider = parserContext.getProvider(classOf[IMsQueryProvider])
  private val resultSetProvider = parserContext.getProvider(classOf[IResultSetProvider])

  private val rsTargetOpt = resultSetProvider.getResultSet(rsId)
  if(!rsTargetOpt.isDefined) {
    throw new Exception("ResultSet with id "+rsId+" does not exist")
  }
  private val rsTarget: ResultSet = rsTargetOpt.get
  private val rsDecoyOpt: Option[ResultSet] = rsTarget.decoyResultSet
  
  def isTargetOnly: Boolean = !rsDecoyOpt.isDefined

  def getUnassignedQueries: Array[MsQuery] = msQueryProvider.getUnassignedMsQueries(if(isTargetOnly) Seq(rsTarget.id) else Seq(rsTarget.id, rsDecoyOpt.get.id), Seq(rsTarget.getMSISearchId))
  
  def getAllMsQueries: Array[MsQuery] = msQueryProvider.getMsiSearchesMsQueries(Seq(rsTarget.getMSISearchId))

  def getTargetPeptideMatches: Array[PeptideMatch] = rsTarget.peptideMatches
  
  def getDecoyPeptideMatches: Array[PeptideMatch] = if(rsDecoyOpt.isDefined) rsDecoyOpt.get.peptideMatches else Array[PeptideMatch]()
  
  def getAllPeptideMatches: Array[PeptideMatch] = getTargetPeptideMatches ++ getDecoyPeptideMatches

}