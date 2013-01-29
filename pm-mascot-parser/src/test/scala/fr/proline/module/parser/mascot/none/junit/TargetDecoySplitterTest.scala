package fr.proline.module.parser.mascot.none.junit

import java.io.File
import com.weiglewilczek.slf4s.Logging
import fr.proline.core.algo.msi.TargetDecoyResultSetSplitter
import fr.proline.core.om.provider.msi.ResultFileProviderRegistry
import fr.proline.module.parser.provider.fake._
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.context.BasicExecutionContext
import fr.proline.core.om.provider.msi.ISeqDatabaseProvider
import fr.proline.core.om.provider.msi.IProteinProvider
import fr.proline.core.om.provider.msi.IPTMProvider
import fr.proline.core.om.provider.msi.IPeptideProvider

object TargetDecoySplitterTest extends Logging { 

  private var _datFileName: String = "/F047876.dat"
  private var absoluteDatFileNameSet = false
  def datFileName_=(value: String): Unit = {
    _datFileName = value
    absoluteDatFileNameSet = true
  }

  def datFileName = _datFileName

  private def buildFakeParserContext: ProviderDecoratedExecutionContext = {
    val executionContext = new BasicExecutionContext(null, null, null, null, null)

    val parserContext = new ProviderDecoratedExecutionContext(executionContext)

    parserContext.putProvider(classOf[IPeptideProvider], PeptideFakeProvider)
    parserContext.putProvider(classOf[IPTMProvider], PTMFakeProvider)
    parserContext.putProvider(classOf[IProteinProvider], ProteinFakeProvider)
    parserContext.putProvider(classOf[ISeqDatabaseProvider], SeqDbFakeProvider)

    parserContext
  }

  def runService(): Unit = {

    logger.info(" --- Get File " + datFileName)

    var datFile: File = null

    if (absoluteDatFileNameSet)
      datFile = new File(datFileName)
    else
      datFile = new File(TargetDecoySplitterTest.getClass.getResource(datFileName).toURI)

    logger.info(" --- TargetDecoySplitter  " + datFile.exists)

    // Get Right ResultFile provider
    val rfProvider = ResultFileProviderRegistry.get("MascotMSParser")
    if (rfProvider == None)
      throw new IllegalArgumentException("No ResultFileProvider for specified identification file format")

    // Open the result file
    val resultFile = rfProvider.get.getResultFile(datFile, Map.empty, buildFakeParserContext)
    val rs = resultFile.getResultSet(false)

    // Build regex matching decoy accession numbers
    val acDecoyRegex = """###REV###.+""".r

    val (tRs, dRs) = TargetDecoyResultSetSplitter.split(rs, acDecoyRegex)
    logger.debug("number of target protein matches = " + tRs.proteinMatches.length)
    logger.debug("number of decoy protein matches  = " + dRs.proteinMatches.length)
    logger.debug("number of target peptide matches = " + tRs.peptideMatches.length)
    logger.debug("number of decoy peptide matches  = " + dRs.peptideMatches.length)
    logger.debug("number of target peptides        = " + tRs.peptides.length)
    logger.debug("number of decoy peptides         = " + dRs.peptides.length)

  }

  def main(args: Array[String]): Unit = {
    System.out.println(" --- You should have correctly modified the UDS db configuration in /db_uds.properties !! --- ")
    //System.out.println(" Press any key to continue ...")
    //System.in.read()

    logger.debug("Start Logging Debug...TargetDecoySplitterTest")

    if (args.length > 0)
      TargetDecoySplitterTest.datFileName = args.apply(0)

    TargetDecoySplitterTest.runService
  }

}