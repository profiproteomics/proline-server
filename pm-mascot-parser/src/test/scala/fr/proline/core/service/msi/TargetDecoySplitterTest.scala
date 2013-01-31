package fr.proline.core.service.msi

import org.junit.Assert._
import org.junit.After
import org.junit.Before
import org.junit.Test
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
import org.scalatest.junit.{ JUnitRunner, JUnitSuite }

@Test
class TargetDecoySplitterTest extends JUnitSuite with Logging { 

  private var _datFileName: String = "/dat_samples/STR_F136482_CTD.dat"
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

  @Test
  def testSplit()  {

    logger.info(" --- Get File " + datFileName)

    var datFile: File = null

    if (absoluteDatFileNameSet)
      datFile = new File(datFileName)
    else
      datFile = new File(TargetDecoySplitterTest.this.getClass.getResource(datFileName).toURI)

    logger.info(" --- TargetDecoySplitter  " + datFile.exists)

    // Get Right ResultFile provider
    val rfProvider = ResultFileProviderRegistry.get("MascotMSParser")
    if (rfProvider == None)
      throw new IllegalArgumentException("No ResultFileProvider for specified identification file format")

    // Open the result file
    val resultFile = rfProvider.get.getResultFile(datFile, Map.empty, buildFakeParserContext)
    val rs = resultFile.getResultSet(false)

    // Build regex matching decoy accession numbers
    val acDecoyRegex = """sp\|REV_\S+""".r

    val (tRs, dRs) = TargetDecoyResultSetSplitter.split(rs, acDecoyRegex)

    assertEquals(tRs.decoyResultSet.get, dRs)
    assertEquals(rs.proteinMatches.length, (tRs.proteinMatches.length + dRs.proteinMatches.length))    

    for (pepMatch <- dRs.peptideMatches) {
      assertTrue(pepMatch.isDecoy)
    }

    for (seqMatch <- dRs.proteinMatches.flatMap(_.sequenceMatches)) {
      assertTrue(seqMatch.isDecoy)
      assertEquals(true, seqMatch.bestPeptideMatch.get.isDecoy)
    }

    logger.debug("number of target protein matches = " + tRs.proteinMatches.length)
    logger.debug("number of decoy protein matches  = " + dRs.proteinMatches.length)
    logger.debug("number of target peptide matches = " + tRs.peptideMatches.length)
    logger.debug("number of decoy peptide matches  = " + dRs.peptideMatches.length)
    logger.debug("number of target peptides        = " + tRs.peptides.length)
    logger.debug("number of decoy peptides         = " + dRs.peptides.length)

  }


}