package fr.proline.core.service.msi

import java.io.File
import scala.Array.canBuildFrom
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test
import org.scalatestplus.junit.JUnitSuite
import com.typesafe.scalalogging.LazyLogging
import fr.proline.context.BasicExecutionContext
import fr.proline.core.algo.msi.TargetDecoyResultSetSplitter
import fr.proline.core.om.model.msi.PeptideMatch
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IPTMProvider
import fr.proline.core.om.provider.msi.IPeptideProvider
import fr.proline.core.om.provider.msi.IProteinProvider
import fr.proline.core.om.provider.msi.ISeqDatabaseProvider
import fr.proline.core.om.provider.msi.ResultFileProviderRegistry
import fr.proline.core.om.provider.msi.SeqDbFakeProvider
import fr.proline.core.om.provider.msi.PTMFakeProvider
import fr.proline.core.om.provider.msi.ProteinFakeProvider
import fr.proline.core.om.provider.msi.PeptideFakeProvider
import org.junit.Ignore
@Test
class TargetDecoySplitterTest extends JUnitSuite with LazyLogging { 

  private var _datFileName: String = "/dat_samples/STR_F136482_CTD.dat"
  private var absoluteDatFileNameSet = false
  def datFileName_=(value: String): Unit = {
    _datFileName = value
    absoluteDatFileNameSet = true
  }

  def datFileName = _datFileName

  private def buildFakeParserContext: ProviderDecoratedExecutionContext = {
    val executionContext = new BasicExecutionContext(1, null, null, null)

    val parserContext = ProviderDecoratedExecutionContext(executionContext) // Use Object factory

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
    val rfProvider = ResultFileProviderRegistry.get("mascot.dat")
    if (rfProvider == None)
      throw new IllegalArgumentException("No ResultFileProvider for specified identification file format")

    // Open the result file
    val parserContext = buildFakeParserContext
    
    try {     
    val resultFile = rfProvider.get.getResultFile(datFile, Map.empty, parserContext)
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

    val peptideMatches = tRs.peptideMatches ++ dRs.peptideMatches
    assertEquals(peptideMatches.size, (tRs.peptideMatches.length+ dRs.peptideMatches.length))
    var pepMatchesByMsQueryInitialId = peptideMatches.groupBy( _.msQuery.initialId )
    val nbrQueries = pepMatchesByMsQueryInitialId.size
    assertEquals(nbrQueries , 782) //Vu avec IRMa: 216 unassigned queries sur les 998...
    
      // Build peptide match joint table
    for( (msQueryInitialId, pepMatches) <- pepMatchesByMsQueryInitialId ) {
      
      // Group peptide matches by result set id
	val sortedPepMatches : Array[PeptideMatch]= pepMatches.sortBy(_.rank)
	pepMatchesByMsQueryInitialId += msQueryInitialId -> sortedPepMatches
    }
    assertEquals(nbrQueries,pepMatchesByMsQueryInitialId.size)
    assertEquals(19, pepMatchesByMsQueryInitialId.get(145).get.length )
    val pepMatchQ145 = pepMatchesByMsQueryInitialId.get(145).get.filter(_.peptide.sequence == "VAIPK")
    assertEquals(2, pepMatchQ145.size)
    assertEquals(pepMatchQ145(0).peptideId, pepMatchQ145(1).peptideId)
    } finally {
      parserContext.closeAll()
    }
    
  }
  
}