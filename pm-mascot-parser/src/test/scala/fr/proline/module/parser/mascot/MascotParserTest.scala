package fr.proline.module.parser.mascot

import java.io.File
import java.util.Calendar

import com.typesafe.scalalogging.LazyLogging
import fr.proline.context.{BasicExecutionContext, MsiDbConnectionContext}
import fr.proline.core.dal.MSIDatabaseTestCase
import fr.proline.core.dbunit.JInit_Dataset
import fr.proline.core.om.model.msi.{PeptideMatch, ResultSet}
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.{IProteinProvider, ISeqDatabaseProvider, ProteinFakeProvider, SeqDbFakeProvider}
import fr.proline.repository.DriverType
import fr.proline.repository.util.DatabaseTestCase
import org.junit.Assert.{assertEquals, assertNotNull, assertTrue}
import org.junit.{After, Before, Test}

@Test
class MascotParserTest extends LazyLogging { // }extends DatabaseTestCase {

  var ecoli_datFileName: String = "/dat_samples/GRE_F068213_M2.4_TD_EColi.dat"

  var file: File = null

  var msiDBTestCase: DatabaseTestCase = null

  @Before
  def init() {
    logger.info("Start Logging ")
    logger.debug("Start Logging Debug ")

    // Init PS db connexion
    msiDBTestCase = new MSIDatabaseTestCase(DriverType.H2)
    msiDBTestCase.initDatabase()
    msiDBTestCase.loadDataSet(JInit_Dataset.msiDbDatasetPath)
  }

  @After
  def closeResources() {

    if (msiDBTestCase != null) {
      msiDBTestCase.tearDown()
    }
    
  }

  @Test
  def testVersion {
    val version = new fr.proline.module.parser.mascot.Version()
    logger.debug("Module name : " + version.getModuleName)
    assert("PM-MascotParser" == version.getModuleName)
    logger.debug("Module version : " + version.getVersion)
    logger.debug("TEST [testVersion] OK: versionning is successful")
  }

  @Test
  def testReadDatFile() = {
    val msiDbCtx = new MsiDbConnectionContext(msiDBTestCase.getConnector)

    val executionContext = new BasicExecutionContext(1,null, msiDbCtx, null)

    try {
      val parserContext = ProviderDecoratedExecutionContext(executionContext) // Use Object factory

      parserContext.putProvider(classOf[IProteinProvider], ProteinFakeProvider)
      parserContext.putProvider(classOf[ISeqDatabaseProvider], SeqDbFakeProvider)

      val f = new File(this.getClass().getResource(ecoli_datFileName).toURI())
      val propertiesBuilder = Map.newBuilder[String, Any]
      propertiesBuilder += (MascotParseParams.ION_SCORE_CUTOFF.toString -> 0.5)
      val mascotDatFile = new MascotResultFile(f, propertiesBuilder.result, parserContext);
      val rs: ResultSet = mascotDatFile.getResultSet(wantDecoy = false)

      //Test Search Param info
      testEColiDescription(rs)

      //Test PTM Providers worked well 
      testEColiPeptidePtms(rs)
      assertNotNull(rs)
      // --> VD TODO: Estimate # peptideMatch & # proteinMatches
      assertEquals(1687, rs.peptideMatches.length)
      assertEquals(2519, rs.proteinMatches.length)

      rs.proteinMatches.foreach(protMatch => {
        assertTrue(protMatch.score > 0)
        assertTrue(protMatch.peptideMatchesCount > 0)
      })
      
       mascotDatFile.eachSpectrum( spectrum => { 
         assertTrue(spectrum.firstTime > 0) 
         assertTrue(spectrum.lastTime > 0)
       })

    } finally {
      executionContext.closeAll()
    }

  }

  private def testEColiDescription(rs: ResultSet) = {

    val msiSearchDate = Calendar.getInstance()
    val msiSearch = rs.msiSearch.get
    msiSearchDate.setTime(msiSearch.date)

    val expectedDate = Calendar.getInstance()
    expectedDate.set(2013, 1, 5)
    assertEquals(msiSearchDate.get(Calendar.MONTH),expectedDate.get(Calendar.MONTH))
    assertEquals(msiSearchDate.get(Calendar.DAY_OF_MONTH),expectedDate.get(Calendar.DAY_OF_MONTH))
    assertEquals(msiSearchDate.get(Calendar.YEAR),expectedDate.get(Calendar.YEAR))

    assertEquals(msiSearch.jobNumber, 68213)
    assertEquals(msiSearch.queriesCount,7047)
    assertEquals(msiSearch.resultFileName, "GRE_F068213_M2.4_TD_EColi.dat")
    assertEquals(msiSearch.searchedSequencesCount, 32182)
    assertEquals(msiSearch.title, "K12 Test nano trap duree gradient T12 HCD QEx1_000192.raw (DH5_50)")
    assertEquals(msiSearch.userName,"AMH")

    assertEquals(msiSearch.searchSettings.fixedPtmDefs.length, 1)
    assertEquals(msiSearch.searchSettings.variablePtmDefs.length, 2)
    assertEquals(msiSearch.searchSettings.variablePtmDefs(0).names.shortName, "Acetyl")
    assertEquals(msiSearch.searchSettings.variablePtmDefs(0).residue, '\u0000')
    assertEquals(msiSearch.searchSettings.variablePtmDefs(1).names.shortName, "Oxidation")
    assertEquals(msiSearch.searchSettings.variablePtmDefs(1).residue, 'M')
  }

  private def testEColiPeptidePtms(rs: ResultSet) = {

    //Test PTM Providers worked well 
    val allPepMatches: Array[PeptideMatch] = rs.peptideMatches

    TestUtils.savePeptideMatches(allPepMatches)

    val PtmShortNames = Array[String]("Acetyl", "Oxidation", "Carbamidomethyl")
    allPepMatches filter (!_.peptide.ptms.isEmpty) foreach { pepMatch =>
      //	     logger.debug(" PepPtm \t" + pepMatch.peptide.sequence+"\t"+pepMatch.msQuery.initialId+"\t"+pepMatch.rank+"\t"+pepMatch.peptide.ptmString)
      val pepPtms: Array[fr.proline.core.om.model.msi.LocatedPtm] = pepMatch.peptide.ptms
      pepPtms.foreach { locatedPtm =>
        assertTrue(PtmShortNames.contains(locatedPtm.definition.names.shortName))
        if (locatedPtm.definition.names.shortName.equals(PtmShortNames(0)))
          assertTrue(locatedPtm.isNTerm)
        else if (locatedPtm.definition.names.shortName.equals(PtmShortNames(1))) {
          assertTrue(locatedPtm.definition.residue.equals('M'))
          var indexB = Seq.newBuilder[Int]
          for (k <- 0 until pepMatch.peptide.sequence.length) {
            val nextChar = pepMatch.peptide.sequence.charAt(k)
            if (nextChar.equals('M'))
              indexB += k + 1
          }
          assertTrue(indexB.result.contains(locatedPtm.seqPosition))
        } else {
          assertTrue(locatedPtm.definition.residue.equals('C'))
          var indexB = Seq.newBuilder[Int]
          for (k <- 0 until pepMatch.peptide.sequence.length) {
            val nextChar = pepMatch.peptide.sequence.charAt(k)
            if (nextChar.equals('C'))
              indexB += k + 1
          }
          assertTrue(indexB.result.contains(locatedPtm.seqPosition))
        }
      }
    }

    allPepMatches filter (nextPepMatch => nextPepMatch.msQuery.initialId.equals(385) && nextPepMatch.rank == 1) foreach { pepMatch =>
      //	   	 logger.debug(" --------------- QUERY 385 !! "+pepMatch.peptide.sequence+ " --- " +pepMatch.peptide.ptms(0).definition.names.shortName)
      assertEquals("Oxidation", pepMatch.peptide.ptms(0).definition.names.shortName)
    }

  }

}