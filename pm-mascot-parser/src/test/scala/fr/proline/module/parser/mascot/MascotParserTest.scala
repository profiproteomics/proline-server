package fr.proline.module.parser.mascot

import java.io.File
import java.util.Calendar
import org.hamcrest.CoreMatchers
import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertThat
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Ignore
import org.junit.Test
import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.context.BasicExecutionContext
import fr.proline.context.DatabaseConnectionContext
import fr.proline.core.om.model.msi.PeptideMatch
import fr.proline.core.om.model.msi.ResultSet
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IProteinProvider
import fr.proline.core.om.provider.msi.ISeqDatabaseProvider
import fr.proline.module.parser.provider.fake.ProteinFakeProvider
import fr.proline.module.parser.provider.fake.SeqDbFakeProvider
import fr.proline.repository.ProlineDatabaseType
import fr.proline.repository.util.DatabaseTestCase
import fr.proline.core.om.provider.msi.impl.SQLPeptideProvider

@Test
class MascotParserTest extends Logging { // }extends DatabaseTestCase {

  var ecoli_datFileName: String = "/dat_samples/GRE_F068213_M2.4_TD_EColi.dat"

  var file: File = null

  var psDBTestCase: DatabaseTestCase = null

  @Before
  def init() {
    logger.info("Start Logging ")
    logger.debug("Start Logging Debug ")

    // Init PS db connexion
    psDBTestCase = new PSDatabaseTestCase()
    psDBTestCase.initDatabase()
    psDBTestCase.loadDataSet("/fr/proline/module/parser/mascot/Unimod_Dataset.xml")
  }

  @After
  def closeResources() {

    if (psDBTestCase != null) {
      psDBTestCase.tearDown()
    }
    SQLPeptideProvider.clear()
  }

  @Test
  def testReadDatFile() = {
    val psDbCtx = new DatabaseConnectionContext(psDBTestCase.getConnector)

    val executionContext = new BasicExecutionContext(null, null, psDbCtx, null, null)

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

    } finally {
      executionContext.closeAll()
    }

  }

  private def testEColiDescription(rs: ResultSet) = {

    val msiSearchDate = Calendar.getInstance()
    val msiSearch = rs.msiSearch.get
    msiSearchDate.setTime(msiSearch.date)

    val expectedDate = Calendar.getInstance()
    expectedDate.set(2013, 01, 05)
    assertThat(msiSearchDate.get(Calendar.MONTH), CoreMatchers.equalTo(expectedDate.get(Calendar.MONTH)))
    assertThat(msiSearchDate.get(Calendar.DAY_OF_MONTH), CoreMatchers.equalTo(expectedDate.get(Calendar.DAY_OF_MONTH)))
    assertThat(msiSearchDate.get(Calendar.YEAR), CoreMatchers.equalTo(expectedDate.get(Calendar.YEAR)))

    assertThat(msiSearch.jobNumber, CoreMatchers.equalTo(68213))
    assertThat(msiSearch.queriesCount, CoreMatchers.equalTo(7047))
    assertThat(msiSearch.submittedQueriesCount, CoreMatchers.equalTo(7047))
    assertThat(msiSearch.resultFileName, CoreMatchers.equalTo("GRE_F068213_M2.4_TD_EColi.dat"))
    assertThat(msiSearch.searchedSequencesCount, CoreMatchers.equalTo(32182))
    assertThat(msiSearch.title, CoreMatchers.equalTo("K12 Test nano trap duree gradient T12 HCD QEx1_000192.raw (DH5_50)"))
    assertThat(msiSearch.userName, CoreMatchers.equalTo("AMH"))

    assertThat(msiSearch.searchSettings.fixedPtmDefs.length, CoreMatchers.equalTo(1))
    assertThat(msiSearch.searchSettings.variablePtmDefs.length, CoreMatchers.equalTo(2))
    assertThat(msiSearch.searchSettings.variablePtmDefs(0).names.shortName, CoreMatchers.equalTo("Acetyl"))
    assertThat(msiSearch.searchSettings.variablePtmDefs(0).residue, CoreMatchers.equalTo('\0'))
    assertThat(msiSearch.searchSettings.variablePtmDefs(1).names.shortName, CoreMatchers.equalTo("Oxidation"))
    assertThat(msiSearch.searchSettings.variablePtmDefs(1).residue, CoreMatchers.equalTo('M'))
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

  class PSDatabaseTestCase extends DatabaseTestCase {
    override def getProlineDatabaseType() = ProlineDatabaseType.PS
  }

  class PDIDatabaseTestCase extends DatabaseTestCase {
    override def getProlineDatabaseType() = ProlineDatabaseType.PDI
  }

}