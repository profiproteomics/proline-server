package fr.proline.module.fragment_match

import scala.Array.canBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Assert.fail
import org.junit.Before
import org.junit.Test
import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.context.BasicExecutionContext
import fr.proline.context.IExecutionContext
import fr.proline.core.dal.AbstractMultipleDBTestCase
import fr.proline.core.dal.ContextFactory
import fr.proline.core.om.model.msi.Peptide
import fr.proline.core.om.model.msi.ResultSet
import fr.proline.core.om.model.msi.TheoreticalFragmentSeries
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IPTMProvider
import fr.proline.core.om.provider.msi.IPeptideProvider
import fr.proline.core.om.provider.msi.IResultSetProvider
import fr.proline.core.om.provider.msi.impl.SQLPTMProvider
import fr.proline.core.om.provider.msi.impl.SQLPeptideProvider
import fr.proline.core.om.provider.msi.impl.SQLResultSetProvider
import fr.proline.repository.DriverType

class FragmentIonTableTest extends AbstractMultipleDBTestCase with Logging {

  // Define the interface to be implemented
  val driverType = DriverType.H2
  val fileName = "GRE_F068213_M2.4_TD_EColi"
  val targetRSId = 2
  val decoyRSId = Option.empty[Int]

  var executionContext: IExecutionContext = null
  var rsProvider: IResultSetProvider = null
  protected var readRS: ResultSet = null

  @Before
  @throws(classOf[Exception])
  def setUp() = {

    logger.info("Initializing DBs")
    super.initDBsDBManagement(driverType)

    //Load Data
    pdiDBTestCase.loadDataSet("/dbunit/datasets/pdi/Proteins_Dataset.xml")
    psDBTestCase.loadDataSet("/dbunit_samples/" + fileName + "/ps-db.xml")
    msiDBTestCase.loadDataSet("/dbunit_samples/" + fileName + "/msi-db.xml")
    udsDBTestCase.loadDataSet("/dbunit_samples/" + fileName + "/uds-db.xml")

    logger.info("PDI, PS, MSI and UDS dbs succesfully initialized !")

    val (execContext, rsProv) = buildSQLContext()
    executionContext = execContext
    rsProvider = rsProv
    readRS = this._loadRS()
  }

  @After
  override def tearDown() {
    if (executionContext != null) executionContext.closeAll()
    super.tearDown()
  }

  private def _loadRS(): ResultSet = {
    val rs = rsProvider.getResultSet(targetRSId).get
    // SMALL HACK because of DBUNIT BUG (see bioproj defect #7548)
    if (decoyRSId.isDefined) rs.decoyResultSet = rsProvider.getResultSet(decoyRSId.get)
    rs
  }

  def buildSQLContext() = {
    val udsDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getUdsDbConnector, false)
    val pdiDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getPdiDbConnector, true)
    val psDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getPsDbConnector, false)
    val msiDbCtx = ContextFactory.buildDbConnectionContext(dsConnectorFactoryForTest.getMsiDbConnector(1), false)
    val executionContext = new BasicExecutionContext(udsDbCtx, pdiDbCtx, psDbCtx, msiDbCtx, null)
    val parserContext = ProviderDecoratedExecutionContext(executionContext) // Use Object factory

    parserContext.putProvider(classOf[IPeptideProvider], new SQLPeptideProvider(psDbCtx))
    parserContext.putProvider(classOf[IPTMProvider], new SQLPTMProvider(psDbCtx))

    val rsProvider = new SQLResultSetProvider(msiDbCtx, psDbCtx, udsDbCtx)

    (parserContext, rsProvider)
  }

  @Test
  def testSimpleFragmentationTest() = {

    val peptideSequence = "QVGVPYIIVFLNK"
    val peptide = new Peptide(
      id = Peptide.generateNewId,
      sequence = peptideSequence,
      ptms = null,
      calculatedMass = Peptide.calcMass(peptideSequence))

    val currentFragmentIonTypes = new FragmentIons(ionTypeB = true, ionTypeY = true, chargeForIonsB = 2, chargeForIonsY = 2)

    val table = new FragmentIonTable(peptide, currentFragmentIonTypes)

    logger.debug(table.toString)
  }

  @Test
  def fragmentationTest() = {
    val peptide = readRS.getPeptideById()(931)
    val expected = readCSV("/"+peptide.sequence+".csv")
    logger.debug(peptide.sequence + " " + peptide.readablePtmString)
    val peptideMatch = readRS.peptideMatches.find { _.peptideId == peptide.id }.get
    logger.debug("Query = " + peptideMatch.msQuery.initialId)
    val currentFragmentIonTypes = new FragmentIons(ionTypeB = true, ionTypeY = true, chargeForIonsB = 2, chargeForIonsY = 2)
    val table = new FragmentIonTable(peptide, currentFragmentIonTypes)
    logger.debug(table.toString)
    compareTheoreticalFragments(expected, table.get)
  }

  @Test
  def fragmentationTestNL() = {
    val peptide = readRS.getPeptideById()(363)
    val expected = readCSV("/"+peptide.sequence+".csv")
    logger.debug(peptide.sequence + " " + peptide.readablePtmString)
    val ptm = peptide.ptms(0)
    logger.debug(ptm.definition.neutralLosses.map(_.toString).mkString(" "))
    val peptideMatch = readRS.peptideMatches.find { _.peptideId == peptide.id }.get
    logger.debug("Query = " + peptideMatch.msQuery.initialId)
    val currentFragmentIonTypes = new FragmentIons(ionTypeB = true, ionTypeY = true, chargeForIonsB = 2, chargeForIonsY = 2)
    val table = new FragmentIonTable(peptide, currentFragmentIonTypes, ptmNeutralLosses = Some(Map((ptm, ptm.definition.neutralLosses(1).monoMass))))
    logger.debug(table.toString)
    compareTheoreticalFragments(expected, table.get)
  }

  @Test
  def fragmentationTestNTerm() = {
    val peptide = readRS.getPeptideById()(614)
    val expected = readCSV("/"+peptide.sequence+".csv")
    logger.debug(peptide.sequence + " " + peptide.readablePtmString)
    val ptm = peptide.ptms(0)
    val peptideMatch = readRS.peptideMatches.find { _.peptideId == peptide.id }.get
    logger.debug("Query = " + peptideMatch.msQuery.initialId)
    val currentFragmentIonTypes = new FragmentIons(ionTypeB = true, ionTypeY = true, chargeForIonsB = 2, chargeForIonsY = 2)
    val table = new FragmentIonTable(peptide, currentFragmentIonTypes)
    logger.debug(table.toString)
	compareTheoreticalFragments(expected, table.get)
  }

  @Test
  def fragmentationTest2Ox() = {
        
    val decoyRS = rsProvider.getResultSet(1).get
    val peptide = decoyRS.getPeptideById()(70)
    val expected = readCSV("/"+peptide.sequence+".csv")
    val ptm0 = peptide.ptms(0)
    val ptm1 = peptide.ptms(1)
    val peptideMatch = decoyRS.peptideMatches.find { _.peptideId == peptide.id }.get
    logger.debug("Query = " + peptideMatch.msQuery.initialId)
    val currentFragmentIonTypes = new FragmentIons(ionTypeB = true, ionTypeY = true, chargeForIonsB = 2, chargeForIonsY = 2)
    val table = new FragmentIonTable(peptide, currentFragmentIonTypes,
      ptmNeutralLosses = Some(Map((ptm0, ptm0.definition.neutralLosses(1).monoMass),
        (ptm1, ptm1.definition.neutralLosses(1).monoMass))))
    logger.debug(table.toString)
    val theoFragments = table.fragments.map(_._2).flatten
    for (fragment <- theoFragments) {
      assert(fragment.series.isDefined)
    }
    compareTheoreticalFragments(expected, table.get)    
  }

  def compareTheoreticalFragments(expected: Array[TheoreticalFragmentSeries], actual: Array[TheoreticalFragmentSeries] ) = {
    assertEquals(expected.length, actual.length)
    for (expectedSerie <- expected) {
      val actualSerie = actual.find(_.fragSeries == expectedSerie.fragSeries)
      if (!actualSerie.isDefined) fail("series "+expectedSerie.fragSeries+" not found")
      logger.debug("comparing series "+expectedSerie.fragSeries)
      (expectedSerie.masses, actualSerie.get.masses).zipped foreach { (e,a) =>
    	  assertEquals(e, a, 0.001)
      }
    }
  }
  
  @Test
  def fragmentationAllPeptideTest() = {
    val start = System.currentTimeMillis()
    for (peptide <- readRS.peptides) {
      val currentFragmentIonTypes = new FragmentIons(ionTypeB = true, ionTypeY = true, chargeForIonsB = 2, chargeForIonsY = 2)
      val table = new FragmentIonTable(peptide, currentFragmentIonTypes)
    }
    logger.debug("Done in " + (System.currentTimeMillis() - start) + "ms for " + readRS.peptides.length + " peptides")
  }

  def readCSV(filename: String): Array[TheoreticalFragmentSeries] = {
    val src = Source.fromURL(getClass().getResource(filename))
    val iter = src.getLines().map(_.split(",", -1))
    val fragmentIonTable = new ArrayBuffer[TheoreticalFragmentSeries]
    iter.next().foreach { s:String => fragmentIonTable += new TheoreticalFragmentSeries(s, new Array[Double](0))  }
    try {
      iter foreach { item =>  
        for (i <- 0 until item.length) {
        	fragmentIonTable(i).masses = fragmentIonTable(i).masses :+ { if (!item(i).isEmpty()) item(i).toDouble else 0.0 }
        }
      }
    } catch {
      case e: Exception => logger.error("cannot create ArrayBuffer[TheoreticalFragmentSeries] from file "+filename, e)
    }
    src.close()
    fragmentIonTable.toArray
  }
}