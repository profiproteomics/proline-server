package fr.proline.module.fragmentmatch

import com.typesafe.scalalogging.StrictLogging
import fr.proline.context.BasicExecutionContext
import fr.proline.context.IExecutionContext
import fr.proline.core.dal.AbstractMultipleDBTestCase
import fr.proline.core.dal.BuildDbConnectionContext
import fr.proline.core.dal.BuildMsiDbConnectionContext
import fr.proline.core.dal.BuildUdsDbConnectionContext
import fr.proline.core.om.model.msi.{LocatedPtm, Peptide, ResultSet, TheoreticalFragmentSeries}
import fr.proline.core.om.provider.PeptideCacheExecutionContext
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IPTMProvider
import fr.proline.core.om.provider.msi.IPeptideProvider
import fr.proline.core.om.provider.msi.IResultSetProvider
import fr.proline.core.om.provider.msi.impl.SQLPTMProvider
import fr.proline.core.om.provider.msi.impl.SQLPeptideProvider
import fr.proline.core.om.provider.msi.impl.SQLResultSetProvider
import fr.proline.core.orm.msi.PtmSpecificity.PtmLocation
import fr.proline.repository.DriverType
import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Assert.fail
import org.junit.Before
import org.junit.Test

import scala.Array.canBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class FragmentIonTableTest extends AbstractMultipleDBTestCase with StrictLogging {

  // Define the interface to be implemented
  val driverType = DriverType.H2
  val fileName = "GRE_F068213_M2.4_TD_EColi"
  val targetRSId = 2
  val decoyRSId = Option.empty[Int]

  var executionContext: IExecutionContext = _
  var rsProvider: IResultSetProvider = _
  protected var readRS: ResultSet = _

  @Before
  @throws(classOf[Exception])
  def setUp(): Unit = {

    logger.info("Initializing DBs")
    super.initDBsDBManagement(driverType)

    //Load Data
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

  def buildSQLContext(): (ProviderDecoratedExecutionContext, SQLResultSetProvider) = {
    val udsDbCtx = BuildUdsDbConnectionContext(dsConnectorFactoryForTest.getUdsDbConnector(), useJPA = false)
    val msiDbCtx = BuildMsiDbConnectionContext(dsConnectorFactoryForTest.getMsiDbConnector(1), useJPA = false)
    val executionContext = PeptideCacheExecutionContext(new BasicExecutionContext(1, udsDbCtx, msiDbCtx, null))
    val parserContext = ProviderDecoratedExecutionContext(executionContext) // Use Object factory

    parserContext.putProvider(classOf[IPeptideProvider], new SQLPeptideProvider(PeptideCacheExecutionContext(parserContext)))
    parserContext.putProvider(classOf[IPTMProvider], new SQLPTMProvider(msiDbCtx))

    val rsProvider = new SQLResultSetProvider(PeptideCacheExecutionContext(parserContext))

    (parserContext, rsProvider)
  }

  @Test
  def testSimpleFragmentationTest(): Unit = {

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
  def testSimpleFragmentationTestZSerie(): Unit = {

    val peptideSequence = "VHISEPEPEVKHESLLEK"
    val peptide = new Peptide(
      id = Peptide.generateNewId,
      sequence = peptideSequence,
      ptms = null,
      calculatedMass = Peptide.calcMass(peptideSequence))
    val currentFragmentIonTypes = new FragmentIons(ionTypeC = true, ionTypeY = true, ionTypeZ = true, chargeForIonsC = 2, chargeForIonsY = 2, chargeForIonsZ = 2)
    val table = new FragmentIonTable(peptide, currentFragmentIonTypes)
    logger.debug(table.toString)
  }

  @Test
  def fragmentationTest(): Unit = {
    val peptide = readRS.getPeptideById()(381) //new ID after dbunit xml new generation. WAS 314  SEQUENCE="MVVTLIHPIAMDDGLR" PTM_STRING="11[O]"
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
  def fragmentationTestNL(): Unit = {
    val peptide = readRS.getPeptideById()(543) //WAS 943 : SEQUENCE="GEPAEFPVNGVIPGWIEALTLMPVGSK" PTM_STRING="22[O]"
    val expected = readCSV("/"+peptide.sequence+".csv")//new ID after dbunit xml new generation
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
  def fragmentationTestNTerm(): Unit = {
    val peptide = readRS.getPeptideById()(879) //WAS 701 SEQUENCE="SELSQLSPQPLWDIFAK" PTM_STRING="0[C(2) H(2) O]"
    val expected = readCSV("/"+peptide.sequence+".csv")//new ID after dbunit xml new generation
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
  def fragmentationTest2Ox(): Unit = {
        
    val decoyRS = rsProvider.getResultSet(1).get
    val peptide = decoyRS.getPeptideById()(72)//new ID after dbunit xml new generation WAS 140 SEQUENCE="SGMVIAVRAPNNRSSM" PTM_STRING="3[O]16[O]"
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

  @Test
  def manual(): Unit = {
    val ptmProvider = executionContext.asInstanceOf[ProviderDecoratedExecutionContext].getProvider(classOf[IPTMProvider])
    val pDef = ptmProvider.getPtmDefinition(52L).get
    val oDef = ptmProvider.getPtmDefinition(94L).get
    val s3 = new LocatedPtm(pDef, 3, pDef.ptmEvidences(0).monoMass, pDef.ptmEvidences(0).averageMass, pDef.ptmEvidences(0).composition)
    val s4 = new LocatedPtm(pDef, 4, pDef.ptmEvidences(0).monoMass, pDef.ptmEvidences(0).averageMass, pDef.ptmEvidences(0).composition)
    val m13 = new LocatedPtm(oDef, 13, oDef.ptmEvidences(0).monoMass, oDef.ptmEvidences(0).averageMass, oDef.ptmEvidences(0).composition)
    val ptms = Array(s3, s4, m13)

    val peptide = new Peptide(-1L, "SASSATACTSGVMTR", "", ptms , 1661.5828)
    val currentFragmentIonTypes = new FragmentIons(ionTypeB = true, ionTypeY = true, chargeForIonsB = 2, chargeForIonsY = 2)
    val table = new FragmentIonTable(peptide, currentFragmentIonTypes, ptmNeutralLosses = Some(Map((s3, s3.definition.neutralLosses(1).monoMass),
      (s4, s4.definition.neutralLosses(1).monoMass), (m13, m13.definition.neutralLosses(0).monoMass) )))

    logger.debug(table.toString)
  }

  def compareTheoreticalFragments(expected: Array[TheoreticalFragmentSeries], actual: Array[TheoreticalFragmentSeries] ): Unit = {
    assertEquals(expected.length, actual.length)
    for (expectedSerie <- expected) {
      val actualSerie = actual.find(_.fragSeries == expectedSerie.fragSeries)
      if (actualSerie.isEmpty) fail("series "+expectedSerie.fragSeries+" not found")
      logger.debug("comparing series "+expectedSerie.fragSeries)
      (expectedSerie.masses, actualSerie.get.masses).zipped foreach { (e,a) =>
    	  assertEquals(e, a, 0.001)
      }
    }
  }
  
  @Test
  def fragmentationAllPeptideTest(): Unit = {
    val start = System.currentTimeMillis()
    for (peptide <- readRS.peptides) {
      val currentFragmentIonTypes = new FragmentIons(ionTypeB = true, ionTypeY = true, chargeForIonsB = 2, chargeForIonsY = 2)
      val table = new FragmentIonTable(peptide, currentFragmentIonTypes)
    }
    logger.debug("Done in " + (System.currentTimeMillis() - start) + "ms for " + readRS.peptides.length + " peptides")
  }

  def readCSV(filename: String): Array[TheoreticalFragmentSeries] = {
    val src = Source.fromURL(getClass.getResource(filename))
    val iter = src.getLines().map(_.split(",", -1))
    val fragmentIonTable = new ArrayBuffer[TheoreticalFragmentSeries]
    iter.next().foreach { s:String => fragmentIonTable += new TheoreticalFragmentSeries(s, new Array[Double](0))  }
    try {
      iter foreach { item =>  
        for (i <- 0 until item.length) {
        	fragmentIonTable(i).masses = fragmentIonTable(i).masses :+ { if (!item(i).isEmpty) item(i).toDouble else 0.0 }
        }
      }
    } catch {
      case e: Exception => logger.error("cannot create ArrayBuffer[TheoreticalFragmentSeries] from file "+filename, e)
    }
    src.close()
    fragmentIonTable.toArray
  }
}