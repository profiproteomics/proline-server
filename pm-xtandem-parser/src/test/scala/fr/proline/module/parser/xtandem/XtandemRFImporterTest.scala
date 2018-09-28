package fr.proline.module.parser.xtandem

import java.io.File

import org.junit.Assert.assertNotNull
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test

import fr.proline.core.dal.AbstractMultipleDBTestCase
import fr.proline.core.dal.BuildLazyExecutionContext
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IPTMProvider
import fr.proline.core.om.provider.msi.impl.ORMResultSetProvider
import fr.proline.core.om.provider.msi.impl.SQLPTMProvider
import fr.proline.core.om.provider.msi.impl.SQLPeptideProvider
import fr.proline.core.service.msi.ResultFileImporter
import fr.proline.repository.DriverType

@Test
class XtandemRFImporterTest extends AbstractMultipleDBTestCase {

  val driverType = DriverType.H2
  var parserContext: ProviderDecoratedExecutionContext = null
  var _xtandemFileName =  "/xtandemResultFile/output.2014_11_18_11_22_40.t.xml"
//  var _xtandemFileName =  "/xtandemResultFile/QEKAC141027_122.raw.mzDB.t.xml"

  @Before
  @throws(classOf[Exception])
  def setUp() = {

    logger.info("Initializing Dbs")
    super.initDBsDBManagement(driverType)

//VDS Still needed    SQLPeptideProvider.clear() // Clear peptide cache between tests

    //Load Data
     msiDBTestCase.loadDataSet("/default_datasets/Init_Dataset.xml")
    udsDBTestCase.loadDataSet("/default_datasets/UDS_Simple_Dataset.xml")
    logger.info("Dbs succesfully initialized")

  }

  def buildJPAContext() = {
    val executionContext = BuildLazyExecutionContext(dsConnectorFactoryForTest, 1, true) // Full JPA
    val rsProvider = new ORMResultSetProvider(executionContext.getMSIDbConnectionContext)
    parserContext = ProviderDecoratedExecutionContext(executionContext) // Use Object factory
    assertNotNull(parserContext)
    (executionContext, rsProvider)
  }

  override def tearDown() {

//    try {
//      SQLPeptideProvider.clear() // Clear peptide cache between tests
//    } finally {
//      super.tearDown()
//    }

  }

//  @Test
//  def runRFImporter() = {
//    val (executionContext, rsProvider) = buildJPAContext
//
//    assertNotNull(executionContext)
//
//    try {
//
//      logger.debug(" --- Get File " + _xtandemFileName)
//      var identFile: File = new File(this.getClass.getResource(_xtandemFileName).toURI)
//      
//      import fr.proline.core.om.provider.uds.impl.{ SQLPeaklistSoftwareProvider => UdsSQLPklSoftProvider }
//      val udsPklSoftProvider = new UdsSQLPklSoftProvider(parserContext.getUDSDbConnectionContext())
//      val pklsft = udsPklSoftProvider.getPeaklistSoftware(2)
//      if(pklsft.isDefined) {
//        println("ABU peaklist software id is '"+pklsft.get.id+"'")
//        println("ABU peaklist software name is '"+pklsft.get.name+"'")
//        println("ABU peaklist software version is '"+pklsft.get.version+"'")
//      } else
//        println("ABU peaklist software is not defined !")
//      
//      val regex = Some("###REV###".r) // Does not work ! maybe it's related to the way proteins are stored ?
//      val importer = new ResultFileImporter(
//        parserContext,
//        resultIdentFile = identFile,
//        fileType = "xtandem.xml",
//        instrumentConfigId = 1,
////        peaklistSoftwareId = udsPklSoftProvider.getPeaklistSoftware(2).get.id,
//        peaklistSoftwareId = 2,
//        importerProperties = Map.empty,
//        acDecoyRegex = regex)
//      
//      logger.debug(" --- run service ")
//      val result = importer.runService()
//      val id = importer.getTargetResultSetId
//      logger.debug(" --- done " + result + " save with resultID " + id)
//
//      assertTrue(result)
//
//      assertTrue(id > 0)
//
//      val rsBackOp = rsProvider.getResultSet(id)
//      assertTrue(rsBackOp.isDefined)
//      val rsBack = rsBackOp.get
//      assertNotNull(rsBack)
//
//      // Other verifs....
//
//      val msiEM = executionContext.getMSIDbConnectionContext.getEntityManager
//
//      val query = msiEM.createQuery("select count (*) from fr.proline.core.orm.msi.ProteinMatchSeqDatabaseMap")
//
//      var count: Long = -1
//      val countObj = query.getSingleResult()
//
//      if (countObj.isInstanceOf[java.lang.Long]) {
//        count = countObj.asInstanceOf[java.lang.Long].longValue
//      }
//
//      assertTrue("Invalid number of ProteinMatchSeqDatabaseMap created : "+count, count > 0L)
//      logger.debug("Number of ProteinMatchSeqDatabaseMap created: " + count)
//    } finally {
//      executionContext.closeAll()
//    }
//
//  }

}