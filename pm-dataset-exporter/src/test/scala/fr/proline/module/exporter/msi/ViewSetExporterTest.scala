package fr.proline.module.exporter.msi

import java.nio.file.Files
import scala.collection.mutable.ArrayBuffer
import org.junit._
import org.apache.commons.io.FileUtils
import com.typesafe.scalalogging.slf4j.Logging

import fr.proline.context.IExecutionContext
import fr.proline.core.dal.ContextFactory
import fr.proline.core.om.provider.msi.impl._
import fr.proline.core.dal.AbstractMultipleDBTestCase
import fr.proline.module.exporter.msi.template.ProlineViewSetTemplateAsXLSX
import fr.proline.module.exporter.msi.view.BuildResultSummaryViewSet
import fr.proline.module.exporter.commons.ViewSetExporter
import fr.proline.repository.DriverType

/**
 * @author David Bouyssie
 *
 */
class ViewSetExporterTest extends AbstractMultipleDBTestCase with Logging {

  // Define some vars
  val driverType = DriverType.H2
  val fileName = "STR_F063442_F122817_MergedRSMs"
  val projectId = 1
  val targetRSMId: Long = 33 // 2 to have info data

  var executionContext: IExecutionContext = null

  @Before
  @throws(classOf[Exception])
  def setUp() = {
    
    java.util.Locale.setDefault(new java.util.Locale("en", "US"))

    logger.info("Initializing DBs")
    super.initDBsDBManagement(driverType)

    // Load Data
    pdiDBTestCase.loadDataSet("/dbunit/datasets/pdi/Proteins_Dataset.xml")
    psDBTestCase.loadDataSet("/dbunit_samples/" + fileName + "/ps-db.xml")
    msiDBTestCase.loadDataSet("/dbunit_samples/" + fileName + "/msi-db.xml")
    udsDBTestCase.loadDataSet("/dbunit_samples/" + fileName + "/uds-db.xml")

    logger.info("PDI, PS, MSI and UDS dbs successfully initialized !")

    executionContext = ContextFactory.buildExecutionContext(dsConnectorFactoryForTest, 1, true) // Full JPA
  }

  @After
  override def tearDown() {
    if (executionContext != null) executionContext.closeAll()
    super.tearDown()
  }
  
  @Test
  def testExportViewSetToDir() {

    val viewSet = BuildResultSummaryViewSet(
      executionContext,
      projectId,
      targetRSMId,
      true,
      true,
      fileName,
      ProlineViewSetTemplateAsXLSX
    )
    
    // Create TEMP dir
    val exportPath = Files.createTempDirectory(null)
    val exportLoc = exportPath.toFile
    
    var exception = Option.empty[Exception]
    try {
      
      // Export the RSM
      ViewSetExporter.exportViewSetToDirectory(viewSet, exportLoc )
      
      //println(exportLoc)
      //Thread.sleep(100000)
      
      // TODO: compare the content of the exported file with an expected content
      
    } catch {
      case e: Exception => exception = Some(e)
    }
    finally {
      // Remove temp dir
      FileUtils.deleteDirectory(exportLoc)
    }

    // Throw exception if catched
    exception.map( throw _ )

  }

}