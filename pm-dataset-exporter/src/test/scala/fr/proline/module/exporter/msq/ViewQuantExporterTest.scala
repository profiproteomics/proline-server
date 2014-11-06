package fr.proline.module.exporter.msq

import com.typesafe.scalalogging.slf4j.Logging
import org.junit.Before
import org.junit.After
import org.junit.Test
import org.apache.commons.io.FileUtils
import java.nio.file.Files
import fr.proline.repository.DriverType
import fr.proline.context.IExecutionContext
import fr.proline.core.dal.ContextFactory
import fr.proline.core.dal.AbstractMultipleDBTestCase
import fr.proline.module.exporter.msq.view.BuildQuantitationViewSet
import fr.proline.module.exporter.msq.template.BasicMasterQuantProteinSetsAsXLSX
import fr.proline.module.exporter.commons.ViewSetExporter


class ViewQuantExporterTest extends AbstractMultipleDBTestCase with Logging {

  // Define some vars
  val driverType = DriverType.H2
  val fileName = "Quant_XIC_900-911"
  val projectId = 74
  val masterQuantChannelId: Long = 375 

  var executionContext: IExecutionContext = null
  
  @Before
  @throws(classOf[Exception])
  def setUp() = {
    
    java.util.Locale.setDefault(new java.util.Locale("en", "US"))

    logger.info("Initializing DBs")
    super.initDBsDBManagement(driverType)

    // Load Data
    pdiDBTestCase.loadDataSet("/dbunit/datasets/pdi/Proteins_Dataset.xml")

    logger.info("PDI  dbs successfully initialized !")

    executionContext = ContextFactory.buildExecutionContext(dsConnectorFactoryForTest, projectId, true) // Full JPA
  }
  
  @After
  override def tearDown() {
    if (executionContext != null) executionContext.closeAll()
    super.tearDown()
  }
  
  @Test
  def testExportViewSetToDir() {

//    val viewSet = BuildQuantitationViewSet(
//      executionContext,
//      projectId,
//      masterQuantChannelId,
//      fileName,
//      BasicMasterQuantProteinSetsAsXLSX
//    )
//    
//    // Create TEMP dir
//    val exportPath = Files.createTempDirectory(null)
//    val exportLoc = exportPath.toFile
//    
//    var exception = Option.empty[Exception]
//    try {
//      
//      // Export the Quant
//      ViewSetExporter.exportViewSetToDirectory(viewSet, exportLoc )
//      
//      //println(exportLoc)
//      //Thread.sleep(100000)
//      
//      // TODO: compare the content of the exported file with an expected content
//      
//    } catch {
//      case e: Exception => exception = Some(e)
//    }
//    finally {
//      // Remove temp dir
//      FileUtils.deleteDirectory(exportLoc)
//    }
//
//    // Throw exception if catched
//    exception.map( throw _ )

  }


  
}