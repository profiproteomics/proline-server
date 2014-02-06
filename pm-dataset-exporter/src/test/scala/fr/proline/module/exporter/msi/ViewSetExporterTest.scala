package fr.proline.module.exporter.msi

import java.nio.file.Files
import scala.collection.mutable.ArrayBuffer
import org.junit._
import org.apache.commons.io.FileUtils
import com.typesafe.scalalogging.slf4j.Logging

import fr.proline.context.IExecutionContext
import fr.proline.core.dal.ContextFactory
import fr.proline.core.om.provider.msi.impl._
import fr.proline.core.om.util.AbstractMultipleDBTestCase
import fr.proline.module.exporter.msi.template.IRMaLikeViewSetTemplateAsXLSX
import fr.proline.module.exporter.msi.view.BuildResultSummaryViewSet
import fr.proline.module.exporter.commons.ViewSetExporter
import fr.proline.repository.DriverType

/**
 * @author David Bouyssie
 *
 */
class ViewSetExporterTest  extends AbstractMultipleDBTestCase with Logging {

  // Define some vars
  val driverType = DriverType.H2
  val fileName = "STR_F063442_F122817_MergedRSMs"
  val targetRSMId: Long = 33

  var executionContext: IExecutionContext = null

  @Before
  @throws(classOf[Exception])
  def setUp() = {

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

    val rsmProvider = new SQLResultSummaryProvider(executionContext.getMSIDbConnectionContext, executionContext.getPSDbConnectionContext, executionContext.getUDSDbConnectionContext)
    val rsm = rsmProvider.getResultSummary(targetRSMId, true).get
    
    // ADD CUSTOM CODE FOR SUBSETS LOADING
    
    // Instantiate additional providers
    val pepProvider = new SQLPeptideProvider(executionContext.getPSDbConnectionContext() )
    val pepInstProvider = new SQLPeptideInstanceProvider(executionContext.getMSIDbConnectionContext, pepProvider)
    val pepSetProvider = new SQLPeptideSetProvider(executionContext.getMSIDbConnectionContext, pepInstProvider )
    
    // Retrieve all subsets ids
    val allSubsetIds = new ArrayBuffer[Long]
    rsm.peptideSets.map { peptideSet =>
      val strictSubsetIds = Option(peptideSet.strictSubsetIds).getOrElse( Array() )
      val subsumableSubsetIds = Option(peptideSet.subsumableSubsetIds).getOrElse( Array() )
      allSubsetIds ++= strictSubsetIds ++ subsumableSubsetIds
    }
    
    // Load all subsets
    val allSubsets = pepSetProvider.getPeptideSets(allSubsetIds.distinct)
    val subsetById = allSubsets.map( ps => ps.id -> ps ).toMap
    rsm.peptideSets.map { peptideSet =>
      if(peptideSet.strictSubsetIds != null && peptideSet.strictSubsets == null) {
        peptideSet.strictSubsets = Some( peptideSet.strictSubsetIds.map( subsetById(_) ) )
      }
      if(peptideSet.subsumableSubsetIds != null && peptideSet.subsumableSubsets == null) {
        peptideSet.subsumableSubsets = Some( peptideSet.subsumableSubsetIds.map( subsetById(_) ) )
      }
    }
    // END OF CUSTOM CODE FOR SUBSETS LOADING
    
    val viewSet = BuildResultSummaryViewSet(rsm,fileName,IRMaLikeViewSetTemplateAsXLSX )
    
    // Create TEMP dir
    val exportPath = Files.createTempDirectory(null)
    val exportLoc = exportPath.toFile
    
    var exception = Option.empty[Exception]
    try {
      
      // Export the RSM
      ViewSetExporter.exportViewSetToDirectory(viewSet, exportLoc )
      
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