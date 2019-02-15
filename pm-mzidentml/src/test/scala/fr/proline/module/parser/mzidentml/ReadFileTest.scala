package fr.proline.module.parser.mzidentml

import java.net.URL

import com.typesafe.scalalogging.StrictLogging
import fr.proline.core.dal.AbstractDatastoreTestCase
import fr.proline.core.om.model.msi.Instrument
import fr.proline.core.om.model.msi.InstrumentConfig
import fr.proline.core.om.provider.msi.IPTMProvider
import fr.proline.core.om.provider.msi.impl.SQLPTMProvider
import fr.proline.repository.DriverType
import org.junit.BeforeClass
import org.junit.Test

object ReadFileTest extends AbstractDatastoreTestCase with StrictLogging {

  override val driverType = DriverType.H2
  override val useJPA = true

  /***   CONNECTION TO DB   ***/

  @BeforeClass
  def init() {
    logger.debug("Test initialization")
    super.initDBsDBManagement(driverType)

    //Load Data
    logger.info("Initializing Dbs")
    msiDBTestCase.loadDataSet(dbUnitResultFile.msiDbDatasetPath )
    logger.info("MSI dbs succesfully initialized")

    udsDBTestCase.loadDataSet("/default_datasets/UDS_Simple_Dataset.xml")
    //udsDBTestCase.loadCompositeDataSet(Array("/dbunit/datasets/uds-db_init_dataset.xml", "/default_datasets/UDS_Simple_Dataset.xml"))
    logger.info("UDS db succesfully initialized")

    executionContext = if( useJPA ) buildJPAContext() else buildSQLContext()
    executionContext.putProvider(classOf[IPTMProvider], new SQLPTMProvider(executionContext.getMSIDbConnectionContext))

  }

}

class ReadFileTest {
  
  val mascotFilePath = "/result_files/mascotExample.mzid"
  val mgfPFilePath = "/result_files/mgfPlus.mzid"
  
  @Test
  def testReadMascotResultFile() {
   
    val inst = new Instrument(-1, "teqt", "src", None)
    val ic = new InstrumentConfig(-1, inst, "ms1A", "msnA", "ActType")
    val fileURL: URL = classOf[ReadFileTest].getResource(mascotFilePath)
    val rf = MzIdResultFile(fileURL, ReadFileTest.executionContext)
    rf.instrumentConfig = Some(ic)

    rf.getResultSet(false)
  }

  @Test
  def testReadMgfPlusResultFile() {
   
    val inst = new Instrument(-1, "teqt", "src", None)
    val ic = new InstrumentConfig(-1, inst, "ms1A", "msnA", "ActType")
    val fileURL: URL = classOf[ReadFileTest].getResource(mgfPFilePath)
    val rf = MzIdResultFile(fileURL, ReadFileTest.executionContext)
    rf.instrumentConfig = Some(ic)

    rf.getResultSet(false)
  }
}