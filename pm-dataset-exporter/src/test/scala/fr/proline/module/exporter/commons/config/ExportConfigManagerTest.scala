package fr.proline.module.exporter.commons.config

import com.typesafe.scalalogging.slf4j.Logging
import org.junit.Assert.assertTrue
import org.junit.Test
import scala.io.Source

class ExportConfigManagerTest  extends  Logging{
  
	@Test
	def testReadConfig() {
	  var filename = this.getClass().getResource("/exportConfig_test.json").toString()
	  // remove file:/
	  filename = filename.substring(6)
	  try {
	    val config = ExportConfigManager.readConfig(Source.fromFile(filename).getLines.mkString);
	    assertTrue("Config nbSheets" , config.sheets.length == 2)
	    assertTrue("Config nbFields" , config.sheets(1).fields.length == 1)
	  } catch {
      	case e: Exception => logger.error("error", e)
	  }
	}
	
	@Test
	def testGetAllConfigurationForIdentificationExport() {
	  try {
	    val configStr = ExportConfigManager.getAllConfigurationForIdentificationExport();
	    //  reserialize check the conf
	    val config = ExportConfigManager.readConfig(configStr)
	    assertTrue("All Ident export nbSheets" , config.sheets.length == 7)
	    
	  } catch {
      	case e: Exception => logger.error("error", e)
	  }
	}
	
}