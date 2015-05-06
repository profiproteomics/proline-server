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
	
	
	@Test
	def testGetAllConfigurationForSCExport() {
	  try {
	    val configStr = ExportConfigManager.getAllConfigurationForSCExport();
	    //  reserialize check the conf
	    val config = ExportConfigManager.readConfig(configStr)
	    assertTrue("All SC export nbSheets" , config.sheets.length == 7)
	    assertTrue("All SC export nbFields in ProteinSet sheet " , config.sheets(2).fields.length == 21)
	    
	  } catch {
      	case e: Exception => logger.error("error", e)
	  }
	}
	
	@Test
	def testGetAllConfigurationForXICExport() {
	  try {
	    val configStr = ExportConfigManager.getAllConfigurationForXICExport();
	    //  reserialize check the conf
	    val config = ExportConfigManager.readConfig(configStr)
	    assertTrue("All XIC export nbSheets" , config.sheets.length == 3)
	    assertTrue("All XIC export nbFields in ProteinSet sheet " , config.sheets(2).fields.length == 21)
	  } catch {
      	case e: Exception => logger.error("error", e)
	  }
	}
	
	@Test
	def testGetDefaultConfiguration(){
	  try{
	    // identification
	    val defaultIdentConf = ExportConfigManager.getDefaultConfiguration(ExportConfigConstant.MODE_IDENT)
	    // reserialize to check
	    val defaultIdentConfObj = ExportConfigManager.readConfig(defaultIdentConf)
	    assertTrue("Default Configuration for Identification ", defaultIdentConfObj.sheets.length == 6)
	    // SC
	    val defaultSCConf = ExportConfigManager.getDefaultConfiguration(ExportConfigConstant.MODE_QUANT_SC)
	    // reserialize to check
	    val defaultSCConfObj = ExportConfigManager.readConfig(defaultSCConf)
	    assertTrue("Default Configuration for SC ", defaultSCConfObj.sheets.length == 4)
	    //XIC
	    val defaultXICConf = ExportConfigManager.getDefaultConfiguration(ExportConfigConstant.MODE_QUANT_XIC)
	    // reserialize to check
	    val defaultXICConfObj = ExportConfigManager.readConfig(defaultXICConf)
	    assertTrue("Default Configuration for XIC ", defaultXICConfObj.sheets.length == 3)
	  } catch {
      	case e: Exception => logger.error("error", e)
	  }
	}
}