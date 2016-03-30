package fr.proline.module.exporter.commons.config

import scala.io.Source

import org.junit.Assert.assertEquals
import org.junit.Test

import com.typesafe.scalalogging.LazyLogging

class ExportConfigManagerTest  extends LazyLogging{
  
	@Test
	def testReadConfig() {
	  var filename = this.getClass().getResource("/exportConfig_test.json").toString()
	  // remove file:/
	  filename = filename.substring(6)
	  try {
	    val config = ExportConfigManager.readConfig(Source.fromFile(filename).getLines.mkString)
	    assertEquals("Config nbSheets", 2, config.sheets.length)
	    assertEquals("Config nbSheets", 1, config.sheets(1).fields.length)
	  } catch {
      	case e: Exception => logger.error("error", e)
	  }
	}
	
	@Test
	def testGetAllConfigurationForIdentificationExport() {
	  try {
	    val configStr = ExportConfigManager.getFullConfigForIdentificationExport()
	    //  reserialize check the conf
	    val config = ExportConfigManager.readConfig(configStr)
	    assertEquals("All Ident export nbSheets", 7, config.sheets.length)
	    
	  } catch {
      	case e: Exception => logger.error("error", e)
	  }
	}
	
	
	@Test
	def testGetAllConfigurationForSCExport() {
	  try {
	    val configStr = ExportConfigManager.getFullConfigForSCExport()
	    //  reserialize check the conf
	    val config = ExportConfigManager.readConfig(configStr)
	    assertEquals("All SC export nbSheets", 7, config.sheets.length)
	    assertEquals("All SC export nbFields in ProteinSet sheet ", 21, config.sheets(2).fields.length)
	    
	  } catch {
      	case e: Exception => logger.error("error", e)
	  }
	}
	
	@Test
	def testGetAllConfigurationForXICExport() {
	  try {
	    val configStr = ExportConfigManager.getFullConfigForXicExport()
	    //  reserialize check the conf
	    val config = ExportConfigManager.readConfig(configStr)
	    assertEquals("All XIC export nbSheets", 7, config.sheets.length)
	    assertEquals("All XIC export nbFields in ProteinSet sheet ", 25, config.sheets(2).fields.length)
	  } catch {
      	case e: Exception => logger.error("error", e)
	  }
	}
	
	@Test
	def testGetDefaultConfiguration(){
	  try{
	    // identification
	    val defaultIdentConf = ExportConfigManager.getDefaultExportConfigAsJson(ExportConfigConstant.MODE_IDENT)
	    // reserialize to check
	    val defaultIdentConfObj = ExportConfigManager.readConfig(defaultIdentConf)
	    assertEquals("Default Configuration for Identification ", 6, defaultIdentConfObj.sheets.length)
	    // SC
	    val defaultSCConf = ExportConfigManager.getDefaultExportConfigAsJson(ExportConfigConstant.MODE_QUANT_SC)
	    // reserialize to check
	    val defaultSCConfObj = ExportConfigManager.readConfig(defaultSCConf)
	    assertEquals("Default Configuration for SC ", 4, defaultSCConfObj.sheets.length)
	    //XIC
	    val defaultXICConf = ExportConfigManager.getDefaultExportConfigAsJson(ExportConfigConstant.MODE_QUANT_XIC)
	    // reserialize to check
	    val defaultXICConfObj = ExportConfigManager.readConfig(defaultXICConf)
	    assertEquals("Default Configuration for XIC ", 3, defaultXICConfObj.sheets.length)
	  } catch {
      	case e: Exception => logger.error("error", e)
	  }
	}
	
	@Test
	def testCheckTitleIdent(){
	 try {
	     val configStr = ExportConfigManager.getFullConfigForIdentificationExport()
	    //  reserialize check the conf
	    val config = ExportConfigManager.readConfig(configStr)
	    //val check: Boolean = ExportConfigManager.checkTitle(config)
	    //assertTrue("Check Title Identification ", check == true)
	  } catch {
      	case e: Exception => logger.error("error", e)
	  }
	}
	
	@Test
	def testCheckTitleSC(){
	 try {
	     val configStr = ExportConfigManager.getFullConfigForSCExport()
	    //  reserialize check the conf
	    val config = ExportConfigManager.readConfig(configStr)
	    //val check: Boolean = ExportConfigManager.checkTitle(config)
	    //assertTrue("Check Title SC ", check == true)
	  } catch {
      	case e: Exception => logger.error("error", e)
	  }
	}
	
	@Test
	def testCheckTitleXIC(){
	 try {
	     val configStr = ExportConfigManager.getFullConfigForXicExport()
	    //  reserialize check the conf
	    val config = ExportConfigManager.readConfig(configStr)
	    //val check: Boolean = ExportConfigManager.checkTitle(config)
	    //assertTrue("Check Title XIC ", check == true)
	  } catch {
      	case e: Exception => logger.error("error", e)
	  }
	}
}