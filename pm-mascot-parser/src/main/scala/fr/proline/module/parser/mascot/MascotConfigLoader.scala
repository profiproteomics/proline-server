package fr.proline.module.parser.mascot

import com.typesafe.scalalogging.LazyLogging

import matrix_science.msparser.ms_enzymefile
import matrix_science.msparser.ms_errors
import matrix_science.msparser.ms_fragrulesfile
import matrix_science.msparser.ms_masses
import matrix_science.msparser.ms_modfile
import matrix_science.msparser.ms_quant_configfile
import matrix_science.msparser.ms_umod_configfile

object MascotConfigLoader {
  
  def getEnzymesFile( fileOrServerURL: String ): ms_enzymefile = {
    val file = new ms_enzymefile( fileOrServerURL )
    MascotErrorsHelper.checkMascotErrors( file.asInstanceOf[ms_errors],"invalid enzymes file !" )    
    file
  }
  
  def getFragmentationRulesFile( fileOrServerURL: String ): ms_fragrulesfile = {
    val file = new ms_fragrulesfile( fileOrServerURL )
    MascotErrorsHelper.checkMascotErrors( file.asInstanceOf[ms_errors],"invalid fragmentations file !" )    
    file
  }
  
  def getMassesFile( fileOrServerURL: String ): ms_masses = {
    val file = new ms_masses( fileOrServerURL )
    MascotErrorsHelper.checkMascotErrors( file.asInstanceOf[ms_errors],"invalid masses file !" )    
    file
  }

  def getModificationsFile( fileOrServerURL: String, massesFile: ms_masses ): ms_modfile = {
    val file = new ms_modfile(fileOrServerURL, massesFile)
    MascotErrorsHelper.checkMascotErrors( file.asInstanceOf[ms_errors],"invalid modifications file !" )  
    file
  }
  
  def getUnimodFile( fileOrServerURL: String ): ms_umod_configfile = {
    val file = new ms_umod_configfile(fileOrServerURL, "schema/unimod_2.xsd")
    MascotErrorsHelper.checkMascotErrors( file.asInstanceOf[ms_errors],"invalid unimod file !" )      
    file
  }
  
  def getQuantitationFile( fileOrServerURL: String, mascotVersion: String ): ms_quant_configfile = {
    var file: ms_quant_configfile = null
    file = new ms_quant_configfile()
    file.setFileName(fileOrServerURL)
    
    /*if ( mascotVersion.startsWith("2.3") || mascotVersion.startsWith("2.4") )
      file = new ms_quant_configfile(fileOrServerURL, "config/quantitation_2.xsd")
    else
      file = new ms_quant_configfile(fileOrServerURL, "config/quantitation_1.xsd")*/
                
    MascotErrorsHelper.checkMascotErrors( file.asInstanceOf[ms_errors],"invalid quantitation file !" )
    
    file
  }
}

trait IMascotConfig extends LazyLogging  {
  
  val enzymesFile: ms_enzymefile
  val fragmentationRulesFile: ms_fragrulesfile
  val mascotVersion: String
  val massesFile: ms_masses
  val modificationsFile: ms_modfile
  val quantitationFile: ms_quant_configfile
  val unimodFile: ms_umod_configfile
  
  val NONE_QUANTI_METHOD_NAME = "None"
  
}

class MascotLocalConfig (
  val mascotVersion: String,
  val enzymesFilePath: String,
  val fragmentationRulesFilePath: String,  
  val massesFileFilePath: String,
  val modificationsFilePath: String,
  val quantitationFilePath: String,
  val unimodFilePath: String  
  ) extends IMascotConfig {
  
  lazy val enzymesFile = MascotConfigLoader.getEnzymesFile(enzymesFilePath)  
  lazy val fragmentationRulesFile = MascotConfigLoader.getFragmentationRulesFile(fragmentationRulesFilePath)  
  lazy val massesFile = MascotConfigLoader.getMassesFile(massesFileFilePath)
  lazy val modificationsFile = MascotConfigLoader.getModificationsFile(modificationsFilePath,massesFile)
  lazy val quantitationFile = MascotConfigLoader.getQuantitationFile( quantitationFilePath, mascotVersion )
  lazy val unimodFile = MascotConfigLoader.getUnimodFile(unimodFilePath)
  
}

class MascotRemoteConfig( val mascotVersion: String, val mascotServerURLAsStr: String ) extends IMascotConfig {
  
  lazy val enzymesFile = MascotConfigLoader.getEnzymesFile(mascotServerURLAsStr)  
  lazy val fragmentationRulesFile = MascotConfigLoader.getFragmentationRulesFile(mascotServerURLAsStr)  
  lazy val massesFile = MascotConfigLoader.getMassesFile(mascotServerURLAsStr)
  lazy val modificationsFile = MascotConfigLoader.getModificationsFile(mascotServerURLAsStr,massesFile)
  lazy val quantitationFile = MascotConfigLoader.getQuantitationFile( mascotServerURLAsStr, mascotVersion )
  lazy val unimodFile = MascotConfigLoader.getUnimodFile(mascotServerURLAsStr)
  
}