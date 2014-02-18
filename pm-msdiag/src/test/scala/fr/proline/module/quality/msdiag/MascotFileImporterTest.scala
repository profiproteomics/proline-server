package fr.proline.module.quality.msdiag

import com.typesafe.scalalogging.slf4j.Logging
import java.io.File
import fr.proline.module.parser.mascot._
import fr.proline.context.BasicExecutionContext
import fr.proline.core.service.msi.ResultFileImporter

object MascotFileImporterTest extends Logging {

  lazy val propertiesBuilder = {
    val pb = Map.newBuilder[String, Any]
    pb += ( "ion.score.cutoff" -> 0.0 )
    pb += ( "subset.threshold" -> 1.0 )
    pb
  }
  
  def parse(file: String, executionContext: BasicExecutionContext): ResultFileImporter = {
    var datFile = new File(getClass.getResource("/dat_samples/" + file).toURI)
    logger.debug("Parsing file " + file)
    
    val importer = new ResultFileImporter(
      executionContext,
      resultIdentFile = datFile,
      fileType = "mascot.dat",
      instrumentConfigId = 1,
      peaklistSoftwareId = 8,
      importerProperties = propertiesBuilder.result,
      acDecoyRegex = Some("""REVERSED_gi\|\S+""".r)
    )
    importer
  }

}