//package fr.proline.module.quality.msdiag
//
//import com.typesafe.scalalogging.slf4j.Logging
//import java.io.File
//import fr.proline.module.parser.omssa._
////import fr.proline.core.om.provider.uds.impl.{ SQLPeaklistSoftwareProvider => UdsSQLPklSoftProvider }
////import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
//import fr.proline.context.BasicExecutionContext
//import fr.proline.core.service.msi.ResultFileImporter
//
//object OmssaFileImporterTest extends Logging {
//
//  lazy val propertiesBuilder = {
//    val pb = Map.newBuilder[String, Any]
//    pb += (OmssaParseParams.OMSSA_VERSION.toString -> "2.1.9")
//    pb += (OmssaParseParams.USERMOD_XML_FILE.toString -> new File(this.getClass().getResource("/omx_samples/usermods.xml").toURI()))
//    pb += (OmssaParseParams.FASTA_FILE_PATH.toString -> "")
//    pb += (OmssaParseParams.FASTA_TAXONOMIES.toString -> "")
//    pb += (OmssaParseParams.PEAK_LIST_FILE_PATH.toString -> "")
//    pb += (OmssaParseParams.RAW_FILE_PATH.toString -> "")
//    pb += (OmssaParseParams.PTM_COMPOSITION_FILE.toString -> new File(this.getClass().getResource("/omx_samples/compositions.txt").toURI()))
//    pb
//  }
//
//  def parse(file: String, executionContext: BasicExecutionContext): ResultFileImporter = {
//    var omxFile = new File(getClass.getResource("/omx_samples/" + file).toURI)
//    logger.debug("Parsing file " + file)
//    
//    val importer = new ResultFileImporter(
//      executionContext,
//      resultIdentFile = omxFile,
//      fileType = "omssa.omx",
//      instrumentConfigId = 1,
//      peaklistSoftwareId = 1,
//      importerProperties = propertiesBuilder.result,
//      acDecoyRegex = Some("""sp\|REVERSED_\S+""".r)
//    )
//    importer
//  }
//
//}