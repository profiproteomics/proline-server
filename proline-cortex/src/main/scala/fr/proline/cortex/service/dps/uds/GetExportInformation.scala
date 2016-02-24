package fr.proline.cortex.service.dps.uds

import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging

import fr.profi.util.serialization.ProfiJson.deserialize
import fr.profi.util.serialization.ProfiJson.serialize
import fr.proline.core.orm.uds.Dataset
import fr.proline.core.orm.uds.Dataset.DatasetType
import fr.proline.core.orm.uds.QuantitationMethod
import fr.proline.core.service.msq.AbundanceUnit
import fr.proline.core.service.msq.QuantMethodType
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.jms.service.api.AbstractRemoteProcessService
import fr.proline.module.exporter.commons.config.ExportConfigConstant
import fr.proline.module.exporter.commons.config.ExportConfigManager

/**
 * Define a JMS Service to :
 * Get information about the export default configuration file for a dataset
 *
 * Input param
 *   project_id : The id of the project
 *   dataset_id : The id of the dataset
 *   extra_params: A map of specific parameters : export_mode can contain the mode (IDENT, QUANT_SC or QUANT_XIC)
 *
 *  Output param
 *    The JSON String corresponding to the default export configuration of specified mode
 *
 */

object DatasetUtil {

  def getExportMode(projectId: Long, datasetId: Long) : String = {
      var mode: String = ExportConfigConstant.MODE_IDENT
      val execCtx =  DbConnectionHelper.createJPAExecutionContext(projectId) 
      try {
        val udsDbCtx = execCtx.getUDSDbConnectionContext()
        val udsEM = udsDbCtx.getEntityManager()
        val udsDs = udsEM.find(classOf[Dataset], datasetId)
        if (udsDs != null) {
          val dsType: DatasetType = udsDs.getType()
          if (dsType == DatasetType.QUANTITATION) {
            mode = ExportConfigConstant.MODE_QUANT_XIC
            val dsMethod: QuantitationMethod = udsDs.getMethod()
            val quantMethodType = dsMethod.getType
            val abundanceUnit = dsMethod.getAbundanceUnit
            if (quantMethodType == QuantMethodType.LABEL_FREE.toString() && abundanceUnit == AbundanceUnit.SPECTRAL_COUNTS.toString()) {
              mode = ExportConfigConstant.MODE_QUANT_SC
            }
          }
        }
      } finally {
        if (execCtx != null) {
          execCtx.closeAll()
        }
      }
     mode
  }
}

class GetExportInformation extends AbstractRemoteProcessService with LazyLogging {
  /* JMS Service identification */
  val serviceName = "proline/dps/uds/GetExportInformation"
  val serviceVersion = "1.0"
  override val defaultVersion = true

  /* Define the concrete doProcess method */
  override def doProcess(paramsRetriever: NamedParamsRetriever): Object = {
    require((paramsRetriever != null), "No Parameters specified")

    val projectId = paramsRetriever.getLong("project_id")
    val datasetId = paramsRetriever.getLong("dataset_id")
    val extraParamsAsOptStr = Option(paramsRetriever.getOptMap("extra_params", true, null)).map(serialize(_))
    val extraParams = extraParamsAsOptStr.map(deserialize[Map[String, Object]](_))

    var mode: String = ExportConfigConstant.MODE_IDENT
    if (projectId > 0) {
      mode = DatasetUtil.getExportMode(projectId, datasetId)
    } else {
      if (extraParams.get("export_mode") != null) {
        mode = extraParams.get("export_mode").asInstanceOf[String]
      }
    }

    return ExportConfigManager.getAllConfigurationExport(mode)

  }

}