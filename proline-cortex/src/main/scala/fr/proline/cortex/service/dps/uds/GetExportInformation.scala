package fr.proline.cortex.service.dps.uds

import com.typesafe.scalalogging.slf4j.Logging
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import fr.profi.util.serialization.ProfiJson._
import fr.proline.module.exporter.commons.config.ExportConfigConstant
import fr.proline.module.exporter.commons.config.ExportConfigManager
import fr.proline.core.orm.uds.QuantitationMethod
import fr.proline.core.orm.uds.Dataset.DatasetType
import fr.proline.core.service.msq.QuantMethodType
import fr.proline.core.dal.BuildExecutionContext
import fr.proline.core.orm.uds.{ Dataset => UdsDataset }
import fr.proline.core.service.msq.AbundanceUnit
import fr.proline.cortex.service.AbstractRemoteProcessService
import fr.proline.cortex.util.DbConnectionHelper

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

class GetExportInformation extends AbstractRemoteProcessService with Logging {
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
      val execCtx = BuildExecutionContext(DbConnectionHelper.getIDataStoreConnectorFactory(), projectId, true)
      try {
        val udsDbCtx = execCtx.getUDSDbConnectionContext()
        val udsEM = udsDbCtx.getEntityManager()
        val udsDs = udsEM.find(classOf[UdsDataset], datasetId)
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
    } else {
      if (extraParams.get("export_mode") != null) {
        mode = extraParams.get("export_mode").asInstanceOf[String]
      }
    }

    return ExportConfigManager.getAllConfigurationExport(mode)

  }

}