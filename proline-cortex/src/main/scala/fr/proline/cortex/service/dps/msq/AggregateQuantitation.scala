package fr.proline.cortex.service.dps.msq

import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.exception.ExceptionUtils
import fr.profi.util.serialization.ProfiJson.{deserialize, serialize}
import fr.proline.context.IExecutionContext
import fr.proline.core.algo.msq.config.AggregationQuantConfig
import fr.proline.core.dal.context.execCtxToTxExecCtx
import fr.proline.core.om.model.msq.ExperimentalDesign
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.orm.uds.ObjectTreeSchema.{SchemaName => UdsSchemaName}
import fr.proline.core.orm.uds.{Dataset => UdsDataset, MasterQuantitationChannel => UdsMasterQuantChannel}
import fr.proline.core.service.msq.quantify.AggregationQuantifier
import fr.proline.core.service.uds.{CreateQuantitation, Quantifier}
import fr.proline.cortex.api.service.dps.msq.IAggregateQuantitationService
import fr.proline.cortex.service.SingleThreadIdentifierType
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.jms.service.api.{AbstractRemoteProcessingService, ISingleThreadedService}

import scala.collection.JavaConversions.asScalaBuffer

/**
 *  Define JMS Service which allows to creates a new quantitation and perform the corresponding data analysis.
 *
 *  Input params :
 *    name : Name of the quantitation dataset that will be created for this quantitation.
 *    description: Description of the quantitation dataset that will be created for this quantitation.
 *    project_id: The id of the project the quantitation will be created in
 *    experimental_design: The experimental design related to this quantitation.
 *    quantitation_config: The parameters to use in order to perform a the aggregation.
 *
 *  Output params :
 *    Boolean for service run status
 */
class AggregateQuantitation extends AbstractRemoteProcessingService with IAggregateQuantitationService with LazyLogging with ISingleThreadedService {

  /* JMS Service identification */
  val singleThreadIdent: String = SingleThreadIdentifierType.QUANTIFY_SINGLETHREAD_IDENT.toString

  def doProcess(paramsRetriever: NamedParamsRetriever): Any = {

    require(paramsRetriever != null, "no parameter specified")

    val projectId = paramsRetriever.getLong(PROCESS_METHOD.PROJECT_ID_PARAM)
    val experimentalDesign = deserialize[ExperimentalDesign](serialize(paramsRetriever.getMap(PROCESS_METHOD.EXPERIMENTAL_DESIGN_PARAM)))
    val quantConfigAsMap = paramsRetriever.getMap(PROCESS_METHOD.QUANTITATION_CONFIG_PARAM)

    val execCtx = DbConnectionHelper.createJPAExecutionContext(projectId) // Use JPA context
    val executionContext = ProviderDecoratedExecutionContext(execCtx) // Use Object factory

    var quantiId = -1L

    try {
      // Isolate future actions in an SQL transaction
      executionContext.tryInTransactions(udsTx = true, msiTx = true, txWork = {
        // Store quantitation in the UDSdb
        val quantiCreator = new CreateQuantitation(
          executionContext = executionContext,
          name = paramsRetriever.getString(PROCESS_METHOD.NAME_PARAM),
          description = paramsRetriever.getString(PROCESS_METHOD.DESCRIPTION_PARAM),
          projectId = projectId,
          methodId = 1, // TODO: this should not be done like this : extract method from aggregated datasets
          experimentalDesign = experimentalDesign
        )
        quantiCreator.runService()
        quantiId = quantiCreator.getUdsQuantitation.getId

        // Retrieve entity manager
        val udsDbCtx = executionContext.getUDSDbConnectionContext()
        val udsEM = udsDbCtx.getEntityManager()
        val udsQuantitation = udsEM.find(classOf[UdsDataset], quantiId)

        // Retrieve master quant channels (they should be sorted by their number)
        val udsMasterQuantChannels = udsQuantitation.getMasterQuantitationChannels.toList

        // Parse the quant configuration
        val quantConfigAsStr = serialize(quantConfigAsMap)
        val quantConfig = deserialize[AggregationQuantConfig](quantConfigAsStr)

        require(!quantConfig.quantitationIds.isEmpty, "List of datasets to aggregate cannot be empty")

        // Store QUANT CONFIG in ObjectTree
        logger.info("Storing quantitation configuration with schema named: " + UdsSchemaName.AGGREGATION_QUANT_CONFIG.toString())
        val qtConfigObjectTree = Quantifier.storeQuantConfig(quantConfigAsStr, UdsSchemaName.AGGREGATION_QUANT_CONFIG, udsEM)

        // TODO remove me if the final decision is to enable multiple aggregation of a same quant dataset
        //Link child quantitation datasets to the newly created one
//        for (childQuantitationId <- quantConfig.quantitationIds) {
//          val childQuantitation = udsEM.find(classOf[UdsDataset], childQuantitationId)
//          childQuantitation.setParentDataset(udsQuantitation)
//          udsEM.merge(childQuantitation)
//        }

        // Link QUANT CONFIG to quantitation DS
        udsQuantitation.putObject(UdsSchemaName.AGGREGATION_QUANT_CONFIG.toString(), qtConfigObjectTree.getId())
        udsEM.merge(udsQuantitation)

        //Perform the aggregation
        aggregateMasterQuantChannels(executionContext, experimentalDesign, udsMasterQuantChannels, quantConfig)

      })
    } catch {
      case t: Throwable => {
        throw ExceptionUtils.wrapThrowable("Error while quantifying the dataset", t, appendCause = true)
      }
    } finally {
      DbConnectionHelper.tryToCloseExecContext(execCtx)
    }

    quantiId
  }

  protected def aggregateMasterQuantChannels(executionContext: IExecutionContext,
                                             experimentalDesign: ExperimentalDesign,
                                             udsMasterQuantChannels: List[UdsMasterQuantChannel],
                                             aggQuantConfig: AggregationQuantConfig) {

    // Quantify each master quant channel
    for (udsMasterQuantChannel <- udsMasterQuantChannels) {

      val quantifier = new AggregationQuantifier(
        executionContext = executionContext,
        experimentalDesign = experimentalDesign,
        udsMasterQuantChannel = udsMasterQuantChannel,
        quantConfig = aggQuantConfig
      )

      quantifier.quantify()

    }
  }

}
