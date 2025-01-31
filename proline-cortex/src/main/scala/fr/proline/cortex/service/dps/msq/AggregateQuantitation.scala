package fr.proline.cortex.service.dps.msq

import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.exception.ExceptionUtils
import fr.profi.util.serialization.ProfiJson.{deserialize, serialize}
import fr.proline.context.IExecutionContext
import fr.proline.core.algo.msq.config.AggregationQuantConfig
import fr.proline.core.dal.context.execCtxToTxExecCtx
import fr.proline.core.om.model.msq.{AbundanceUnit, ExperimentalDesign, QuantMethodType}
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.orm.uds.ObjectTreeSchema.{SchemaName => UdsSchemaName}
import fr.proline.core.orm.uds.repository.QuantitationMethodRepository
import fr.proline.core.orm.uds.{QuantitationLabel, QuantitationMethod, Dataset => UdsDataset, MasterQuantitationChannel => UdsMasterQuantChannel}
import fr.proline.core.service.msq.quantify.AggregationQuantifier
import fr.proline.core.service.uds.{CreateQuantitation, Quantifier}
import fr.proline.cortex.api.service.dps.msq.IAggregateQuantitationService
import fr.proline.cortex.service.SingleThreadIdentifierType
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.jms.service.api.{AbstractRemoteProcessingService, ISingleThreadedService}

import scala.collection.JavaConverters._

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

        // Parse the quant configuration
        val quantConfigAsStr = serialize(quantConfigAsMap)
        val quantConfig = deserialize[AggregationQuantConfig](quantConfigAsStr)

        // Retrieve entity manager
        val udsDbCtx = executionContext.getUDSDbConnectionContext()
        val udsEM = udsDbCtx.getEntityManager()

        //Suppose that exp. design is valid : if quantLabelId is define, all labels must belongs to same quantMethod (at least same method type...)
        val qLabelId = experimentalDesign.masterQuantChannels(0).quantChannels(0).quantLabelId
        var qMethod : QuantitationMethod = null
        if(qLabelId.isDefined) {
          val qLabel = udsEM.find(classOf[QuantitationLabel], qLabelId.get)
          if(qLabel != null)
            qMethod= qLabel.getMethod
        }
        if(qMethod == null){//find no method. set default
          qMethod = QuantitationMethodRepository.findQuantMethodForTypeAndAbundanceUnit(udsEM, QuantMethodType.LABEL_FREE.toString, AbundanceUnit.FEATURE_INTENSITY);
        }

        // Store quantitation in the UDSdb
        val quantiCreator = new CreateQuantitation(
          executionContext = executionContext,
          name = paramsRetriever.getString(PROCESS_METHOD.NAME_PARAM),
          description = paramsRetriever.getString(PROCESS_METHOD.DESCRIPTION_PARAM),
          projectId = projectId,
          methodId = qMethod.getId,
          experimentalDesign = experimentalDesign
        )
        quantiCreator.runService()
        quantiId = quantiCreator.getUdsQuantitation.getId

        // Retrieve entity manager
        //val udsDbCtx = executionContext.getUDSDbConnectionContext()
        //val udsEM2 = udsDbCtx.getEntityManager()
        val udsQuantitation = udsEM.find(classOf[UdsDataset], quantiId)

        // Retrieve master quant channels (they should be sorted by their number)
        val udsMasterQuantChannels = udsQuantitation.getMasterQuantitationChannels.asScala.toList


        require(!quantConfig.quantitationIds.isEmpty, "List of datasets to aggregate cannot be empty")

        // Store QUANT CONFIG in ObjectTree
        logger.info("Storing quantitation configuration with schema named: " + UdsSchemaName.AGGREGATION_QUANT_CONFIG.getKeyName())
        val qtConfigObjectTree = Quantifier.storeQuantConfig(quantConfigAsStr, UdsSchemaName.AGGREGATION_QUANT_CONFIG, udsEM)

        // TODO remove me if the final decision is to enable multiple aggregation of a same quant dataset
        //Link child quantitation datasets to the newly created one
//        for (childQuantitationId <- quantConfig.quantitationIds) {
//          val childQuantitation = udsEM.find(classOf[UdsDataset], childQuantitationId)
//          childQuantitation.setParentDataset(udsQuantitation)
//          udsEM.merge(childQuantitation)
//        }

        // Link QUANT CONFIG to quantitation DS
        udsQuantitation.putObject(UdsSchemaName.AGGREGATION_QUANT_CONFIG.getKeyName(), qtConfigObjectTree.getId())
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

  private def aggregateMasterQuantChannels(executionContext: IExecutionContext,
                                           experimentalDesign: ExperimentalDesign,
                                           udsMasterQuantChannels: List[UdsMasterQuantChannel],
                                           aggQuantConfig: AggregationQuantConfig): Unit = {

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
