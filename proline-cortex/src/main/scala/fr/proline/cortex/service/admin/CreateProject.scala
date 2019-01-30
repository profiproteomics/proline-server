package fr.proline.cortex.service.admin

import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging

import fr.proline.admin.service.db.CreateProjectDBs
import fr.proline.admin.service.db.SetupProline
import fr.proline.admin.service.user.ProjectUtils
import fr.proline.context.DatabaseConnectionContext
import fr.proline.cortex.api.service.admin.ICreateProjectService
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.jms.service.api.AbstractRemoteProcessingService

/**
 * JMS Service to create a new Project in current Proline DataStore.
 * This Service will create Project data in UDS DB and create necessary project databases (msi, lcms...)
 *
 * Input params :
 *  name : "The project name" (String)
 *  description :  "The project description" (String)
 *  owner_id : "The project owner ID" (Long)
 *
 * Output params :
 *  new project Id.
 *
 */
class CreateProject extends AbstractRemoteProcessingService with ICreateProjectService with LazyLogging {

  //PARAM Description
  // "name" : "The project name" : String
  // "description" :  "The project description": String
  // "owner_id" : "The project owner ID" : Long

  def doProcess(paramsRetriever: NamedParamsRetriever): Any = {

    require(paramsRetriever != null, "no parameter specified")
    require(paramsRetriever.hasParam(PROCESS_METHOD.NAME_PARAM) && paramsRetriever.hasParam(PROCESS_METHOD.OWNER_ID_PARAM), "Project name and owner requiered")

    val description = paramsRetriever.getOptString(PROCESS_METHOD.DESCRIPTION_PARAM, true, "")
    val name = paramsRetriever.getString(PROCESS_METHOD.NAME_PARAM)
    var prjID: Long = -1L
    val connectorFactory = DbConnectionHelper.getDataStoreConnectorFactory()
    val udsDbConnectionContext = new DatabaseConnectionContext(connectorFactory.getUdsDbConnector)

    try {

      // Create Data entries in UDS
      val createPrj = new fr.proline.admin.service.user.CreateProject(connectorFactory, udsDbConnectionContext, name, description, paramsRetriever.getLong("owner_id"), createDbs = true)
      logger.debug("Create Project Object " + name)
      createPrj.doWork()
      prjID = createPrj.projectId

      if (prjID <= 0)
        throw new RuntimeException("Error creating project (" + prjID + ")")

    } finally {
      DbConnectionHelper.tryToCloseDbContext(udsDbConnectionContext)
    }

    prjID.asInstanceOf[Object]
  }

}