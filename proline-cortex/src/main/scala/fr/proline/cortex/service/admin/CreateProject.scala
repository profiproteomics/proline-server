package fr.proline.cortex.service.admin

import fr.proline.cortex.service.AbstractRemoteProcessService
import com.typesafe.scalalogging.LazyLogging
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import fr.proline.context.DatabaseConnectionContext
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.admin.service.db.CreateProjectDBs
import fr.proline.admin.service.db.SetupProline

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
class CreateProject extends AbstractRemoteProcessService with LazyLogging {

  /* JMS Service identification */
  override val serviceName = "proline/admin/CreateProject"
  override val defaultVersion = true
  override val serviceVersion = "1.0"

  //PARAM Description
  // "name" : "The project name" : String   
  // "description" :  "The project description": String
  // "owner_id" : "The project owner ID" : Long

  override def doProcess(paramsRetriever: NamedParamsRetriever): Object = {

    require(paramsRetriever.hasParam("name") && paramsRetriever.hasParam("owner_id"), "Project name and owner requiered")

    val description = if (paramsRetriever.hasParam("description")) paramsRetriever.getString("description") else ""
    val name = paramsRetriever.getString("name")
    var prjID: Long = -1L
    
    val udsDbConnectionContext: DatabaseConnectionContext = new DatabaseConnectionContext(DbConnectionHelper.getIDataStoreConnectorFactory.getUdsDbConnector())

    try {
      
      // Create Data entries in UDS
      val createPrj = new fr.proline.admin.service.user.CreateProject(udsDbConnectionContext, name, description, paramsRetriever.getLong("owner_id"))
      logger.debug("Create Project Object " + name)
      createPrj.doWork()
      prjID = createPrj.projectId

      if (prjID <= 0)
        throw new RuntimeException("Error creating project (" + prjID + ")")

      // Create project databases
      logger.debug("Create Project databases for project " + prjID)
      new CreateProjectDBs(udsDbConnectionContext, SetupProline.config, prjID).doWork()

    } finally {

      try {
        udsDbConnectionContext.close()
      } catch {
        case exClose: Exception => logger.error("Error closing UDS Db Context", exClose)
      }

    }

    prjID.asInstanceOf[Object]
  }

}