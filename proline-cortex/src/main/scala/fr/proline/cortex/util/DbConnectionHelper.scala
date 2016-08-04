package fr.proline.cortex.util

import com.typesafe.scalalogging.LazyLogging

import fr.proline.admin.service.db.SetupProline
import fr.proline.context.IExecutionContext
import fr.proline.core.dal.BuildLazyExecutionContext
import fr.proline.core.orm.util.DataStoreConnectorFactory
import fr.proline.jms.util.NodeConfig
import fr.proline.repository.IDataStoreConnectorFactory
import fr.proline.repository.IDatabaseConnector
import fr.proline.repository.ProlineDatabaseType

object DbConnectionHelper extends LazyLogging {

  private var m_dsConnectorFactory: IDataStoreConnectorFactory = null

  def initDataStore() {
    m_dsConnectorFactory = DataStoreConnectorFactory.getInstance()

    if (!m_dsConnectorFactory.isInitialized) {
      val prolineConfig = SetupProline.config

      val udsDbProperties = prolineConfig.udsDBConfig.dbConnProperties

      logger.debug("Initializing DataStoreConnectorFactory from UDS Db Properties")

      m_dsConnectorFactory.asInstanceOf[DataStoreConnectorFactory].initialize(udsDbProperties, "Proline Cortex " + NodeConfig.NODE_ID)
    }

  }

  def getDataStoreConnectorFactory() = {
    if (m_dsConnectorFactory == null) {
      initDataStore()
    }
    m_dsConnectorFactory
  }

  def createJPAExecutionContext(projectID: Long): IExecutionContext = {
    createExecutionContext(projectID, true)
  }

  def createSQLExecutionContext(projectID: Long): IExecutionContext = {
    createExecutionContext(projectID, false)
  }

  private def createExecutionContext(projectID: Long, useJPA: Boolean): IExecutionContext = {
    val onConnectionContextClose = { dbConnector: IDatabaseConnector =>
      
      logger.info("onConnectionContextClose called ... ")
      
      if ((dbConnector.getOpenConnectionCount == 0) && (dbConnector.getOpenEntityManagerCount == 0)) {
        
        val dbType = dbConnector.getProlineDatabaseType
        logger.info("db type =  " + dbType)
        
        if (dbType == ProlineDatabaseType.LCMS) {
          logger.info(s"Closing database connector for LCMSdb with project id=$projectID (EntityManagerCount equals zero)")
          m_dsConnectorFactory.closeLcMsDbConnector(projectID)
        } else if (dbType == ProlineDatabaseType.MSI) {
          logger.info(s"Closing database connector for MSIdb with project id=$projectID (EntityManagerCount equals zero)")
          m_dsConnectorFactory.closeMsiDbConnector(projectID)
        }
      }

    }

    BuildLazyExecutionContext(DbConnectionHelper.getDataStoreConnectorFactory, projectID, useJPA, Some(onConnectionContextClose))
  }

}