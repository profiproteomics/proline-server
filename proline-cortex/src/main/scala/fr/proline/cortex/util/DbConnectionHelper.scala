package fr.proline.cortex.util

import com.typesafe.scalalogging.LazyLogging
import fr.proline.admin.service.db.SetupProline
import fr.proline.core.orm.util.DataStoreConnectorFactory
import fr.proline.repository.IDataStoreConnectorFactory
import fr.proline.cortex.NodeConfig
import fr.proline.context.IExecutionContext
import fr.proline.core.dal.BuildExecutionContext

object DbConnectionHelper extends LazyLogging {

  private var m_dsConnectorFactory: IDataStoreConnectorFactory = null;

  def initDataStore() {
    m_dsConnectorFactory = DataStoreConnectorFactory.getInstance()

    if (!m_dsConnectorFactory.isInitialized) {
      val prolineConfig = SetupProline.config

      val udsDbProperties = prolineConfig.udsDBConfig.dbConnProperties

      logger.debug("Initializing DataStoreConnectorFactory from UDS Db Properties")

      m_dsConnectorFactory.asInstanceOf[DataStoreConnectorFactory].initialize(udsDbProperties, "Proline Cortex " + NodeConfig.NODE_ID)
    }

  }

  def getIDataStoreConnectorFactory() = {
    if (m_dsConnectorFactory == null) {
      initDataStore()
    }
    m_dsConnectorFactory
  }

  def createJPAExecutionContext(projectID: Long): IExecutionContext = {
    BuildExecutionContext(DbConnectionHelper.getIDataStoreConnectorFactory, projectID, true)
  }

}