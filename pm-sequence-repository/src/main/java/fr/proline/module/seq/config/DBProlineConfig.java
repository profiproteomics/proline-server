package fr.proline.module.seq.config;

import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigException;
import com.typesafe.config.ConfigFactory;

import fr.proline.core.orm.uds.ExternalDb;
import fr.proline.repository.AbstractDatabaseConnector;
import fr.proline.repository.ConnectionMode;
import fr.proline.repository.DriverType;
import fr.proline.repository.ProlineDatabaseType;

public class DBProlineConfig {

    private static final Logger LOG = LoggerFactory.getLogger(DBProlineConfig.class);

    private static final Object CONFIGURATION_LOCK = new Object();

    private static DBProlineConfig instance;
    private Config m_dbProlineConfig = null;
    private ExternalDb m_udsExternalDB = null;
    private Integer m_maxPoolConnection = null;
    private String auth_user = null;
    private String auth_password = null;

    private DBProlineConfig() {
        m_dbProlineConfig = ConfigFactory.load("application");
    }

    public static DBProlineConfig getInstance() {
        if (instance == null) {
            instance = new DBProlineConfig();
            instance.initUDSProperties();
        }
        return instance;
    }

    public static void forcePropertiesFileReload() {
        synchronized (CONFIGURATION_LOCK) {
            if (instance != null) {
                instance.m_udsExternalDB = null;
                instance.m_maxPoolConnection = null;
            }
        }
    }

    public Integer getMaxPoolConnection() {
        synchronized (CONFIGURATION_LOCK) {
            if (m_maxPoolConnection == null && m_dbProlineConfig.hasPath("proline-config")) {
                Config prolineConfig = m_dbProlineConfig.getConfig("proline-config");
                if (prolineConfig.hasPath("max-pool-connection")) {
                    m_maxPoolConnection = Integer.valueOf(prolineConfig.getInt("max-pool-connection"));
                } else {
                    m_maxPoolConnection = Math.max(1, AbstractDatabaseConnector.DEFAULT_MAX_POOL_CONNECTIONS / 2); //moitie moins que le serveur...
                }
            }
        }

        return m_maxPoolConnection;
    }

    public String getDbPath() {
        Config dbConfig = m_dbProlineConfig.getConfig("uds-db").getConfig("connection-properties");
        String path = "";
        if(dbConfig.hasPath("dbPath"))
            path = dbConfig.hasPath("dbPath") ?  dbConfig.getString("dbPath") : "";
        return path;
    }

    private void initUDSProperties() {
        m_udsExternalDB = new ExternalDb();
        try {
            Config prolineConfig = m_dbProlineConfig.getConfig("proline-config");
            String driverType = prolineConfig.getString("driver-type");

            Config driverConfig = m_dbProlineConfig.getConfig(driverType + "-config");
            Config driverConnectionConfig = driverConfig.getConfig("connection-properties");
            DriverType driver = DriverType.valueOf(driverType.toUpperCase());
            m_udsExternalDB.setDriverType(driver);

            Config dbConfig = m_dbProlineConfig.getConfig("uds-db").getConfig("connection-properties");
            m_udsExternalDB.setDbName(dbConfig.getString("dbName"));

            Config authConfig = m_dbProlineConfig.getConfig("auth-config");
//			m_udsExternalDB.setDbPassword(authConfig.getString("password"));
//			m_udsExternalDB.setDbUser(authConfig.getString("user"));
            auth_user = authConfig.getString("user");
            auth_password = authConfig.getString("password");

            Config hostConfig = m_dbProlineConfig.getConfig("host-config");
            m_udsExternalDB.setHost(hostConfig.getString("host"));
            if (hostConfig.hasPath("port")) {
                m_udsExternalDB.setPort(hostConfig.getInt("port"));
            }

            m_udsExternalDB.setType(ProlineDatabaseType.UDS);
            m_udsExternalDB.setConnectionMode(ConnectionMode.valueOf(driverConnectionConfig.getString("connectionMode").toUpperCase()));
        } catch (ConfigException ce) {
            LOG.warn(" No UDS configuration found ! ");
        }
    }

    public Map<Object, Object> getUDSProperties() {
        Map<Object, Object> result = null;
        synchronized (CONFIGURATION_LOCK) {
            if (m_udsExternalDB == null) {
                initUDSProperties();
            }
        }
        try {
            result = m_udsExternalDB.toPropertiesMap(auth_user, auth_password);
        } catch (Exception e) {
            LOG.warn(" Error getting UDS External DB properties.");
            result = new HashMap<Object, Object>();
        }
        return result;
    }

    public ConnectionMode getUDSConnectionMode(){
        return m_udsExternalDB.getConnectionMode();
    }

    public ExternalDb getExternalDBTemplate() {
        ExternalDb externalDB = new ExternalDb();
        externalDB.setConnectionMode(m_udsExternalDB.getConnectionMode());
        externalDB.setHost(m_udsExternalDB.getHost());
        externalDB.setPort(m_udsExternalDB.getPort());
        externalDB.setDriverType(m_udsExternalDB.getDriverType());

        return externalDB;
    }

}
