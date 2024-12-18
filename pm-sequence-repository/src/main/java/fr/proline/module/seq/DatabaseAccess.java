package fr.proline.module.seq;

import fr.proline.core.orm.uds.ExternalDb;
import fr.proline.core.orm.uds.repository.ExternalDbRepository;
import fr.proline.core.orm.util.DStoreCustomPoolConnectorFactory;
import fr.proline.module.seq.config.DBProlineConfig;
import fr.proline.repository.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.persistence.EntityManager;
import javax.persistence.EntityTransaction;
import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Map;

/**
 * Handle <code>DataStoreConnectorFactory</code> and DatabaseConnector keeping and initialization for Sequence-Repository retrieve Service and Provider.
 * <p>
 * This factory is thread-safe (internally synchronized).
 * 
 * @author LMN
 * 
 */
public final class DatabaseAccess {

	private static final Logger LOG = LoggerFactory.getLogger(DatabaseAccess.class);

	private static final String SEQ_DB_NAME = "seq_db";

	private static final Object INITIALIZATION_LOCK = new Object();

	/* All mutable fields and their initialization are @GuardedBy("INITIALIZATION_LOCK") */

	private static IDataStoreConnectorFactory connectorFactory;

	private static IDatabaseConnector seqDatabaseConnector;

	/* Private constructor (Utility class) */
	private DatabaseAccess() {
	}

	public static void initDataStoreConnectorFactory() {
		synchronized (INITIALIZATION_LOCK) {
			IDataStoreConnectorFactory result = DStoreCustomPoolConnectorFactory.getInstance();

			if (!result.isInitialized()) {
				/* Initialization holding INITIALIZATION_LOCK */
				DBProlineConfig.forcePropertiesFileReload();				
				final Map<Object, Object> udsDbConfigProperties = DBProlineConfig.getInstance().getUDSProperties();				
				if (udsDbConfigProperties.isEmpty()) {
					throw new IllegalArgumentException("No valid UDS Db Properties");
				} else {
					Integer maxPoolConnection = DBProlineConfig.getInstance().getMaxPoolConnection();				
					udsDbConfigProperties.put(AbstractDatabaseConnector.PROLINE_MAX_POOL_CONNECTIONS_KEY, maxPoolConnection);
					LOG.debug("Initializing DataStoreConnectorFactory using properties [{}] ",
						udsDbConfigProperties);
					((DStoreCustomPoolConnectorFactory) result).initialize(udsDbConfigProperties, "SequenceRepository"); //VDS #16961, ConnectionPoolType.SIMPLE_POOL_MANAGEMENT);
				}
			}
			connectorFactory = result;
		}
	}

	public static IDataStoreConnectorFactory getDataStoreConnectorFactory() {
		IDataStoreConnectorFactory result;

		synchronized (INITIALIZATION_LOCK) {

			if (connectorFactory == null) {
				/* Initialization holding INITIALIZATION_LOCK */
				initDataStoreConnectorFactory();
			}
			result = connectorFactory;

		} // End of synchronized block on INITIALIZATION_LOCK

		return result;
	}

	private static IDataStoreConnectorFactory getDataStoreConnectorFactory(final Map<Object, Object> udsDbProperties) {
		IDataStoreConnectorFactory result = null;

		synchronized (INITIALIZATION_LOCK) {

			if (connectorFactory == null) {
				result = DStoreCustomPoolConnectorFactory.getInstance();

				if (!result.isInitialized()) {
					/* Initialization holding INITIALIZATION_LOCK */

					if (udsDbProperties == null || udsDbProperties.isEmpty()) {
						throw new IllegalArgumentException("No valid UDS Db Properties");
					} else {
						if(!udsDbProperties.containsKey(AbstractDatabaseConnector.PROLINE_MAX_POOL_CONNECTIONS_KEY)){
							Integer maxPoolConnection = DBProlineConfig.getInstance().getMaxPoolConnection();				
							udsDbProperties.put(AbstractDatabaseConnector.PROLINE_MAX_POOL_CONNECTIONS_KEY, maxPoolConnection);
						}
						LOG.debug("Initializing DataStoreConnectorFactory from [{}] properties",
							udsDbProperties);
						((DStoreCustomPoolConnectorFactory) result).initialize(udsDbProperties, "SequenceRepository"); //VDS #16961, ConnectionPoolType.SIMPLE_POOL_MANAGEMENT);
					}
				} else {
					LOG.warn("DataStoreConnectorFactory was already initialized. Properties [{}] was ignore", udsDbProperties);
				}
				connectorFactory = result;
			} else {
				LOG.warn("DataStoreConnectorFactory was already initialized. Properties [{}] was ignore", udsDbProperties);
				result = connectorFactory;
			}

		} // End of synchronized block on INITIALIZATION_LOCK

		return result;
	}

	public static void closeSEQDatabaseConnector() {
		synchronized (INITIALIZATION_LOCK) {
			if (seqDatabaseConnector != null) {
				seqDatabaseConnector.close();
				seqDatabaseConnector = null;
				connectorFactory = null;
			}
		}
	}

	/**
	 * Retrieve current SEQ DatabaseConnector instance.
	 * 
	 * @param allowCreateUpdateDB
	 *            <code>true</code> if called by service (lazy initialization of SEQ Databank) or <code>false</code> if called by client (provider use)
	 * @return SEQ DatabaseConnector or <code>null</code> if DatabaseConnector cannot be initialized
	 */
	public static IDatabaseConnector getSEQDatabaseConnector(final boolean allowCreateUpdateDB) {
		IDatabaseConnector result = null;

		synchronized (INITIALIZATION_LOCK) {

			if (seqDatabaseConnector == null) {
				result = createSEQDatabaseConnector(allowCreateUpdateDB, null);

				seqDatabaseConnector = result;
			} else {
				result = seqDatabaseConnector;
			}

		} // End of synchronized block on INITIALIZATION_LOCK

		return result;
	}

	/**
	 * Retrieve current SEQ DatabaseConnector instance.
	 * 
	 * @param allowCreateUpdateDB
	 *            <code>true</code> if called by service (lazy initialization of SEQ Databank) or <code>false</code> if called by client (provider use)
	 * @param udsDbProperties
	 *            Properties to use to create UDSConnector
	 * @return SEQ DatabaseConnector or <code>null</code> if DatabaseConnector cannot be initialized
	 */
	public static IDatabaseConnector getSEQDatabaseConnector(final boolean allowCreateUpdateDB, final Map<Object, Object> udsDbProperties) {
		IDatabaseConnector result = null;

		synchronized (INITIALIZATION_LOCK) {

			if (seqDatabaseConnector == null) {
				result = createSEQDatabaseConnector(allowCreateUpdateDB, udsDbProperties);

				seqDatabaseConnector = result;
			} else {
				result = seqDatabaseConnector;
			}

		} // End of synchronized block on INITIALIZATION_LOCK

		return result;
	}

	/* Called holding INITIALIZATION_LOCK */
	private static IDatabaseConnector createSEQDatabaseConnector(final boolean allowCreateUpdateDB, final Map<Object, Object> udsDbProperties) {
		IDatabaseConnector seqDbConnector = null;
		IDataStoreConnectorFactory dataStoreConnectorFactory = null;
		if (udsDbProperties != null && !udsDbProperties.isEmpty())
			dataStoreConnectorFactory = getDataStoreConnectorFactory(udsDbProperties);
		else
			dataStoreConnectorFactory = getDataStoreConnectorFactory();
		final IDatabaseConnector udsDbConnector = dataStoreConnectorFactory.getUdsDbConnector();

		EntityManager udsEM = udsDbConnector.createEntityManager();

		try {
			/* Try to load SEQ Db Connector */
			
      ExternalDb seqDb = ExternalDbRepository.findExternalByType(udsEM, ProlineDatabaseType.SEQ);
			if (seqDb == null) {

				if (allowCreateUpdateDB) {
					LOG.info("No ExternalDb for SEQ Db creating a new one");
					try {
						seqDbConnector = createSEQDatabase(udsDbConnector, udsEM);
					} catch (SQLException e) {
						throw new RuntimeException("Error creating SEQ Db "+e);						
					}
				} else {
					throw new RuntimeException("SEQ Db does not exist");
				}

			} else {
				boolean cheksumrepair=false;
				Map<Object,Object> dbProperties  = seqDb.toPropertiesMap(udsDbConnector.getDriverType(),
						(String) udsDbConnector.getProperty(AbstractDatabaseConnector.PERSISTENCE_JDBC_USER_KEY),
						(String) udsDbConnector.getProperty(AbstractDatabaseConnector.PERSISTENCE_JDBC_PASSWORD_KEY));
				Integer maxPoolConnection = DBProlineConfig.getInstance().getMaxPoolConnection();
				dbProperties.put(AbstractDatabaseConnector.PROLINE_MAX_POOL_CONNECTIONS_KEY, maxPoolConnection);
				
				//VDS #16961 seqDbConnector = DatabaseConnectorFactory.createDatabaseConnectorInstance(ProlineDatabaseType.SEQ, dbProperties, ConnectionPoolType.SIMPLE_POOL_MANAGEMENT);
				seqDbConnector = DatabaseConnectorFactory.createDatabaseConnectorInstance(ProlineDatabaseType.SEQ, dbProperties);
				if (allowCreateUpdateDB) {
					int retDBUpgrade = DatabaseUpgrader.upgradeDatabase(seqDbConnector,cheksumrepair);
					if(retDBUpgrade<0)
						throw new RuntimeException("Error accessing Seq DB for upgrade");
				}
			}		     			

		} finally {

			if (udsEM != null) {
				try {
					udsEM.close();
				} catch (Exception exClose) {
					LOG.error("Error closing UDS Db EntityManager", exClose);
				}
			}

		}

		return seqDbConnector;
	}

	/* TODO Do this in Proline-Admin ? */
	private static IDatabaseConnector createSEQDatabase(
		final IDatabaseConnector udsDbConnector,
		final EntityManager udsEM) throws SQLException {
		ExternalDb seqDb = null;

		EntityTransaction udsTransac = null;
		boolean transacOK = false;

		try {
			udsTransac = udsEM.getTransaction();
			udsTransac.begin();
			transacOK = false;

			final DriverType udsDriverType = udsDbConnector.getDriverType();
                        

			if (udsDriverType == DriverType.H2) {
				// RAW ExternalDb creation
				// TODO create structures required by setConnectionMode (if file or server mode)
				ConnectionMode mode = DBProlineConfig.getInstance().getUDSConnectionMode();
				String path ="";
				switch (mode){
					case FILE:
						path = DBProlineConfig.getInstance().getDbPath();
						break;
				}

				seqDb = createSEQDb(udsEM, path+SEQ_DB_NAME);
			} else if (udsDriverType == DriverType.POSTGRESQL) {
				final boolean success = createPgSEQDatabase(udsDbConnector);

				if (success) {
					seqDb = createSEQDb(udsEM, SEQ_DB_NAME);
				} else {
					LOG.error("Unable to create Postgres SEQ Databank");
				}

			} else {
				LOG.error("Unsupported SGBD type {}", udsDriverType);
			}

			udsTransac.commit();
			transacOK = true;
		} finally {

			if ((udsTransac != null) && !transacOK) {
				try {
					udsTransac.rollback();
				} catch (Exception ex) {
					LOG.error("Error rollbacking UDS Db EntityManager Transaction", ex);
				}
			}

		}

		IDatabaseConnector seqDbConnector = null;
        boolean cheksumrepair=false;
		if (transacOK && (seqDb != null)) {
			Map<Object,Object> dbProperties  = seqDb.toPropertiesMap((String) udsDbConnector.getProperty(AbstractDatabaseConnector.PERSISTENCE_JDBC_USER_KEY),
					(String) udsDbConnector.getProperty(AbstractDatabaseConnector.PERSISTENCE_JDBC_PASSWORD_KEY));
			Integer maxPoolConnection = DBProlineConfig.getInstance().getMaxPoolConnection();
			dbProperties.put(AbstractDatabaseConnector.PROLINE_MAX_POOL_CONNECTIONS_KEY, maxPoolConnection);

			//VDS #16961 seqDbConnector = DatabaseConnectorFactory.createDatabaseConnectorInstance( ProlineDatabaseType.SEQ, dbProperties, ConnectionPoolType.SIMPLE_POOL_MANAGEMENT);
			seqDbConnector = DatabaseConnectorFactory.createDatabaseConnectorInstance( ProlineDatabaseType.SEQ, dbProperties);

			DatabaseUpgrader.upgradeDatabase(seqDbConnector,cheksumrepair);
		}

		return seqDbConnector;
	}

	private static boolean createPgSEQDatabase(final IDatabaseConnector udsDbConnector) throws SQLException {
		boolean success = false;

		Connection sqlCon = null;

		try {
			final DataSource ds = udsDbConnector.getDataSource();

			sqlCon = ds.getConnection();

			final long count = countExistingSEQDb(sqlCon);

			if (count > 0L) {
				LOG.info("{} already existing", SEQ_DB_NAME);

				success = true;
			} else {
				final Statement stmt = sqlCon.createStatement();

				final int result = stmt.executeUpdate("CREATE DATABASE " + SEQ_DB_NAME);

				stmt.close();

				if (result >= 0) {
					LOG.info("New Postgres {} database created", SEQ_DB_NAME);

					success = true;
				} else {
					LOG.error("Error creating Postgres SEQ Databank : {}", result);
				}

			}

//		} catch (Exception ex) {
//			LOG.error("Error creating Postgres SEQ Databank", ex);
		} finally {

			if (sqlCon != null) {
				try {
					sqlCon.close();
				} catch (SQLException exClose) {
					success = false;
					LOG.error("Error closing UDS Db SQL Connection", exClose);
				}
			}

		}

		return success;
	}

	/* Simple clone from PS Db ExternalDb configuration */
	private static ExternalDb createSEQDb(final EntityManager udsEM, String dbName) {
		ExternalDb seqDb = DBProlineConfig.getInstance().getExternalDBTemplate();
		seqDb.setDbName(dbName);
		seqDb.setDbVersion("V0.2");  //FIXME get information from seqDb ?!
		seqDb.setType(ProlineDatabaseType.SEQ);
		udsEM.persist(seqDb);

		LOG.debug("New SEQ ExternalDb persisted");
		return seqDb;
	}

	private static long countExistingSEQDb(final Connection sqlCon) throws SQLException {
		long result = -1L;

		final PreparedStatement stmt = sqlCon
				.prepareStatement("SELECT COUNT (*) FROM pg_database WHERE UPPER(datname) = ?");
		stmt.setString(1, SEQ_DB_NAME.toUpperCase());

		final ResultSet rs = stmt.executeQuery();

		if (rs.next()) {
			final Object obj = rs.getObject(1);

			if (obj instanceof Number) {
				result = ((Number) obj).longValue();
			}

		}

		stmt.close();// Close ResultSet and Statement
		// sqlCon is closed in finally block of createPgSEQDatabase()

		return result;
	}

}
