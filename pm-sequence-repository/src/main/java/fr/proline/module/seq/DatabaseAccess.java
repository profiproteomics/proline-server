package fr.proline.module.seq;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.EntityTransaction;
import javax.sql.DataSource;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import fr.profi.util.StringUtils;
import fr.proline.core.orm.uds.ExternalDb;
import fr.proline.core.orm.uds.repository.ExternalDbRepository;
import fr.proline.core.orm.util.DStoreCustomPoolConnectorFactory;
import fr.proline.repository.DatabaseConnectorFactory;
import fr.proline.repository.DatabaseUpgrader;
import fr.proline.repository.DriverType;
import fr.proline.repository.IDataStoreConnectorFactory;
import fr.proline.repository.IDatabaseConnector;
import fr.proline.repository.ProlineDatabaseType;

/**
 * Handle <code>DataStoreConnectorFactory</code> and DatabaseConnector keeping and initialization for
 * Sequence-Repository retrieve Service and Provider.
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

    public static IDataStoreConnectorFactory getDataStoreConnectorFactory() {
	IDataStoreConnectorFactory result = null;

	synchronized (INITIALIZATION_LOCK) {

	    if (connectorFactory == null) {
		result = DStoreCustomPoolConnectorFactory.getInstance();

		if (!result.isInitialized()) {
		    /* Initialization holding INITIALIZATION_LOCK */
		    ServiceConfiguration.forcePropertiesFileReload();
		    final String udsDbConfigFileName = ServiceConfiguration.getUDSDbConfigurationFileName();

		    if (StringUtils.isEmpty(udsDbConfigFileName)) {
			throw new IllegalArgumentException("No valid UDS Db Configuration file");
		    } else {
			LOG.debug("Initializing DataStoreConnectorFactory from [{}] file",
				udsDbConfigFileName);
			((DStoreCustomPoolConnectorFactory)result).initialize(udsDbConfigFileName, "SequenceRepository");
		    }
		}
		connectorFactory = result;
	    } else {
		result = connectorFactory;
	    }

	} // End of synchronized block on INITIALIZATION_LOCK

	return result;
    }

    /**
     * Retrieve current SEQ DatabaseConnector instance.
     * 
     * @param serviceSide
     *            <code>true</code> if called by service (lazy initialization of SEQ Database) or
     *            <code>false</code> if called by client (provider use)
     * @return SEQ DatabaseConnector or <code>null</code> if DatabaseConnector cannot be initialized
     */
    public static IDatabaseConnector getSEQDatabaseConnector(final boolean serviceSide) {
	IDatabaseConnector result = null;

	synchronized (INITIALIZATION_LOCK) {

	    if (seqDatabaseConnector == null) {
		result = createSEQDatabaseConnector(serviceSide);

		seqDatabaseConnector = result;
	    } else {
		result = seqDatabaseConnector;
	    }

	} // End of synchronized block on INITIALIZATION_LOCK

	return result;
    }

    /* Called holding INITIALIZATION_LOCK */
    private static IDatabaseConnector createSEQDatabaseConnector(final boolean serviceSide) {
	IDatabaseConnector seqDbConnector = null;

	final IDatabaseConnector udsDbConnector = getDataStoreConnectorFactory().getUdsDbConnector();
	final EntityManagerFactory udsEMF = udsDbConnector.getEntityManagerFactory();

	EntityManager udsEM = udsEMF.createEntityManager();

	try {
	    /* Try to load SEQ Db Connector */
	    final ExternalDb seqDb = ExternalDbRepository.findExternalByType(udsEM, ProlineDatabaseType.SEQ);

	    if (seqDb == null) {

		if (serviceSide) {
		    LOG.info("No ExternalDb for SEQ Db creating a new one");
		    seqDbConnector = createSEQDatabase(udsDbConnector, udsEM);
		} else {
		    throw new RuntimeException("SEQ Db does not exist");
		}

	    } else {
		seqDbConnector = DatabaseConnectorFactory.createDatabaseConnectorInstance(
			ProlineDatabaseType.SEQ, seqDb.toPropertiesMap(udsDbConnector.getDriverType()));
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
    private static IDatabaseConnector createSEQDatabase(final IDatabaseConnector udsDbConnector,
	    final EntityManager udsEM) {
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
		seqDb = createSEQDb(udsEM, udsDriverType);
	    } else if (udsDriverType == DriverType.POSTGRESQL) {
		final boolean success = createPgSEQDatabase(udsDbConnector);

		if (success) {
		    seqDb = createSEQDb(udsEM, udsDriverType);
		} else {
		    LOG.error("Unable to create Postgres SEQ Database");
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

	if (transacOK && (seqDb != null)) {
	    seqDbConnector = DatabaseConnectorFactory.createDatabaseConnectorInstance(
		    ProlineDatabaseType.SEQ, seqDb.toPropertiesMap());

	    DatabaseUpgrader.upgradeDatabase(seqDbConnector);
	}

	return seqDbConnector;
    }

    private static boolean createPgSEQDatabase(final IDatabaseConnector udsDbConnector) {
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
		    LOG.error("Error creating Postgres SEQ Database : {}", result);
		}

	    }

	} catch (Exception ex) {
	    LOG.error("Error creating Postgres SEQ Database", ex);
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
    private static ExternalDb createSEQDb(final EntityManager udsEM, final DriverType udsDriverType) {
	ExternalDb seqDb = null;

	final ExternalDb psDb = ExternalDbRepository.findExternalByType(udsEM, ProlineDatabaseType.PS);

	if (psDb == null) {
	    LOG.error("Cannot find template PS ExternalDb");
	} else {
	    seqDb = new ExternalDb();

	    seqDb.setDriverType(udsDriverType);
	    seqDb.setDbName(SEQ_DB_NAME);
	    seqDb.setDbPassword(psDb.getDbPassword());
	    seqDb.setDbUser(psDb.getDbUser());
	    seqDb.setDbVersion(psDb.getDbVersion());
	    seqDb.setHost(psDb.getHost());
	    seqDb.setPort(psDb.getPort());
	    seqDb.setType(ProlineDatabaseType.SEQ);
	    seqDb.setConnectionMode(psDb.getConnectionMode());

	    udsEM.persist(seqDb);

	    LOG.debug("New SEQ ExternalDb persisted");
	}

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

	stmt.close(); // Close ResultSet and Statement
	// sqlCon is closed in finally block of createPgSEQDatabase()

	return result;
    }

}