package fr.proline.module.seq;


import fr.proline.repository.DatabaseUpgrader;
import fr.proline.repository.DriverType;
import fr.proline.repository.IDatabaseConnector;
import fr.proline.repository.util.DatabaseUtils;
import org.dbunit.DataSourceDatabaseTester;
import org.dbunit.IDatabaseTester;
import org.dbunit.database.DatabaseConfig;
import org.dbunit.database.IDatabaseConnection;
import org.dbunit.dataset.IDataSet;
import org.dbunit.dataset.datatype.DefaultDataTypeFactory;
import org.dbunit.dataset.datatype.IDataTypeFactory;
import org.dbunit.ext.h2.H2DataTypeFactory;
import org.dbunit.ext.postgresql.PostgresqlDataTypeFactory;
import org.dbunit.util.fileloader.DataFileLoader;
import org.dbunit.util.fileloader.FlatXmlDataFileLoader;

import javax.sql.DataSource;
import java.sql.Statement;
import java.util.Arrays;
import java.util.Collection;

public class AbstractDatabaseTest {

  protected void initDBTestCase() {
    initDBTestCase("/dbunit_samples/SmallRuns_XIC/uds-db.xml","/dbunit_samples/SmallRuns_XIC/msi-db.xml");
  }

  protected void initDBTestCase(String udsDatasetName, String msiDatasetName) {
    try {

      IDatabaseConnector udsConnector = DatabaseAccess.getDataStoreConnectorFactory().getUdsDbConnector();
      setUpDatabase(udsConnector, udsDatasetName);

      IDatabaseConnector msiConnector = DatabaseAccess.getDataStoreConnectorFactory().getMsiDbConnector(1L);
      setUpDatabase(msiConnector, msiDatasetName);

      DatabaseAccess.getSEQDatabaseConnector(true);

    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  protected void initSeqDBTestCase(String datasetName) {
    try {
      IDatabaseConnector connector = DatabaseAccess.getSEQDatabaseConnector(true);
      setUpDatabase(connector, datasetName);
    } catch (Exception e) {
      e.printStackTrace();
    }
  }


  public static void setUpDatabase(IDatabaseConnector connector, String datasetName) throws Exception {
    String scriptLocation = DatabaseUpgrader.buildMigrationScriptsLocation(connector.getProlineDatabaseType(), connector.getDriverType());
    String classLocation = DatabaseUpgrader.buildMigrationClassLocation(connector.getProlineDatabaseType(), connector.getDriverType());
    DatabaseUpgrader.upgradeDatabase(connector, scriptLocation, classLocation, false);

    final DataFileLoader dataLoader = new FlatXmlDataFileLoader(DatabaseUtils.createDatatSetBuilder());
    final IDataSet dataset = dataLoader.load(datasetName);
    IDatabaseTester databaseTester = createDatabaseTester(connector, connector.getDriverType());
    databaseTester.setDataSet(dataset);
    databaseTester.onSetup();
  }


  private static IDatabaseTester createDatabaseTester(IDatabaseConnector connector, DriverType driverType) {

    DataSource dataSource = connector.getDataSource();

    // Return a DataSourceDatabaseTester configured with the appropriate IDataTypeFactory
    return new DataSourceDatabaseTester(dataSource) {
      @Override
      public IDatabaseConnection getConnection() throws Exception {
        IDatabaseConnection dbC = super.getConnection();

        // ***** WORKAROUND FOR DBUNIT empty fields ERROR ("value is empty but must contain a value") ***** //
        DatabaseConfig databaseConfig = dbC.getConfig();
        databaseConfig.setProperty(DatabaseConfig.FEATURE_ALLOW_EMPTY_FIELDS, Boolean.TRUE);

        // Retrieve the IDataTypeFactory corresponding to the DriverType
        IDataTypeFactory dataTypeFactory = null;
        switch (driverType) {
          case H2:
            dataTypeFactory = new H2DataTypeFactory();

            // ***** WORKAROUND FOR DBUNIT Referential integrity constraint violation ERROR ***** //
            Statement smt = dbC.getConnection().createStatement();
            smt.execute("SET REFERENTIAL_INTEGRITY FALSE");
            smt.close();
            // ***** WORKAROUND FOR DBUNIT Referential integrity constraint violation ERROR ***** //

            break;
          case POSTGRESQL:
            dataTypeFactory = new PostgresqlDataTypeFactory();
            break;
          case SQLITE:
            dataTypeFactory = new DefaultDataTypeFactory() {
              @SuppressWarnings("rawtypes")
              @Override
              public Collection getValidDbProducts() {
                return Arrays.asList(new String[]{"SQLite"});
              }
            };
            break;
        }

        dbC.getConfig().setProperty(
                DatabaseConfig.PROPERTY_DATATYPE_FACTORY, dataTypeFactory
        );

        return dbC;
      }
    };
  }

}
