/*
 * Copyright (C)  2026.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the CeCILL FREE SOFTWARE LICENSE AGREEMENT
 * ; either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * CeCILL License V2.1 for more details.
 *
 * You should have received a copy of the CeCILL License
 * along with this program;
 * If not, see <http://www.cecill.info/licences/Licence_CeCILL_V2.1-en.html>.
 *
 */

package fr.proline.module.paser.diann.util;

import fr.proline.context.IExecutionContext;
import fr.proline.core.dal.AbstractMultipleDBTestCase;
import fr.proline.core.dal.BuildLazyExecutionContext;
import fr.proline.core.dbunit.DbUnitResultFileLocation;
import fr.proline.core.om.provider.PeptideCacheExecutionContext;
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext;
import fr.proline.core.om.provider.msi.IInstrumentConfigProvider;
import fr.proline.core.om.provider.msi.IPTMProvider;
import fr.proline.core.om.provider.msi.IPeptideProvider;
import fr.proline.core.om.provider.msi.impl.SQLInstrumentConfigProvider;
import fr.proline.core.om.provider.msi.impl.SQLPTMProvider;
import fr.proline.core.om.provider.msi.impl.SQLPeptideProvider;
import fr.proline.repository.DriverType;
import org.junit.Before;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.Option;

public class AbstractDatastoreTest extends AbstractMultipleDBTestCase {
  Logger logger = LoggerFactory.getLogger(AbstractDatastoreTest.class);
  protected ProviderDecoratedExecutionContext executionContext;

  @Before
  public void setUp() throws Exception {
    super.initDBsDBManagement(DriverType.H2);

    //Load data
    DbUnitResultFileLocation dbUnitResultFile = new TestDataset();
    msiDBTestCase().loadDataSet(dbUnitResultFile.msiDbDatasetPath());
    udsDBTestCase().loadDataSet(dbUnitResultFile.udsDbDatasetPath());
    logger.info(" -- MSI and UDS dbs successfully initialized !");
    //executionContext = buildJPAContext();
    executionContext = buildSQLContext();
  }
//
//  public ProviderDecoratedExecutionContext buildJPAContext() {
//
//    IExecutionContext execCtx = BuildLazyExecutionContext.apply(dsConnectorFactoryForTest(), 1, true, Option.empty()); // Full JPA
//    ProviderDecoratedExecutionContext decoratedContext = ProviderDecoratedExecutionContext.apply(execCtx);
//    decoratedContext.putProvider(IPeptideProvider.class, new ORMPeptideProvider(execCtx.getMSIDbConnectionContext()) );
//    decoratedContext.putProvider(IPTMProvider.class, new ORMPTMProvider(execCtx.getMSIDbConnectionContext()) );
//    return  decoratedContext;
//  }

  public ProviderDecoratedExecutionContext buildSQLContext() {

    IExecutionContext execCtx = BuildLazyExecutionContext.apply(dsConnectorFactoryForTest(), 1, true, Option.empty());
    ProviderDecoratedExecutionContext decoratedContext = ProviderDecoratedExecutionContext.apply(execCtx);
    decoratedContext.putProvider(IPeptideProvider.class, new SQLPeptideProvider(new PeptideCacheExecutionContext(execCtx)));
    decoratedContext.putProvider(IPTMProvider.class, new SQLPTMProvider(execCtx.getMSIDbConnectionContext()) );
    decoratedContext.putProvider(IInstrumentConfigProvider.class, new SQLInstrumentConfigProvider(execCtx.getUDSDbConnectionContext()));

    return  decoratedContext;
  }

  static class TestDataset extends DbUnitResultFileLocation {
    @Override
    public String datastoreDirPath() {
      return "/dbunit_samples/SmallRuns_XIC";
    }
  }
}
