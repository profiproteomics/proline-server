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

package fr.proline.module.paser.diann.builder;

import fr.proline.core.om.model.msi.ResultSet;
import fr.proline.core.om.model.msi.ResultSummary;
import fr.proline.core.om.model.msq.MasterQuantChannel;
import fr.proline.core.om.provider.PeptideCacheExecutionContext;
import fr.proline.core.om.provider.msi.impl.SQLResultSetProvider;
import fr.proline.core.om.provider.msi.impl.SQLResultSummaryProvider;
import fr.proline.module.paser.diann.DiaNNResultsParser;
import fr.proline.module.paser.diann.model.DiaNNResult;
import fr.proline.module.paser.diann.parquet.DiaNNParquetReader;
import fr.proline.module.paser.diann.quantify.DiaNNQuantifier;
import fr.proline.module.paser.diann.util.AbstractDatastoreTest;
import fr.proline.repository.util.JDBCWork;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.Option;

import java.io.File;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Objects;

public class DiaNNProcessDataTest extends AbstractDatastoreTest {

//  private final static String FILE_NAME = "C:\\vero\\DEV\\DIA\\DiaNN\\small-dia-PASEF 10ng\\results1File\\fromFasta";
  private final static String FILE_NAME = "/smallPasef";
  private static final Logger logger = LoggerFactory.getLogger(DiaNNProcessDataTest.class);
  private DiaNNResult diaNNResult;

  @Before
  public void setUp() throws Exception {
    File parentFile = new File(Objects.requireNonNull(getClass().getResource(FILE_NAME)).toURI());
    DiaNNParquetReader reader = new DiaNNParquetReader(new File(parentFile, DiaNNResultsParser.MAIN_REPORT_PARQUET));
    diaNNResult =  reader.readData();
    super.setUp();
  }

  @Test
  public void testCreateDataset() throws SQLException {
    logger.debug(" Create GFake Object for Create quant dataset");
    HashMap<String , ResultSet> rsByRun = createFakeResultsSets();
    HashMap<Long ,ResultSummary> rsmByRS = new HashMap<>();
    rsmByRS.put(1L, createFakeResultSummary(1L));
    rsmByRS.put(2L, createFakeResultSummary(2L));
    logger.debug(" Run Create quant dataset");
    DiaNNProcessData quantifier = new DiaNNProcessData(executionContext, diaNNResult, null, rsByRun, new HashMap<>() ,rsmByRS);
    MasterQuantChannel mqchannel = quantifier.createQuantitationExpDesign(createFakeResultSummary(5L));
    Assert.assertNotNull(mqchannel);
    Assert.assertTrue(mqchannel.id() > 0);
    JDBCWork testData = con -> {
      Statement stmt = con.createStatement();
      java.sql.ResultSet sqlRS = stmt.executeQuery("SELECT quantitation_id from master_quant_channel where id = "+mqchannel.id());
      Assert.assertTrue(sqlRS.next());
      long dsId = sqlRS.getLong(1);

      sqlRS = stmt.executeQuery("SELECT name from data_set where id = "+dsId);
      Assert.assertTrue(sqlRS.next());
      Assert.assertEquals(sqlRS.getString(1), diaNNResult.getName());
    };
    executionContext.getUDSDbConnectionContext().doWork(testData, false);

    DiaNNQuantifier q = new DiaNNQuantifier(mqchannel,rsByRun, null, diaNNResult, null , executionContext );
  }

  private HashMap<String , ResultSet> createFakeResultsSets(){
    SQLResultSetProvider resultSetProvider = new SQLResultSetProvider(new PeptideCacheExecutionContext(executionContext));
    HashMap<String, ResultSet> result = new HashMap<>();

    ResultSet rsOneFile = resultSetProvider.getResultSet(1, Option.empty()).get();
    result.put(diaNNResult.getRuns().get(0), rsOneFile);
    ResultSet rsTwoFile = resultSetProvider.getResultSet(2, Option.empty()).get();
    result.put(diaNNResult.getRuns().get(1), rsTwoFile);
    return result;
  }

  private ResultSummary createFakeResultSummary(long id){
    SQLResultSummaryProvider rsmprovider = new SQLResultSummaryProvider(new PeptideCacheExecutionContext(executionContext));
    return rsmprovider.getResultSummary(id, false, Option.empty()).get();
  }
}
