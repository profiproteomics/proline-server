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

package fr.proline.module.paser.diann.parquet;


import fr.proline.module.paser.diann.DiaNNResultsParser;
import fr.proline.module.paser.diann.model.DiaNNResult;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.sql.SQLException;
import java.util.List;

public class ReadDiaNNTest {
//  private final static String FILE_NAME2 ="C:\\vero\\DEV\\DIA\\DiaNN\\AscendMix\\drill";
//  private final static String FILE_NAME ="C:\\vero\\DEV\\DIA\\DiaNN\\AscendMix\\Param11_Mix500ngAscend_nonorm_reportinfo";
  private final static String FILE_NAME = "/smallPasef";
  private File fileToTest;
  @Before
  public void setUp() throws Exception {
    File parent = new File(getClass().getResource(FILE_NAME).toURI());
    fileToTest = new File(parent, DiaNNResultsParser.MAIN_REPORT_PARQUET);
  }


  @Test
  public void testReadDiaNNData() {
//    DiaNNResultsParser resultFile = new DiaNNResultsParser(null, FILE_NAME2, new HashMap<>());
//    resultFile.runService();
    DiaNNParquetReader reader = new DiaNNParquetReader(fileToTest);
    DiaNNResult result = null;
    try {
      result = reader.readData();
      Assert.assertNotNull(result);
      List<String> runs = result.getRuns();
      Assert.assertEquals(2, runs.size());
      Assert.assertEquals(3149, result.getPrecursorForRun("20221016_PRO1_LSVD_00_30-0051_10ng_Regular_P1-B6_1_11227").size());
      Assert.assertEquals(3320, result.getPrecursorForRun("20221016_PRO1_LSVD_00_30-0051_10ng_Regular_P1-B6_1_11228").size());
      Assert.assertEquals(871, result.getProteinGroupsForRun("20221016_PRO1_LSVD_00_30-0051_10ng_Regular_P1-B6_1_11228").size());
    } catch (SQLException e) {
      Assert.fail(e.getMessage());
    }


  }

  @Test
  public void testComputeMissCleavedTrypsinLikeRule() {
    DiaNNParquetReader reader = new DiaNNParquetReader(null, null, "K*,R*");
    Assert.assertEquals(Integer.valueOf(2), reader.computeMissCleaved("AKRA"));
    reader.setCutValue( "K*,R*,!*P");
    Assert.assertEquals(Integer.valueOf(0), reader.computeMissCleaved("AKPA"));
  }

  @Test
  public void testComputeMissCleavedRule2() {
    DiaNNParquetReader reader = new DiaNNParquetReader(null, null,  "*P");
    Assert.assertEquals(Integer.valueOf(2), reader.computeMissCleaved("APKP"));
    reader.setCutValue( "*P,!KP");
    Assert.assertEquals(Integer.valueOf(1), reader.computeMissCleaved("APKP"));
  }
}