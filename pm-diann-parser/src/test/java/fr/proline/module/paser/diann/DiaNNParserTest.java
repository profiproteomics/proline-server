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

package fr.proline.module.paser.diann;


import fr.proline.core.om.model.msi.PtmDefinition;
import fr.proline.core.om.provider.msi.IPTMProvider;
import fr.proline.core.om.provider.msi.IPeptideProvider;
import fr.proline.module.paser.diann.model.DiaNNResult;
import fr.proline.module.paser.diann.util.AbstractDatastoreTest;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class DiaNNParserTest  extends AbstractDatastoreTest {
//  private final static String FILE_NAME2 ="C:\\vero\\DEV\\DIA\\DiaNN\\AscendMix\\Param11_Mix500ngAscend_nonorm_reportinfo";
  private final static String FILE_NAME = "/smallPasef"; // "/2FilesResult";
//  private final static String FILE_NAME1 = "C:\\vero\\DEV\\DIA\\DiaNN\\small-dia-PASEF 10ng\\results1File\\fromFasta";
  private final static Integer NB_RUNS = 2;
  private static final Logger logger = LoggerFactory.getLogger(DiaNNParserTest.class);

  @Test
  public  void testPTMs()  {

    logger.debug("Test initialization");
    IPeptideProvider pepProvider = super.executionContext.getProvider(IPeptideProvider.class);
    IPTMProvider ptmProvider = super.executionContext.getProvider(IPTMProvider.class);
    Assert.assertNotNull(pepProvider);
    Assert.assertNotNull(ptmProvider);
    PtmDefinition[] ptmDefinition = ptmProvider.getUnimodPtmDefinition(4);
    Assert.assertNotNull(ptmDefinition);
    Assert.assertTrue(ptmDefinition.length>1);
    List<String> names = Arrays.stream(ptmDefinition).map(ptm -> ptm.names().shortName()).toList();
    Assert.assertTrue(names.contains("Carbamidomethyl"));

  }


  @Test
  public  void testDiaNNParser()  {

    logger.debug("Test initialization Done ");
    IPeptideProvider pepProvider = super.executionContext.getProvider(IPeptideProvider.class);
    IPTMProvider ptmProvider = super.executionContext.getProvider(IPTMProvider.class);
    Assert.assertNotNull(pepProvider);
    Assert.assertNotNull(ptmProvider);
    logger.debug("GOT Providers ");
    File diannFile;
    try {
       diannFile = new File(Objects.requireNonNull(getClass().getResource(FILE_NAME)).toURI());
    } catch (URISyntaxException e) {
      throw new RuntimeException(e);
    }
    DiaNNResultsParser parser = new DiaNNResultsParser(executionContext, diannFile, new HashMap<>());
    logger.debug(" Created ");
    boolean b = parser.runService();
    Assert.assertTrue(b);
    Assert.assertEquals(NB_RUNS, (Object)parser.m_resultSetsByRun.size());
    Assert.assertEquals(NB_RUNS, (Object)parser.m_rsmIdsByRSId.size());
  }

  @Test
  public void testDiaNNParserConstructorWithValidOptions() {
    File diannFile = getDiaNNFolder();
    Map<String, Object> parserOptions = new HashMap<>();
    parserOptions.put(DiaNNResultsParser.INSTR_CONFIG_OPTION_KEY, 4L);
    parserOptions.put(DiaNNResultsParser.PEAKLIST_SOFT_ID_OPTION_KEY, 1L);
    parserOptions.put(DiaNNResultsParser.PARENT_DATASET_ID_OPTION_KEY, 10L);
    parserOptions.put(DiaNNResultsParser.FILTER_MODE_OPTION_KEY, DiaNNResult.FilterMode.NONE);

    DiaNNResultsParser parser = new DiaNNResultsParser(executionContext, diannFile, parserOptions);
    Assert.assertNotNull(parser);
  }

  @Test
  public void testDiaNNParserConstructorInvalidInstrumentConfigOption() {
    assertInvalidOption(DiaNNResultsParser.INSTR_CONFIG_OPTION_KEY, "4", "expected Long");
  }

  @Test
  public void testDiaNNParserConstructorInvalidPeaklistSoftwareOption() {
    assertInvalidOption(DiaNNResultsParser.PEAKLIST_SOFT_ID_OPTION_KEY, "1", "expected Long");
  }

  @Test
  public void testDiaNNParserConstructorInvalidParentDatasetOption() {
    assertInvalidOption(DiaNNResultsParser.PARENT_DATASET_ID_OPTION_KEY, "10", "expected Long");
  }

  @Test
  public void testDiaNNParserConstructorInvalidFilterModeOption() {
    assertInvalidOption(DiaNNResultsParser.FILTER_MODE_OPTION_KEY, "NONE", "expected FilterMode");
  }

  private File getDiaNNFolder() {
    try {
      return new File(Objects.requireNonNull(getClass().getResource(FILE_NAME)).toURI());
    } catch (URISyntaxException e) {
      throw new RuntimeException(e);
    }
  }

  private void assertInvalidOption(String optionKey, Object invalidValue, String expectedMessagePart) {
    File diannFile = getDiaNNFolder();
    Map<String, Object> parserOptions = new HashMap<>();
    parserOptions.put(optionKey, invalidValue);

    try {
      new DiaNNResultsParser(executionContext, diannFile, parserOptions);
      Assert.fail("Expected IllegalArgumentException for option " + optionKey);
    } catch (IllegalArgumentException exception) {
      Assert.assertTrue(exception.getMessage().contains(optionKey));
      Assert.assertTrue(exception.getMessage().contains(expectedMessagePart));
    }
  }

}
