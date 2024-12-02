package fr.proline.module.parser.maxquant;

import fr.proline.context.BasicExecutionContext;
import fr.proline.core.om.model.msi.*;
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext;
import fr.proline.core.om.provider.msi.IPTMProvider;
import fr.proline.core.om.provider.msi.IPeptideProvider;
import fr.proline.core.om.provider.msi.ISeqDatabaseProvider;
import fr.proline.module.parser.maxquant.model.ResultSetsDataMapper;
import fr.proline.module.parser.maxquant.util.TestPTMProvider;
import fr.proline.module.parser.maxquant.util.TestPeptideProvider;
import fr.proline.module.parser.maxquant.util.TestSeqdDBProvider;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.Option;

import java.net.URL;
import java.util.Map;

public class MSDataReaderTest {

	private static ProviderDecoratedExecutionContext m_pec;
	private static Logger logger = LoggerFactory.getLogger(MSDataReaderTest.class);
	
	@BeforeClass
	public static void setUp(){
		BasicExecutionContext ec = new BasicExecutionContext(1, null, null, null);
		m_pec = ProviderDecoratedExecutionContext.apply(ec);
		m_pec.putProvider(IPTMProvider.class, new TestPTMProvider());
		m_pec.putProvider(IPeptideProvider.class, new TestPeptideProvider());
		m_pec.putProvider(ISeqDatabaseProvider.class, new TestSeqdDBProvider());
	}
	
	@Test
	public void testReadMSDataV1_5(){
		
		String folder = "/mq_results/1_5/SmallRun";
		InstrumentConfig ic = new InstrumentConfig(-1, new Instrument(-1, "test", "", null) , "FTMS", "FTMS", "CID");
		PeaklistSoftware ps = new PeaklistSoftware(-1,"test ","1.0", null,null);
		URL folderURL = this.getClass().getResource(folder);
		FragmentationRuleSet frs = new FragmentationRuleSet(-1,"test",  new FragmentationRule[0]);

		ExperimentPropertiesReader reader = new ExperimentPropertiesReader(folderURL,m_pec.getProvider(ISeqDatabaseProvider.class),m_pec.getProvider(IPTMProvider.class), ic, Option.apply(frs), ps);
		Map<String, Long> rsidByName = reader.getResultSetIds();
				
		SearchSettings ss = reader.getSearchSettings();
		PtmDefinition[] varPtms = ss.variablePtmDefs();
		PtmDefinition[] fixedPtms = ss.fixedPtmDefs();

		StringBuffer warningMsg = new StringBuffer();
		MSDataReader dataReader = new MSDataReader(folderURL, m_pec, -1l, ps);
		Long start = System.currentTimeMillis();
		ResultSetsDataMapper rsMapper= dataReader.parseMSData2ResulSets(rsidByName, fixedPtms, "(.*)\\|", warningMsg);
		logger.info("MS parsed in "+(System.currentTimeMillis() - start)+" ms");
		Assert.assertEquals(0,warningMsg.length());
		Assert.assertNotNull(rsMapper);
		Assert.assertNotNull(rsMapper.getPeptideMatchesForRs("OVEMB150205_12"));
		Assert.assertNotNull(rsMapper.getPeptideMatchesForRs("OVEMB150205_27"));
		Assert.assertNotNull(rsMapper.getPeptidesForRs("OVEMB150205_12"));
		Assert.assertNotNull(rsMapper.getPeptidesForRs("OVEMB150205_27"));
		Assert.assertNotNull(rsMapper.getProteinMatchesForRs("OVEMB150205_12"));
		Assert.assertNotNull(rsMapper.getProteinMatchesForRs("OVEMB150205_27"));
		Assert.assertNotNull(rsMapper.getSpectrumByIdForRs("OVEMB150205_12"));
		Assert.assertNotNull(rsMapper.getSpectrumByIdForRs("OVEMB150205_27"));
		
		Assert.assertEquals(184,rsMapper.getPeptideMatchesForRs("OVEMB150205_12").size());
		Assert.assertEquals(251,rsMapper.getPeptideMatchesForRs("OVEMB150205_27").size());
		
	}

	@Test
	public void testReadErrMSDataV1_5(){

		
		String folder = "/mq_results/1_5/SmallRunErr";
		InstrumentConfig ic = new InstrumentConfig(-1, new Instrument(-1, "test", "", null) , "FTMS", "FTMS", "CID");
		PeaklistSoftware ps = new PeaklistSoftware(-1,"test ","1.0", null,null);
		URL folderURL = this.getClass().getResource(folder);
		FragmentationRuleSet frs = new FragmentationRuleSet(-1,"test",  new FragmentationRule[0]);
		ExperimentPropertiesReader reader = new ExperimentPropertiesReader(folderURL,m_pec.getProvider(ISeqDatabaseProvider.class),m_pec.getProvider(IPTMProvider.class), ic,  Option.apply(frs), ps);
		Map<String, Long> rsidByName = reader.getResultSetIds();
				
		SearchSettings ss = reader.getSearchSettings();
		PtmDefinition[] varPtms = ss.variablePtmDefs();
		PtmDefinition[] fixedPtms = ss.fixedPtmDefs();

		StringBuffer warningMsg = new StringBuffer();
		MSDataReader dataReader = new MSDataReader(folderURL, m_pec, -1l,ps);
		ResultSetsDataMapper rsMapper= dataReader.parseMSData2ResulSets(rsidByName, fixedPtms, "(.*)\\|", warningMsg);
		Assert.assertNotEquals(0,warningMsg.length());
		logger.info(" Warning Msg: "+warningMsg.toString());
		Assert.assertNotNull(rsMapper);
		Assert.assertNotNull(rsMapper.getPeptideMatchesForRs("OVEMB150205_12"));
		Assert.assertNotNull(rsMapper.getPeptideMatchesForRs("OVEMB150205_27"));
		Assert.assertNotNull(rsMapper.getPeptidesForRs("OVEMB150205_12"));
		Assert.assertNotNull(rsMapper.getPeptidesForRs("OVEMB150205_27"));
		Assert.assertNotNull(rsMapper.getProteinMatchesForRs("OVEMB150205_12"));
		Assert.assertNotNull(rsMapper.getProteinMatchesForRs("OVEMB150205_27"));
		Assert.assertNotNull(rsMapper.getSpectrumByIdForRs("OVEMB150205_12"));
		Assert.assertNotNull(rsMapper.getSpectrumByIdForRs("OVEMB150205_27"));
		
		Assert.assertEquals(183,rsMapper.getPeptideMatchesForRs("OVEMB150205_12").size());
		Assert.assertEquals(251,rsMapper.getPeptideMatchesForRs("OVEMB150205_27").size());
		
	}

	@Ignore
	@Test
	public void testReadMSDataV_Laura(){

		String folder = "/mq_results/1_5/test_Maxquant";
		InstrumentConfig ic = new InstrumentConfig(-1, new Instrument(-1, "test", "", null) , "FTMS", "FTMS", "CID");
		PeaklistSoftware ps = new PeaklistSoftware(-1,"test ","1.0", null,null);
		URL folderURL = this.getClass().getResource(folder);
		FragmentationRuleSet frs = new FragmentationRuleSet(-1,"test",  new FragmentationRule[0]);

		ExperimentPropertiesReader reader = new ExperimentPropertiesReader(folderURL,m_pec.getProvider(ISeqDatabaseProvider.class),m_pec.getProvider(IPTMProvider.class), ic, Option.apply(frs), ps);
		Map<String, Long> rsidByName = reader.getResultSetIds();

		SearchSettings ss = reader.getSearchSettings();
		PtmDefinition[] varPtms = ss.variablePtmDefs();
		PtmDefinition[] fixedPtms = ss.fixedPtmDefs();

		StringBuffer warningMsg = new StringBuffer();
		MSDataReader dataReader = new MSDataReader(folderURL, m_pec, -1l, ps);
		Long start = System.currentTimeMillis();
		ResultSetsDataMapper rsMapper= dataReader.parseMSData2ResulSets(rsidByName, fixedPtms, "(.*)\\|", warningMsg);
		logger.info("MS parsed in "+(System.currentTimeMillis() - start)+" ms");
		Assert.assertEquals(0,warningMsg.length());
		Assert.assertNotNull(rsMapper);
		Assert.assertNotNull(rsMapper.getPeptideMatchesForRs("OVEMB150205_12"));
		Assert.assertNotNull(rsMapper.getPeptideMatchesForRs("OVEMB150205_27"));
		Assert.assertNotNull(rsMapper.getPeptidesForRs("OVEMB150205_12"));
		Assert.assertNotNull(rsMapper.getPeptidesForRs("OVEMB150205_27"));
		Assert.assertNotNull(rsMapper.getProteinMatchesForRs("OVEMB150205_12"));
		Assert.assertNotNull(rsMapper.getProteinMatchesForRs("OVEMB150205_27"));
		Assert.assertNotNull(rsMapper.getSpectrumByIdForRs("OVEMB150205_12"));
		Assert.assertNotNull(rsMapper.getSpectrumByIdForRs("OVEMB150205_27"));

		Assert.assertEquals(184,rsMapper.getPeptideMatchesForRs("OVEMB150205_12").size());
		Assert.assertEquals(251,rsMapper.getPeptideMatchesForRs("OVEMB150205_27").size());

	}


}
