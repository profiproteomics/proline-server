package fr.proline.module.parser.maxquant;

import java.net.URL;
import java.util.Arrays;
import java.util.Map;

import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import fr.proline.context.BasicExecutionContext;
import fr.proline.core.om.model.msi.Instrument;
import fr.proline.core.om.model.msi.InstrumentConfig;
import fr.proline.core.om.model.msi.PeaklistSoftware;
import fr.proline.core.om.model.msi.PtmDefinition;
import fr.proline.core.om.model.msi.SearchSettings;
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext;
import fr.proline.core.om.provider.msi.IPTMProvider;
import fr.proline.core.om.provider.msi.IPeptideProvider;
import fr.proline.core.om.provider.msi.ISeqDatabaseProvider;
import fr.proline.module.parser.maxquant.model.ResultSetsDataMapper;
import fr.proline.module.parser.maxquant.util.TestPTMProvider;
import fr.proline.module.parser.maxquant.util.TestPeptideProvider;
import fr.proline.module.parser.maxquant.util.TestSeqdDBProvider;

public class MSDataReaderTest {

	private static ProviderDecoratedExecutionContext m_pec;
	private static Logger logger = LoggerFactory.getLogger(MSDataReaderTest.class);
	
	@BeforeClass
	public static void setUp(){
		BasicExecutionContext ec = new BasicExecutionContext(null, null, null, null, null);
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
		ExperimentPropertiesReader reader = new ExperimentPropertiesReader(folderURL,m_pec.getProvider(ISeqDatabaseProvider.class),m_pec.getProvider(IPTMProvider.class), ic, ps);
		Map<String, Long> rsidByName = reader.getResultSetIds();
				
		SearchSettings ss = reader.getSearchSettings();
		PtmDefinition[] varPtms = ss.variablePtmDefs();
		PtmDefinition[] fixedPtms = ss.fixedPtmDefs();
		PtmDefinition[] allPtms = Arrays.copyOf(varPtms, varPtms.length+fixedPtms.length);
		System.arraycopy(fixedPtms, 0, allPtms, varPtms.length, fixedPtms.length);
		
		StringBuffer warningMsg = new StringBuffer();
		MSDataReader dataReader = new MSDataReader(folderURL, m_pec, ic,ps);
		ResultSetsDataMapper rsMapper= dataReader.parseMSData2ResulSets(rsidByName, allPtms, warningMsg);
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
		ExperimentPropertiesReader reader = new ExperimentPropertiesReader(folderURL,m_pec.getProvider(ISeqDatabaseProvider.class),m_pec.getProvider(IPTMProvider.class), ic, ps);
		Map<String, Long> rsidByName = reader.getResultSetIds();
				
		SearchSettings ss = reader.getSearchSettings();
		PtmDefinition[] varPtms = ss.variablePtmDefs();
		PtmDefinition[] fixedPtms = ss.fixedPtmDefs();
		PtmDefinition[] allPtms = Arrays.copyOf(varPtms, varPtms.length+fixedPtms.length);
		System.arraycopy(fixedPtms, 0, allPtms, varPtms.length, fixedPtms.length);
		
		StringBuffer warningMsg = new StringBuffer();
		MSDataReader dataReader = new MSDataReader(folderURL, m_pec, ic,ps);
		ResultSetsDataMapper rsMapper= dataReader.parseMSData2ResulSets(rsidByName, allPtms, warningMsg);
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
}
