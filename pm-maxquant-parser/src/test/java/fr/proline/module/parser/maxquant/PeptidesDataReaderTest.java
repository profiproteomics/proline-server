package fr.proline.module.parser.maxquant;

import fr.proline.context.BasicExecutionContext;
import fr.proline.core.om.model.msi.FragmentationRule;
import fr.proline.core.om.model.msi.FragmentationRuleSet;
import fr.proline.core.om.model.msi.Instrument;
import fr.proline.core.om.model.msi.InstrumentConfig;
import fr.proline.core.om.model.msi.PeaklistSoftware;
import fr.proline.core.om.model.msi.PtmDefinition;
import fr.proline.core.om.model.msi.SearchSettings;
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext;
import fr.proline.core.om.provider.msi.IPTMProvider;
import fr.proline.core.om.provider.msi.IPeptideProvider;
import fr.proline.core.om.provider.msi.ISeqDatabaseProvider;
import fr.proline.module.parser.maxquant.util.TestPTMProvider;
import fr.proline.module.parser.maxquant.util.TestPeptideProvider;
import fr.proline.module.parser.maxquant.util.TestSeqdDBProvider;
import org.junit.BeforeClass;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URL;
import java.util.Arrays;
import java.util.Map;
import scala.Option;

public class PeptidesDataReaderTest {

	private static ProviderDecoratedExecutionContext m_pec;
	private static Logger logger = LoggerFactory.getLogger(PeptidesDataReaderTest.class);
	
	@BeforeClass
	public static void setUp(){
		BasicExecutionContext ec = new BasicExecutionContext(1, null, null, null);
		m_pec = ProviderDecoratedExecutionContext.apply(ec);
		m_pec.putProvider(IPTMProvider.class, new TestPTMProvider());
		m_pec.putProvider(IPeptideProvider.class, new TestPeptideProvider());
		m_pec.putProvider(ISeqDatabaseProvider.class, new TestSeqdDBProvider());
	}
	
	
//	@Test
	public void testReadPeptidesRealDataV1_5(){
		
//		String folder = "/mq_results/1_5/SmallRun";
		InstrumentConfig ic = new InstrumentConfig(-1, new Instrument(-1, "test", "", null) , "FTMS", "FTMS", "CID");
		PeaklistSoftware ps = new PeaklistSoftware(-1,"test ","1.0", null,null);
		FragmentationRuleSet frs = new FragmentationRuleSet(-1,"test", new FragmentationRule[0]);

//		URL folderURL = this.getClass().getResource(folder);
		String folderName = "C:\\Local\\bruley\\Data\\example_MaxQuant\\100-10";
		ExperimentPropertiesReader reader = new ExperimentPropertiesReader(folderName,m_pec.getProvider(ISeqDatabaseProvider.class),m_pec.getProvider(IPTMProvider.class), ic,  Option.apply(frs), ps);
		Map<String, Long> rsidByName = reader.getResultSetIds();

		SearchSettings ss = reader.getSearchSettings();
		PtmDefinition[] varPtms = ss.variablePtmDefs();
		PtmDefinition[] fixedPtms = ss.fixedPtmDefs();
		PtmDefinition[] allPtms = Arrays.copyOf(varPtms, varPtms.length+fixedPtms.length);
		System.arraycopy(fixedPtms, 0, allPtms, varPtms.length, fixedPtms.length);
		
		StringBuffer warningMsg = new StringBuffer();
		MSDataReader dataReader = new MSDataReader(folderName, m_pec, 1l,ps);
		Long start = System.currentTimeMillis();
		dataReader.parseMSData2ResulSets(rsidByName, allPtms, "(.*)", warningMsg);
		logger.info("MS parsed in "+(System.currentTimeMillis() - start)+" ms");
		PeptidesDataReader peptidesReader = new PeptidesDataReader(folderName, dataReader.getPeptidesByMQModifiedSequence());
		start = System.currentTimeMillis();
		peptidesReader.parseQuantitationData(rsidByName, warningMsg);
		logger.info("Peptides parsed in "+(System.currentTimeMillis() - start)+" ms");
		
		
	}

	@Test
	public void testReadPeptidesDataV1_5(){
		
		String folder = "/mq_results/1_5/SmallRun";
		InstrumentConfig ic = new InstrumentConfig(-1, new Instrument(-1, "test", "", null) , "FTMS", "FTMS", "CID");
		PeaklistSoftware ps = new PeaklistSoftware(-1,"test ","1.0", null,null);
		URL folderURL = this.getClass().getResource(folder);
		FragmentationRuleSet frs = new FragmentationRuleSet(-1,"test", new FragmentationRule[0]);

//		String folderName = "C:\\Local\\bruley\\Data\\example_MaxQuant\\100-10";
		ExperimentPropertiesReader reader = new ExperimentPropertiesReader(folderURL,m_pec.getProvider(ISeqDatabaseProvider.class),m_pec.getProvider(IPTMProvider.class), ic,Option.apply(frs), ps);
		Map<String, Long> rsidByName = reader.getResultSetIds();

		SearchSettings ss = reader.getSearchSettings();
		PtmDefinition[] varPtms = ss.variablePtmDefs();
		PtmDefinition[] fixedPtms = ss.fixedPtmDefs();
		PtmDefinition[] allPtms = Arrays.copyOf(varPtms, varPtms.length+fixedPtms.length);
		System.arraycopy(fixedPtms, 0, allPtms, varPtms.length, fixedPtms.length);
		
		StringBuffer warningMsg = new StringBuffer();
		MSDataReader dataReader = new MSDataReader(folderURL, m_pec, 1l,ps);
		Long start = System.currentTimeMillis();
		dataReader.parseMSData2ResulSets(rsidByName, allPtms, "(.*)\\|", warningMsg);
		logger.info("MS parsed in "+(System.currentTimeMillis() - start)+" ms");
		PeptidesDataReader peptidesReader = new PeptidesDataReader(folderURL, dataReader.getPeptidesByMQModifiedSequence());
		start = System.currentTimeMillis();
		peptidesReader.parseQuantitationData(rsidByName, warningMsg);
		logger.info("Peptides parsed in "+(System.currentTimeMillis() - start)+" ms");
		
		
	}


}
