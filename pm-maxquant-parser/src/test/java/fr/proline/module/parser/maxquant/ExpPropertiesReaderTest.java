package fr.proline.module.parser.maxquant;

import java.util.Map;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import fr.proline.core.om.model.msi.Instrument;
import fr.proline.core.om.model.msi.InstrumentConfig;
import fr.proline.core.om.model.msi.PeaklistSoftware;
import fr.proline.core.om.model.msi.SearchSettings;
import fr.proline.core.om.provider.msi.IPTMProvider;
import fr.proline.core.om.provider.msi.ISeqDatabaseProvider;
import fr.proline.module.parser.maxquant.util.TestPTMProvider;
import fr.proline.module.parser.maxquant.util.TestSeqdDBProvider;


public class ExpPropertiesReaderTest {
	


	@Before
	public void setup(){
		
	}
	
	@Test
	public void testReadPropertiesV1_5(){
		ISeqDatabaseProvider testSeqDBProvider = new TestSeqdDBProvider();
		IPTMProvider testPtmProvider = new TestPTMProvider(); 
		
		String folder = "/mq_results/1_5/SmallRun";
		InstrumentConfig ic = new InstrumentConfig(-1, new Instrument(-1, "test", "", null) , "FTMS", "FTMS", "CID");
		PeaklistSoftware ps = new PeaklistSoftware(-1,"test ","1.0", null,null);

		ExperimentPropertiesReader reader = new ExperimentPropertiesReader(this.getClass().getResource(folder), testSeqDBProvider,testPtmProvider, ic, ps);
		
		Map<String, Long> rsidByName = reader.getResultSetIds();
		Assert.assertEquals(2, rsidByName.size());
		Assert.assertNotNull(rsidByName.get("OVEMB150205_12"));
		Assert.assertNotNull(rsidByName.get("OVEMB150205_27"));
		
		SearchSettings ss = reader.getSearchSettings();
		Assert.assertNotNull(ss);
		Assert.assertEquals(2, ss.maxMissedCleavages());
		Assert.assertEquals(2, ss.seqDatabases().length);
		Assert.assertEquals(4.5, ss.ms1ErrorTol(), 0.001);
		Assert.assertEquals("MaxQuant", ss.softwareName());
		Assert.assertEquals("1.5.3.30", ss.softwareVersion());
		Assert.assertEquals("max charge 7", ss.ms1ChargeStates());
		Assert.assertEquals(1, ss.usedEnzymes().length);
		Assert.assertEquals("Trypsin/P", ss.usedEnzymes()[0].name());
		Assert.assertEquals(2, ss.variablePtmDefs().length);
		Assert.assertEquals(1, ss.fixedPtmDefs().length);
		
		Assert.assertNotNull(ss.msmsSearchSettings());
		Assert.assertTrue(ss.msmsSearchSettings().isDefined());
	}
	
	@Test
	public void testReadPropertiesV1_4(){
		ISeqDatabaseProvider testSeqDBProvider = new TestSeqdDBProvider();
		IPTMProvider testPtmProvider = new TestPTMProvider(); 
		
		String folder = "/mq_results/1_4/result1";
		InstrumentConfig ic = new InstrumentConfig(-1, new Instrument(-1, "test", "", null) , "FTMS", "FTMS", "CID");
		PeaklistSoftware ps = new PeaklistSoftware(-1,"test ","1.0", null,null);

		ExperimentPropertiesReader reader = new ExperimentPropertiesReader(this.getClass().getResource(folder), testSeqDBProvider,testPtmProvider, ic, ps);
		
		Map<String, Long> rsidByName = reader.getResultSetIds();
		Assert.assertEquals(24, rsidByName.size());
		Assert.assertNotNull(rsidByName.get("VELOS16655"));
		Assert.assertNotNull(rsidByName.get("VELOS16633"));
		
		SearchSettings ss = reader.getSearchSettings();
		Assert.assertNotNull(ss);
		Assert.assertEquals(2, ss.maxMissedCleavages());
		Assert.assertEquals(2, ss.seqDatabases().length);
		Assert.assertEquals(5, ss.ms1ErrorTol(), 0.001);
		Assert.assertEquals("MaxQuant", ss.softwareName());
		Assert.assertEquals("1.4.1.2", ss.softwareVersion());
		Assert.assertEquals("max charge 7", ss.ms1ChargeStates());
		Assert.assertEquals(1, ss.usedEnzymes().length);
		Assert.assertEquals("Trypsin/P", ss.usedEnzymes()[0].name());
		Assert.assertEquals(2, ss.variablePtmDefs().length);
		Assert.assertEquals(1, ss.fixedPtmDefs().length);
		
		Assert.assertNotNull(ss.msmsSearchSettings());
		Assert.assertTrue(ss.msmsSearchSettings().isDefined());
	}

}
