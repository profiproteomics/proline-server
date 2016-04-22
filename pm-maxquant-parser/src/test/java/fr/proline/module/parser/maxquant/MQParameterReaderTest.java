package fr.proline.module.parser.maxquant;

import java.io.InputStream;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import fr.proline.core.om.model.msi.Peaklist;
import fr.proline.module.parser.maxquant.model.IMsMsParameters;


public class MQParameterReaderTest  {

	@Test
	public void testReadParam1_5() {
		
	    JAXBContext context;
		try {
			context = JAXBContext.newInstance(fr.proline.module.parser.maxquant.model.v1_5.MaxQuantParams.class);		
	    
		    Unmarshaller unmarshaller = context.createUnmarshaller() ;
		    InputStream is = MQParameterReaderTest.class.getResourceAsStream("/mq_results/1_5/SmallRun/mqpar.xml");
		    fr.proline.module.parser.maxquant.model.v1_5.MaxQuantParams mqParams = (fr.proline.module.parser.maxquant.model.v1_5.MaxQuantParams)unmarshaller.unmarshal(is) ;
		    Assert.assertEquals("1.5.3.30", mqParams.getVersion());
		    Assert.assertEquals(2, mqParams.getFilePaths().size());
		    Assert.assertNotNull( mqParams.getParameters()) ;
		    
		    Assert.assertEquals(new Integer(7), mqParams.getParameters().get(0).getMaxCharge());
		    Assert.assertEquals(new Integer(2), mqParams.getParameters().get(0).getMaxMissedCleavages());
	    	    
			Peaklist peaklist = new Peaklist(Peaklist.generateNewId(), //id
				"MaxQuant RAW FILE", // fileType
				"rawFileName", // path
				"", //rawFileIdentifier
				2, // msLevel
				"none", //spectrumDataCompression
				null,// peaklistSoftware
				null);//PeaklistProperties
			Assert.assertTrue(peaklist.id() < 0);
			peaklist.id_$eq(12L);
			
			Assert.assertEquals(12L, peaklist.id());
			List<? extends IMsMsParameters> msmsParams = mqParams.getMsMsParameters();
			Assert.assertEquals(4,msmsParams.size());
			for(IMsMsParameters msmsParam : msmsParams){
				switch(msmsParam.getInstrumTypeName()){
				case "FTMS" :
					Assert.assertEquals(new Float(20),msmsParam.getMatchTolerance());
					break;
				case "ITMS" :
					Assert.assertEquals(new Float(0.5),msmsParam.getMatchTolerance());
					break;
				case "TOF" :
					Assert.assertEquals(new Float(40),msmsParam.getMatchTolerance());
					break;
				case "Unknown":
					Assert.assertEquals(new Float(0.5),msmsParam.getMatchTolerance());
					break;
				default:
					Assert.fail("Unexpected Instrument type ! ");
				}
				if(msmsParam.getInstrumTypeName().equalsIgnoreCase("FTMS")){
					Assert.assertEquals(new Float(20.0),msmsParam.getMatchTolerance());
				}
			}
			
		} catch (JAXBException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			throw new RuntimeException(e);
		}
			   
	}
	
	@Ignore
	public void testReadParam1_4() {
		
	    JAXBContext context;
		try {
			context = JAXBContext.newInstance(fr.proline.module.parser.maxquant.model.v1_4.MaxQuantParams.class);		
	    
		    Unmarshaller unmarshaller = context.createUnmarshaller() ;
		    InputStream is = MQParameterReaderTest.class.getResourceAsStream("/mq_results/1_4/result1/mqpar.xml");
		    fr.proline.module.parser.maxquant.model.v1_4.MaxQuantParams mqParams = (fr.proline.module.parser.maxquant.model.v1_4.MaxQuantParams)unmarshaller.unmarshal(is) ;

		    Assert.assertEquals(24, mqParams.getFilePaths().size());
		    Assert.assertNotNull( mqParams.getParameters()) ;
		    
		    Assert.assertEquals(new Integer(7), mqParams.getParameters().get(0).getMaxCharge());
		    Assert.assertEquals(new Integer(2), mqParams.getParameters().get(0).getMaxMissedCleavages());
	    	    
			List<? extends IMsMsParameters> msmsParams = mqParams.getMsMsParameters();
			Assert.assertEquals(4,msmsParams.size());
			for(IMsMsParameters msmsParam : msmsParams){
				switch(msmsParam.getInstrumTypeName()){
				case "FTMS" :
					Assert.assertEquals(new Float(20),msmsParam.getMatchTolerance());
					break;
				case "ITMS" :
					Assert.assertEquals(new Float(0.5),msmsParam.getMatchTolerance());
					break;
				case "TOF" :
					Assert.assertEquals(new Float(0.1),msmsParam.getMatchTolerance());
					break;
				case "Unknown":
					Assert.assertEquals(new Float(0.5),msmsParam.getMatchTolerance());
					break;
				default:
					Assert.fail("Unexpected Instrument type ! ");
				}
				if(msmsParam.getInstrumTypeName().equalsIgnoreCase("FTMS")){
					Assert.assertEquals(new Float(20.0),msmsParam.getMatchTolerance());
				}
			}
			
		} catch (JAXBException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			throw new RuntimeException(e);
		}
			   
	}

}
