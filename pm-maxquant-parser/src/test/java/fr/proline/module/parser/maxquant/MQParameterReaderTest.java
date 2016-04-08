package fr.proline.module.parser.maxquant;

import java.io.InputStream;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.junit.Assert;
import org.junit.Test;

import fr.proline.core.om.model.msi.Peaklist;
import fr.proline.module.parser.maxquant.model.MaxQuantParams;

public class MQParameterReaderTest  {

	@Test
	public void testReadParam() {
		
	    JAXBContext context;
		try {
			context = JAXBContext.newInstance(MaxQuantParams.class);		
	    
		    Unmarshaller unmarshaller = context.createUnmarshaller() ;
		    InputStream is = MQParameterReaderTest.class.getResourceAsStream("/mq_results/mqpar.xml");
		    MaxQuantParams mqParams = (MaxQuantParams)unmarshaller.unmarshal(is) ;
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
			
		} catch (JAXBException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			throw new RuntimeException(e);
		}
			   
	}

}
