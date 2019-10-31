package fr.proline.module.parser.maxquant;

import java.util.List;

import org.junit.Assert;
import org.junit.Test;

import fr.proline.core.om.model.msi.PtmDefinition;
import fr.proline.module.parser.maxquant.util.TestPTMProvider;

public class PTMUtilsTest {
	
//	private static Logger logger  = LoggerFactory.getLogger(PTMUtilsTest.class);
	
	@Test
	public void testParseString(){
		
		//Parse Simple modif
		String ptmStr = "Dioxidation (M)";
		List<PtmDefinition > ptmDefs = PTMUtils.parsePTMString(new TestPTMProvider(), ptmStr);
		Assert.assertNotNull(ptmDefs);
		Assert.assertEquals(1, ptmDefs.size());
		Assert.assertEquals("Dioxidation", ptmDefs.get(0).names().shortName());
		Assert.assertEquals('M', ptmDefs.get(0).residue());
		Assert.assertEquals("Anywhere", ptmDefs.get(0).location());
		
		//Parse multi site modif
		ptmStr = "Phospho (STY)";
		ptmDefs = PTMUtils.parsePTMString(new TestPTMProvider(), ptmStr);
		Assert.assertNotNull(ptmDefs);
		Assert.assertEquals(3, ptmDefs.size());
		Assert.assertEquals("Phospho", ptmDefs.get(0).names().shortName());
		Assert.assertEquals("Anywhere", ptmDefs.get(0).location());
		Assert.assertTrue(ptmDefs.get(0).residue() == 'S' || ptmDefs.get(0).residue() == 'T' || ptmDefs.get(0).residue() == 'Y');

		//Parse multi () modif
		ptmStr = "Hex(1)HexNAc(1) (ST)";
		ptmDefs = PTMUtils.parsePTMString(new TestPTMProvider(), ptmStr);
		Assert.assertNotNull(ptmDefs);
		Assert.assertEquals(2, ptmDefs.size());
		Assert.assertEquals("Hex(1)HexNAc(1)", ptmDefs.get(0).names().shortName());
		Assert.assertEquals("Anywhere", ptmDefs.get(0).location());
		Assert.assertTrue(ptmDefs.get(0).residue() == 'S' || ptmDefs.get(0).residue() == 'T');
	
		//Parse Protein N-term  modif
		ptmStr = "Acetyl (Protein Nterm)";
		ptmDefs = PTMUtils.parsePTMString(new TestPTMProvider(), ptmStr);
		Assert.assertNotNull(ptmDefs);
		Assert.assertEquals(1, ptmDefs.size());
		Assert.assertEquals("Acetyl", ptmDefs.get(0).names().shortName());
		Assert.assertEquals("Protein N-term", ptmDefs.get(0).location());
		Assert.assertEquals('\0', ptmDefs.get(0).residue());
		
		//Parse Residue N-term  modif
		ptmStr = "Ammonia-loss (N-term C)";
		ptmDefs = PTMUtils.parsePTMString(new TestPTMProvider(), ptmStr);
		Assert.assertNotNull(ptmDefs);
		Assert.assertEquals(1, ptmDefs.size());
		Assert.assertEquals("Ammonia-loss", ptmDefs.get(0).names().shortName());
		Assert.assertEquals("Any N-term", ptmDefs.get(0).location());
		Assert.assertEquals('C', ptmDefs.get(0).residue());

		//Parse No residue modif
		ptmStr = "Pro6";
		try {
			ptmDefs = PTMUtils.parsePTMString(new TestPTMProvider(), ptmStr);
			Assert.fail("Expected exception not thrown for no residue / Anywhere modif !");
		} catch (IllegalArgumentException iae){
			
		}
	
		
	}
}
