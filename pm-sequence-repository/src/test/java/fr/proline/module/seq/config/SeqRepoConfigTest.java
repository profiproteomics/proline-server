package fr.proline.module.seq.config;

import java.io.File;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;

public class SeqRepoConfigTest {

	@Test
	public void testParseFastaDir() {
		
		SeqRepoConfig config = SeqRepoConfig.getInstance();
		List<String> filePath = config.getFastaDirectories();
		
		Assert.assertEquals(1, filePath.size());
		File fastaDir = new File(filePath.get(0));		
		Assert.assertTrue(fastaDir.exists());
		Assert.assertTrue(fastaDir.isDirectory());
	}
	
	@Test
	public void testParseParsingRules() {
		
		SeqRepoConfig config = SeqRepoConfig.getInstance();
		List<ParsingRuleEntry> rules = config.getParsingRules();
		
		Assert.assertEquals(2, rules.size());
		ParsingRuleEntry pr1 =  rules.get(0);
		Assert.assertEquals("label1", pr1.getName());
		Assert.assertEquals("_(?:D|(?:Decoy))_(.*)\\.fasta", pr1.getFastaReleaseRegEx());
		Assert.assertEquals(1, pr1.getFastaNameRegExs().size());
		Assert.assertEquals("UP_", pr1.getFastaNameRegExs().get(0));
 
	}
}
