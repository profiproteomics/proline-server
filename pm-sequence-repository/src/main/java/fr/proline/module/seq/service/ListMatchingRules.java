package fr.proline.module.seq.service;

import static fr.proline.module.seq.Constants.LATIN_1_CHARSET;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import fr.proline.module.seq.config.ParsingRuleEntry;
import fr.proline.module.seq.config.SeqRepoConfig;
import fr.proline.module.seq.util.RegExUtil;

public class ListMatchingRules {

	List<String> m_fastaDirs;
	private static final Logger LOG = LoggerFactory.getLogger(ListMatchingRules.class);
    
	public static void main(final String[] args) {

		try {
			DataSourceBuilder dsBuilder = new DataSourceBuilder();
			Map<String, List<File>> fastaPaths = dsBuilder.getFastaFiles();
			LOG.info(" ---- Scanning Fasta local path ---- ");
			Set<Map.Entry<String, List<File>>> entries = fastaPaths.entrySet();
	
			for (Map.Entry<String, List<File>> entry : entries) {
	
				String fastaName = entry.getKey();
				List<File> fastaFiles = entry.getValue();
	
				ParsingRuleEntry rule = ParsingRuleEntry.getParsingRuleEntry(fastaName);
				String m_seDbIdentRegEx = null;
				if (rule != null) {
					LOG.info(" Using rule \"{}\" for \"{}\" ", rule.getProteinAccRegEx(), fastaName);
					String releaseRegEx = rule.getFastaReleaseRegEx();
					LOG.info("   Release (using rule \"{}\") = \"{}\" ", releaseRegEx, RegExUtil.parseReleaseVersion(fastaName, releaseRegEx));
					m_seDbIdentRegEx = rule.getProteinAccRegEx();
				} else {
					LOG.info(" Using default rule \"{}\" for fasta \"{}\" ", SeqRepoConfig.getInstance().getDefaultProtAccRegEx(), fastaName);
					m_seDbIdentRegEx = SeqRepoConfig.getInstance().getDefaultProtAccRegEx();
				}
	
				//Read 3 entries in fasta files using ParsingRuleEntry regEx
				for (File nextFile : fastaFiles) {
					BufferedReader br = null;
					try {
	    				InputStream is = new FileInputStream(nextFile);
	    				 br = new BufferedReader(new InputStreamReader(is, LATIN_1_CHARSET));
	    
	    				String rawLine = br.readLine();
	    				int countEntry = 0;
	    				while (countEntry < 3 && rawLine != null) {
	    
	    					final String trimmedLine = rawLine.trim();
	    					if (!trimmedLine.isEmpty() && trimmedLine.startsWith(">")) { //Found an entry
	    						countEntry++;
	    
	    						String foundEntry = RegExUtil.getMatchingString(trimmedLine, m_seDbIdentRegEx);
	    						if(foundEntry == null)
	    							LOG.warn("\t !! No accession group found for entry \"{}\".", trimmedLine);
	    						else
	    							LOG.info("\t Accession \"{}\" will be used for entry \"{}\".", foundEntry, trimmedLine);
	
	    					} // End entryFound
	    					rawLine = br.readLine();
	    				} // End read some entries
					
					} finally {
	
						if (br != null) {
							try {
								br.close();
							} catch (IOException exClose) {
								LOG.error("Error closing [" + nextFile + ']', exClose);
							}
						}
	
					}
				} //End go through associated fasta files
	
			} //End go through fasta paths
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();				
		} 		
	}
}
