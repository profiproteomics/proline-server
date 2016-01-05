package fr.proline.module.seq.config;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import com.typesafe.config.ConfigList;
import com.typesafe.config.ConfigValue;

public class SeqRepoConfig {
	
	@SuppressWarnings("unused")
	private static final Logger LOG = LoggerFactory.getLogger(SeqRepoConfig.class);

	private static final String PARSING_RULES_KEY = "parsing-rules";
	private static final String PR_NAME_KEY = "name";
	private static final String PR_FASTA_RELEASE_KEY ="fasta-version";
	private static final String PR_FASTA_NAME_KEY ="fasta-name";
	private static final String PR_PROT_ACC_KEY ="protein-accession";
	private static final String FASTA_DIR_KEY  = "local-fasta-directories";	
	private static final String DEFAULT_PROT_ACC_RULE_KEY  = "default-protein-accession";
		
	private Map<String, Object> m_allProperties; 
	private static final Object CONFIGURATION_LOCK = new Object();

	private static SeqRepoConfig instance;
	private Config m_seqRepoConfig = null;
	
	private SeqRepoConfig(){
		m_seqRepoConfig = ConfigFactory.load( "parsing-rules");
	}
	
	public static SeqRepoConfig getInstance(){
		if (instance == null)
			instance = new SeqRepoConfig();
		return instance;
	}
	
	private Config getSeqRepoConfig(){
		if(m_seqRepoConfig == null)
			m_seqRepoConfig = ConfigFactory.load( "parsing-rules");
		return m_seqRepoConfig;
	}
	
	@SuppressWarnings({ "rawtypes", "unchecked" })
	private void loadProperties(){
		synchronized (CONFIGURATION_LOCK) {
			m_allProperties = new HashMap<>();
			
			//Read fastaLocalPath 
			ArrayList<String> fastaDirs = new ArrayList<String>();
    		if (getSeqRepoConfig().hasPath(FASTA_DIR_KEY)) {
    			List<Object> fastaDirsObj =  getSeqRepoConfig().getList(FASTA_DIR_KEY).unwrapped();
    			for (Object nextEntry : fastaDirsObj) {
    				fastaDirs.add(nextEntry.toString());
				}
    		}
    		m_allProperties.put(FASTA_DIR_KEY, fastaDirs);
    		
    		//Read Default Protein Acc RegEx
    		String defProtAcc = null;
    		if (getSeqRepoConfig().hasPath(DEFAULT_PROT_ACC_RULE_KEY)) {
    			defProtAcc = getSeqRepoConfig().getString(DEFAULT_PROT_ACC_RULE_KEY);
    		}
    		m_allProperties.put(DEFAULT_PROT_ACC_RULE_KEY, defProtAcc);
    		
    		//Read Parsing Rules
    		List<ParsingRuleEntry> parsingRules = new ArrayList<>();
    		if (!getSeqRepoConfig().hasPath(PARSING_RULES_KEY)) {
    			throw new RuntimeException("No Parsing rules specidied");
    		}
    		ConfigList parsingRulesDef = getSeqRepoConfig().getList(PARSING_RULES_KEY);
    		for(ConfigValue nextEntry : parsingRulesDef){
    			Map cv =(Map<String, Object>) nextEntry.unwrapped();
    			ParsingRuleEntry pre = new ParsingRuleEntry((String)cv.get(PR_NAME_KEY), (List<String>) cv.get(PR_FASTA_NAME_KEY),(String) cv.get(PR_FASTA_RELEASE_KEY),(String) cv.get(PR_PROT_ACC_KEY));
    			parsingRules.add(pre);
    		}
    		
    		m_allProperties.put(PARSING_RULES_KEY, parsingRules);
		}
	}
	
	public static void forcePropertiesReload(){
		synchronized (CONFIGURATION_LOCK) {
			if(instance != null){
				instance.m_seqRepoConfig = null;
				instance.m_allProperties = null;
			}
		}
	}
	
	private Map<String, Object> getProperties(){
		if(m_allProperties  == null)
			loadProperties();
		return m_allProperties;
	}
	
	@SuppressWarnings("unchecked")
	public List<String> getFastaDirectories(){	
		return (List<String>) getProperties().get(FASTA_DIR_KEY);
	}
	
	public String getDefaultProtAccRegEx(){		
		return (String) getProperties().get(DEFAULT_PROT_ACC_RULE_KEY);
	}
		
	@SuppressWarnings("unchecked")
	public List<ParsingRuleEntry> getParsingRules(){
		return (List<ParsingRuleEntry>) getProperties().get(PARSING_RULES_KEY);
		
	}
	
	
	
}
