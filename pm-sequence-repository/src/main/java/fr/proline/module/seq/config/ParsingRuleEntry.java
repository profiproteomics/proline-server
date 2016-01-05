package fr.proline.module.seq.config;

import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ParsingRuleEntry {

	private String name;
	private List<String> fastaNameRegExs;
	private String fastaReleaseRegEx;
	private String proteinAccRegEx;

	private static final Logger LOG = LoggerFactory.getLogger(ParsingRuleEntry.class);
	
	
	public ParsingRuleEntry(String name, List<String> fastaNameRegExs, String fastaReleaseRegEx, String proteinAccRegEx) {
		super();
		this.name = name;
		this.fastaNameRegExs = fastaNameRegExs;
		this.fastaReleaseRegEx = fastaReleaseRegEx;
		this.proteinAccRegEx = proteinAccRegEx;
	}
	
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}

	public List<String> getFastaNameRegExs() {
		return fastaNameRegExs;
	}

	public void setFastaNameRegExs(List<String> fastaNameRegExs) {
		this.fastaNameRegExs = fastaNameRegExs;
	}

	public String getFastaReleaseRegEx() {
		return fastaReleaseRegEx;
	}
	public void setFastaReleaseRegEx(String fastaReleaseRegEx) {
		this.fastaReleaseRegEx = fastaReleaseRegEx;
	}
	public String getProteinAccRegEx() {
		return proteinAccRegEx;
	}
	public void setProteinAccRegEx(String proteinAccRegEx) {
		this.proteinAccRegEx = proteinAccRegEx;
	}
	
	public static ParsingRuleEntry getParsingRuleEntry(final String fastaFileName) {
		assert (fastaFileName != null) : "getParsingRuleEntry() fastaFileName is null";

		ParsingRuleEntry result = null;
		for(ParsingRuleEntry nextPR : SeqRepoConfig.getInstance().getParsingRules()){
			for(String fastaRegEx : nextPR.getFastaNameRegExs()){
				final Pattern pattern = Pattern.compile(fastaRegEx, Pattern.CASE_INSENSITIVE);
				final Matcher matcher = pattern.matcher(fastaFileName);				
				if (matcher.find()) {
					LOG.debug("[{}] matches Fasta Name Regex \"{}\"", fastaFileName, fastaRegEx);
					result = nextPR;
					break;
				}

			} // End loop for each regex
			if( result!=null)
				break;
		}

		return result;
	}
	
	
}
