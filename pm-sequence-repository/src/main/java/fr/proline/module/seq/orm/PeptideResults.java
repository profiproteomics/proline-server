package fr.proline.module.seq.orm;

public class PeptideResults {
	public String target_matches_count;
	public String decoy_matches_count;
	public String fdr;
 public String getTarget_matches_count() {
		return target_matches_count;
	}
	public void setTarget_matches_count(String target_matches_count) {
		this.target_matches_count = target_matches_count;
	}
	public String getDecoy_matches_count() {
		return decoy_matches_count;
	}
	public void setDecoy_matches_count(String decoy_matches_count) {
		this.decoy_matches_count = decoy_matches_count;
	}
	public String getFdr() {
		return fdr;
	}
	public void setFdr(String fdr) {
		this.fdr = fdr;
	}


}
