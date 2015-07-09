package fr.proline.module.seq.orm;

public class Results {
	public PeptideResults peptide_results;
	
	public Proteinresults protein_results;
	
	public PeptideResults getPeptide_results() {
		return peptide_results;
	}
	public void setPeptide_results(PeptideResults peptide_results) {
		this.peptide_results = peptide_results;
	}
	public Proteinresults getProtein_results() {
		return protein_results;
	}
	public void setProtein_results(Proteinresults protein_results) {
		this.protein_results = protein_results;
	}

}
