package fr.proline.module.parser.maxquant.model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import fr.proline.core.om.model.msi.Peptide;
import fr.proline.core.om.model.msi.PeptideMatch;
import fr.proline.core.om.model.msi.ProteinMatch;

public class ResultSetsDataMapper {

	private Map<String, List<Peptide>> m_peptidesByResultSet;
	private Map<String, List<PeptideMatch>> m_pepMatchesByResultSet;
	private Map<String, List<ProteinMatch>> m_protMatchesByResultSet;

	public ResultSetsDataMapper() {
		m_peptidesByResultSet = new HashMap<>();
		m_pepMatchesByResultSet = new HashMap<>();
		m_protMatchesByResultSet = new HashMap<>();
	}
	
	public void resetMaps(){
		m_peptidesByResultSet.clear();
		m_pepMatchesByResultSet.clear();
		m_protMatchesByResultSet.clear();
	}

	public void setPeptideMatches(String rsName, List<PeptideMatch> peptideMatches) {
		m_pepMatchesByResultSet.put(rsName, peptideMatches);
	}

	public void setPeptides(String rsName, List<Peptide> peptides) {
		m_peptidesByResultSet.put(rsName, peptides);
	}

	public void setProteinMatches(String rsName, List<ProteinMatch> protMatches) {
		m_protMatchesByResultSet.put(rsName, protMatches);
	}

	public void addPeptideMatches(String rsName, PeptideMatch peptideMatch) {
		if (!m_pepMatchesByResultSet.containsKey(rsName))
			m_pepMatchesByResultSet.put(rsName, new ArrayList<PeptideMatch>());
		m_pepMatchesByResultSet.get(rsName).add(peptideMatch);
	}

	public void addPeptides(String rsName, Peptide peptide) {
		if (!m_peptidesByResultSet.containsKey(rsName))
			m_peptidesByResultSet.put(rsName, new ArrayList<Peptide>());
		m_peptidesByResultSet.get(rsName).add(peptide);
	}

	public void addProteinMatches(String rsName, ProteinMatch protMatch) {
		if (!m_protMatchesByResultSet.containsKey(rsName))
			m_protMatchesByResultSet.put(rsName, new ArrayList<ProteinMatch>());
		m_protMatchesByResultSet.get(rsName).add(protMatch);
	}

	public List<Peptide> getPeptidesForRs(String rsName) {
		return m_peptidesByResultSet.get(rsName);
	}

	public List<PeptideMatch> getPeptideMatchesForRs(String rsName) {
		return m_pepMatchesByResultSet.get(rsName);
	}

	public List<ProteinMatch> getProteinMatchesForRs(String rsName) {
		return m_protMatchesByResultSet.get(rsName);
	}

}
