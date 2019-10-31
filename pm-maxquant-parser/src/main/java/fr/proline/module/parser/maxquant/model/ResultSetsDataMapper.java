package fr.proline.module.parser.maxquant.model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import fr.proline.core.om.model.msi.Peptide;
import fr.proline.core.om.model.msi.PeptideMatch;
import fr.proline.core.om.model.msi.ProteinMatch;
import fr.proline.core.om.model.msi.Spectrum;

public class ResultSetsDataMapper {

	private Map<String, List<Peptide>> m_peptidesByResultSet;
	private Map<String, List<PeptideMatch>> m_pepMatchesByResultSet;
	private Map<String, Map<Long, List<ProteinMatch>>> m_protMatchesByPepMatchByResultSet;
	private Map<String, Map<Long, Spectrum>> m_spectrumByIdByResultSet;

	public ResultSetsDataMapper() {
		m_peptidesByResultSet = new HashMap<>();
		m_pepMatchesByResultSet = new HashMap<>();
		m_protMatchesByPepMatchByResultSet = new HashMap<>();
		m_spectrumByIdByResultSet = new HashMap<>();
	}

	public void resetMaps() {
		m_peptidesByResultSet.clear();
		m_pepMatchesByResultSet.clear();
		m_protMatchesByPepMatchByResultSet.clear();
		m_spectrumByIdByResultSet.clear();
	}

	public void setPeptideMatches(String rsName, List<PeptideMatch> peptideMatches) {
		m_pepMatchesByResultSet.put(rsName, peptideMatches);
	}

	public void setPeptides(String rsName, List<Peptide> peptides) {
		m_peptidesByResultSet.put(rsName, peptides);
	}

	public void setProteinMatches(String rsName, Map<Long, List<ProteinMatch>> protMatchesBypepMatch) {
		m_protMatchesByPepMatchByResultSet.put(rsName, protMatchesBypepMatch);
	}

	public void setSpectrums(String rsName, Map<Long, Spectrum> spectrumById) {
		m_spectrumByIdByResultSet.put(rsName, spectrumById);
	}

	public void addPeptideMatches(String rsName, PeptideMatch peptideMatch) {
		if (!m_pepMatchesByResultSet.containsKey(rsName))
			m_pepMatchesByResultSet.put(rsName, new ArrayList<PeptideMatch>());
		m_pepMatchesByResultSet.get(rsName).add(peptideMatch);
	}

	public void addPeptides(String rsName, Peptide peptide) {
		if (!m_peptidesByResultSet.containsKey(rsName))
			m_peptidesByResultSet.put(rsName, new ArrayList<Peptide>());

		if (!m_peptidesByResultSet.get(rsName).contains(peptide))
			m_peptidesByResultSet.get(rsName).add(peptide);
	}

	public void addProteinMatchesToPepMatch(String rsName, Long pepMatchId, List<ProteinMatch> protMatches) {
		if (!m_protMatchesByPepMatchByResultSet.containsKey(rsName))
			m_protMatchesByPepMatchByResultSet.put(rsName, new HashMap<Long, List<ProteinMatch>>());
		Map<Long, List<ProteinMatch>> rsMap = m_protMatchesByPepMatchByResultSet.get(rsName);
		if (!rsMap.containsKey(pepMatchId))
			rsMap.put(pepMatchId, new ArrayList<ProteinMatch>());
		rsMap.get(pepMatchId).addAll(protMatches);
		m_protMatchesByPepMatchByResultSet.put(rsName, rsMap);
	}

	public void addProteinMatchToPepMatch(String rsName, Long pepMatchId, ProteinMatch protMatch) {
		if (!m_protMatchesByPepMatchByResultSet.containsKey(rsName))
			m_protMatchesByPepMatchByResultSet.put(rsName, new HashMap<Long, List<ProteinMatch>>());
		Map<Long, List<ProteinMatch>> rsMap = m_protMatchesByPepMatchByResultSet.get(rsName);
		if (!rsMap.containsKey(pepMatchId))
			rsMap.put(pepMatchId, new ArrayList<ProteinMatch>());
		rsMap.get(pepMatchId).add(protMatch);
		m_protMatchesByPepMatchByResultSet.put(rsName, rsMap);
	}

	public void addSpectrum(String rsName, Long spectrumId, Spectrum newSpectrum) {
		if (!m_spectrumByIdByResultSet.containsKey(rsName))
			m_spectrumByIdByResultSet.put(rsName, new HashMap<Long, Spectrum>());
		m_spectrumByIdByResultSet.get(rsName).put(spectrumId, newSpectrum);

	}

	public List<Peptide> getPeptidesForRs(String rsName) {
		return m_peptidesByResultSet.get(rsName);
	}

	public List<PeptideMatch> getPeptideMatchesForRs(String rsName) {
		return m_pepMatchesByResultSet.get(rsName);
	}

	public Map<Long, List<ProteinMatch>> getProteinMatchesForRs(String rsName) {
		return m_protMatchesByPepMatchByResultSet.get(rsName);
	}

	public Map<Long, Spectrum> getSpectrumByIdForRs(String rsName) {
		return m_spectrumByIdByResultSet.get(rsName);
	}

}
