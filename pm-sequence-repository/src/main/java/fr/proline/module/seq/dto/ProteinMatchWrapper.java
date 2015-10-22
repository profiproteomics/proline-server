package fr.proline.module.seq.dto;
import java.io.Serializable;

import fr.proline.core.orm.msi.ResultSet;

public class ProteinMatchWrapper implements Serializable {
	
	private static final long serialVersionUID = 1L;
	private Long id;
	private String accession;
	private Long bioSequenceId;
	private boolean isLastBioSequence;
	private float coverage;
	private String description;
	private String geneName;
	private boolean isDecoy;
	private int peptideCount;
	private int peptideMatchCount;
	private ResultSet resultSet;
	private Float score;
	private long scoringId;
	private Long taxonId;
	public Long getId() {
		return id;
	}
	public void setId(Long id) {
		this.id = id;
	}
	public String getAccession() {
		return accession;
	}
	public void setAccession(String accession) {
		this.accession = accession;
	}
	public Long getBioSequenceId() {
		return bioSequenceId;
	}
	public void setBioSequenceId(Long bioSequenceId) {
		this.bioSequenceId = bioSequenceId;
	}
	public boolean isLastBioSequence() {
		return isLastBioSequence;
	}
	public void setLastBioSequence(boolean isLastBioSequence) {
		this.isLastBioSequence = isLastBioSequence;
	}
	public float getCoverage() {
		return coverage;
	}
	public void setCoverage(float coverage) {
		this.coverage = coverage;
	}
	public String getDescription() {
		return description;
	}
	public void setDescription(String description) {
		this.description = description;
	}
	public String getGeneName() {
		return geneName;
	}
	public void setGeneName(String geneName) {
		this.geneName = geneName;
	}
	public boolean isDecoy() {
		return isDecoy;
	}
	public void setDecoy(boolean isDecoy) {
		this.isDecoy = isDecoy;
	}
	public int getPeptideCount() {
		return peptideCount;
	}
	public void setPeptideCount(int peptideCount) {
		this.peptideCount = peptideCount;
	}
	public int getPeptideMatchCount() {
		return peptideMatchCount;
	}
	public void setPeptideMatchCount(int peptideMatchCount) {
		this.peptideMatchCount = peptideMatchCount;
	}
	public ResultSet getResultSet() {
		return resultSet;
	}
	public void setResultSet(ResultSet resultSet) {
		this.resultSet = resultSet;
	}
	public Float getScore() {
		return score;
	}
	public void setScore(Float score) {
		this.score = score;
	}
	public long getScoringId() {
		return scoringId;
	}
	public void setScoringId(long scoringId) {
		this.scoringId = scoringId;
	}
	public Long getTaxonId() {
		return taxonId;
	}
	public void setTaxonId(Long taxonId) {
		this.taxonId = taxonId;
	}
	public ProteinMatchWrapper(Long id, String accession) {
		super();
		this.id = id;
		this.accession = accession;
		
	}
}
