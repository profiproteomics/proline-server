package fr.proline.module.seq.orm;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.Table;

@Entity
@Table(name = "se_db_identifier")
@NamedQueries(value = {
		@NamedQuery(name = "findSEDbIdentByValues", query = "SELECT DISTINCT sdi from fr.proline.module.seq.orm.SEDbIdentifier sdi"
				+ " where sdi.value in (:values)"),
		@NamedQuery(name = "findSEDbIdentBySEDbInstanceAndValues", query = "SELECT DISTINCT sdi from fr.proline.module.seq.orm.SEDbIdentifier sdi"
				+ " where (sdi.seDbInstance = :seDbInstance) and (sdi.value in (:values))"),
		@NamedQuery(name = "findSEDbIdentBySEDbNameAndValues", query = "SELECT DISTINCT sdi from fr.proline.module.seq.orm.SEDbIdentifier sdi"
				+ " where (sdi.seDbInstance.seDb.name = :seDbName) and (sdi.value in (:values))"), })
public class SEDbIdentifier implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private long id;

	@Column(nullable = false)
	private String value;

	private boolean inferred;

	@ManyToOne(optional = false)
	@JoinColumn(name = "se_db_instance_id")
	private SEDbInstance seDbInstance;

	@ManyToOne(optional = false)
	@JoinColumn(name = "bio_sequence_id")
	private BioSequence bioSequence;

	@ManyToOne
	@JoinColumn(name = "repository_identifier_id")
	private RepositoryIdentifier repositoryIdentifier;

	@SuppressWarnings("unused")
	private void setId(final long pId) {
		id = pId;
	}

	public long getId() {
		return id;
	}

	public void setValue(final String pValue) {
		value = pValue;
	}

	public String getValue() {
		return value;
	}

	public void setInferred(final boolean pInferred) {
		inferred = pInferred;
	}

	public boolean isInferred() {
		return inferred;
	}

	public void setBioSequence(final BioSequence pBioSequence) {
		bioSequence = pBioSequence;
	}

	public BioSequence getBioSequence() {
		return bioSequence;
	}

	public void setSEDbInstance(final SEDbInstance pSEDBInstance) {
		seDbInstance = pSEDBInstance;
	}

	public SEDbInstance getSEDbInstance() {
		return seDbInstance;
	}

	public void setRepositoryIdentifier(final RepositoryIdentifier pRepositoryIdentifier) {
		repositoryIdentifier = pRepositoryIdentifier;
	}

	public RepositoryIdentifier getRepositoryIdentifier() {
		return repositoryIdentifier;
	}

}
