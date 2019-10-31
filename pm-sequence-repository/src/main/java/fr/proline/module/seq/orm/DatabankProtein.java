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
		@NamedQuery(name = "findSEDbIdentByValues", query = "SELECT DISTINCT sdi from fr.proline.module.seq.orm.DatabankProtein sdi"
				+ " where sdi.identifier in (:values)"),
		@NamedQuery(name = "findSEDbIdentBySEDbInstanceAndValues", query = "SELECT DISTINCT sdi from fr.proline.module.seq.orm.DatabankProtein sdi"
				+ " where (sdi.databankInstance = :seDbInstance) and (sdi.identifier in (:values))"),
		@NamedQuery(name = "findSEDbIdentBySEDbNameAndValues", query = "SELECT DISTINCT sdi from fr.proline.module.seq.orm.DatabankProtein sdi"
				+ " where (sdi.databankInstance.databank.name = :seDbName) and (sdi.identifier in (:values))"), })
public class DatabankProtein implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private long id;

	@Column(name = "value", nullable = false)
	private String identifier;

	private boolean inferred;

	private String description;
	
	@ManyToOne(optional = false)
	@JoinColumn(name = "se_db_instance_id")
	private DatabankInstance databankInstance;

	@ManyToOne(optional = false)
	@JoinColumn(name = "bio_sequence_id")
	private BioSequence bioSequence;

	@ManyToOne
	@JoinColumn(name = "repository_identifier_id")
	private RepositoryProtein repositoryIdentifier;

	@SuppressWarnings("unused")
	private void setId(final long pId) {
		id = pId;
	}

	public long getId() {
		return id;
	}

	public void setIdentifier(final String pValue) {
		identifier = pValue;
	}

	public String getIdentifier() {
		return identifier;
	}

	public void setDescription(final String pdescription) {
		description = pdescription;
	}

	public String getDescription() {
		return description;
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

	public void setDatabankInstance(final DatabankInstance pSEDBInstance) {
		databankInstance = pSEDBInstance;
	}

	public DatabankInstance getDatabankInstance() {
		return databankInstance;
	}

	public void setRepositoryIdentifier(final RepositoryProtein pRepositoryIdentifier) {
		repositoryIdentifier = pRepositoryIdentifier;
	}

	public RepositoryProtein getRepositoryIdentifier() {
		return repositoryIdentifier;
	}

}
