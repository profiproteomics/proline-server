package fr.proline.module.seq.orm;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.NamedQuery;
import javax.persistence.Table;

@Entity
@Table(name = "parsing_rule")
@NamedQuery(name = "findParsingRuleByName", query = "SELECT pr from fr.proline.module.seq.orm.ParsingRule pr where pr.name = :name")
public class ParsingRule implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private long id;

	@Column(nullable = false)
	private String name;

	private String release;

	@Column(name = "se_db_identifier", nullable = false)
	private String seDbIdentifier;

	@Column(name = "repository_identifier")
	private String repositoryIdentifier;

	@Column(name = "repo_id_from_se_id")
	private String repoIdFromSEId;

	@SuppressWarnings("unused")
	private void setId(final long pId) {
		id = pId;
	}

	public long getId() {
		return id;
	}

	public void setName(final String pName) {
		name = pName;
	}

	public String getName() {
		return name;
	}

	public void setRelease(final String pRelease) {
		release = pRelease;
	}

	public String getRelease() {
		return release;
	}

	public void setSEDbIdentifier(final String pSEDbIdentifier) {
		seDbIdentifier = pSEDbIdentifier;
	}

	public String getSEDbIdentifier() {
		return seDbIdentifier;
	}

	public void setRepositoryIdentifier(final String pRepositoryIdent) {
		repositoryIdentifier = pRepositoryIdent;
	}

	public String getRepositoryIdentifier() {
		return repositoryIdentifier;
	}

	public void setRepoIdFromSEId(final String pRepoIdFromSeId) {
		repoIdFromSEId = pRepoIdFromSeId;
	}

	public String getRepoIdFromSEId() {
		return repoIdFromSEId;
	}

}
