package fr.proline.module.seq.orm;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "parsing_rule")
public class ParsingRule implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private long id;

	@Column(nullable = false)
	private String name;

	@Column(name = "release_identifier")
	private String releaseRegEx;

	@Column(name = "se_db_identifier", nullable = false)
	private String seDbNameRegEx;

	@Column(name = "repository_identifier")
	private String repositoryNameRegEx;

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

	public void setReleaseRegEx(final String pRelease) {
		releaseRegEx = pRelease;
	}

	public String getReleaseRegEx() {
		return releaseRegEx;
	}

	public void setSEDbNameRegEx(final String pSEDbIdentifier) {
		seDbNameRegEx = pSEDbIdentifier;
	}

	public String getSEDbNameRegEx() {
		return seDbNameRegEx;
	}

	public void setRepositoryNameRegEx(final String pRepositoryIdent) {
		repositoryNameRegEx = pRepositoryIdent;
	}

	public String getRepositoryNameRegEx() {
		return repositoryNameRegEx;
	}

	public void setRepoIdFromSEId(final String pRepoIdFromSeId) {
		repoIdFromSEId = pRepoIdFromSeId;
	}

	public String getRepoIdFromSEId() {
		return repoIdFromSEId;
	}

}
