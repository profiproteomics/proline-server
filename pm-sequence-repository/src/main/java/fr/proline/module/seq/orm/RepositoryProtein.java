package fr.proline.module.seq.orm;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.NamedQuery;
import javax.persistence.Table;

@Entity
@Table(name = "repository_identifier")
@NamedQuery(name = "findRepositoryIdentByRepoNameAndValues", query = "SELECT DISTINCT ri from fr.proline.module.seq.orm.RepositoryProtein ri"
		+ " where (ri.repository.name = :repositoryName) and (ri.value in (:values))")
public class RepositoryProtein implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private long id;

	@Column(nullable = false)
	private String value;

	@ManyToOne(optional = false)
	@JoinColumn(name = "repository_id")
	private Repository repository;

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

	public void setRepository(final Repository repository) {
		this.repository = repository;
	}

	public Repository getRepository() {
		return repository;
	}

}
