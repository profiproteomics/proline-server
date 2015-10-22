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
@Table(name = "repository")
@NamedQuery(name = "findRepositoryByName", query = "SELECT repo from fr.proline.module.seq.orm.Repository repo where repo.name = :name")
public class Repository implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private long id;

	@Column(nullable = false)
	private String name;

	private String url;

	public void setId(final long pId) {
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

	public void setURL(final String pURL) {
		url = pURL;
	}

	public String getURL() {
		return url;
	}

}
