package fr.proline.module.seq.orm;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.*;

@Entity
@Table(name = "se_db_instance")
@NamedQueries(value = {
		@NamedQuery(name = "findSEDbInstanceBySEDbName", query = "SELECT sdi from fr.proline.module.seq.orm.DatabankInstance sdi"
				+ " where sdi.databank.name = :seDbName order by sdi.release"),
		@NamedQuery(name = "findSEDbInstanceByNameAndSourcePath", query = "SELECT sdi from fr.proline.module.seq.orm.DatabankInstance sdi"
				+ " where (sdi.databank.name = :seDbName) and (sdi.sourcePath = :sourcePath) order by sdi.release"),

		@NamedQuery(name = "findSEDbInstanceByNameAndRelease", query = "SELECT sdi from fr.proline.module.seq.orm.DatabankInstance sdi"
				+ " where (sdi.databank.name = :seDbName) and (sdi.release = :release)"), })

public class DatabankInstance implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private long id;

	@Column(length = 50, nullable = false)
	private String release;

	@Column(name = "source_path", nullable = false)
	private String sourcePath;

	@Column(name = "source_last_modified_time", nullable = false)
	private Timestamp sourceLastModifiedTime;

	@ManyToOne(optional = false)
	@JoinColumn(name = "se_db_id")
	private Databank databank;

	@Transient
	private String toString = null;

	@SuppressWarnings("unused")
	private void setId(final long pId) {
		id = pId;
	}

	public long getId() {
		return id;
	}

	public void setRelease(final String pRelease) {
		release = pRelease;
	}

	public String getRelease() {
		return release;
	}

	public void setSourcePath(final String pSourcePath) {
		sourcePath = pSourcePath;
		toString = null;
	}

	public String getSourcePath() {
		return sourcePath;
	}

	public void setSourceLastModifiedTime(final Timestamp timestamp) {

		if (timestamp == null) {
			throw new IllegalArgumentException("Timestamp is null");
		}

		sourceLastModifiedTime = (Timestamp) timestamp.clone();
	}

	public Timestamp getSourceLastModifiedTime() {
		Timestamp result = null;

		if (sourceLastModifiedTime != null) {// Should not be null
			result = (Timestamp) sourceLastModifiedTime.clone();
		}

		return result;
	}

	public void setDatabank(final Databank pSEDb) {
		databank = pSEDb;
		toString = null;
	}

	public Databank getDatabank() {
		return databank;
	}

	@Override
	public String toString() {
		if (toString == null) {
			StringBuilder builder = new StringBuilder().append("[name=").append(databank.getName()).append(", source=").append(sourcePath).append(']');
			toString = builder.toString();
		}
		return toString;
	}
}
