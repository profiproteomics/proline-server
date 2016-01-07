package fr.proline.module.seq.orm;

import java.io.Serializable;
import java.sql.Timestamp;

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
@Table(name = "se_db_instance")
@NamedQueries(value = {
		@NamedQuery(name = "findSEDbInstanceBySEDbName", query = "SELECT sdi from fr.proline.module.seq.orm.SEDbInstance sdi"
				+ " where sdi.seDb.name = :seDbName order by sdi.release"),
		@NamedQuery(name = "findSEDbInstanceByNameAndSourcePath", query = "SELECT sdi from fr.proline.module.seq.orm.SEDbInstance sdi"
				+ " where (sdi.seDb.name = :seDbName) and (sdi.sourcePath = :sourcePath) order by sdi.release"),

		@NamedQuery(name = "findSEDbInstanceByNameAndRelease", query = "SELECT sdi from fr.proline.module.seq.orm.SEDbInstance sdi"
				+ " where (sdi.seDb.name = :seDbName) and (sdi.release = :release)"), })
public class SEDbInstance implements Serializable {

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
	private SEDb seDb;

	//VDS : TODO Use map, may have many Parsing Rule ? ... Actually save last one ! 
	@ManyToOne
	@JoinColumn(name = "parsing_rule_id")
	private ParsingRule parsingRule;
	
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
	}

	public String getSourcePath() {
		return sourcePath;
	}

	public void setParsingRule(final ParsingRule pParsingRule) {
		parsingRule = pParsingRule;
	}

	public ParsingRule getParsingRule() {
		return parsingRule;
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

	public void setSEDb(final SEDb pSEDb) {
		seDb = pSEDb;
	}

	public SEDb getSEDb() {
		return seDb;
	}

}
