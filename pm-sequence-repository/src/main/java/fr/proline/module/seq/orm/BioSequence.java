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
@Table(name = "bio_sequence")
@NamedQuery(name = "findBioSequenceByHashes", query = "SELECT DISTINCT bs from fr.proline.module.seq.orm.BioSequence bs where bs.hash in (:hashes)")
public class BioSequence implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private long id;

	@Column(nullable = false)
	private String sequence;

	@Column(length = 64, nullable = false)
	private String hash;

	@SuppressWarnings("unused")
	private void setId(final long pId) {
		id = pId;
	}

	public long getId() {
		return id;
	}

	public void setSequence(final String pSequence) {
		sequence = pSequence;
	}

	public String getSequence() {
		return sequence;
	}

	public void setHash(final String pHash) {
		hash = pHash;
	}

	public String getHash() {
		return hash;
	}

}
