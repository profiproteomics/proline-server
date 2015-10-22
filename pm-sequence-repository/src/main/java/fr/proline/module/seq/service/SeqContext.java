package fr.proline.module.seq.service;

import java.util.List;
import java.util.Map;

import javax.persistence.EntityManager;

import fr.proline.module.seq.orm.BioSequence;
import fr.proline.module.seq.orm.Repository;
import fr.proline.module.seq.orm.RepositoryIdentifier;
import fr.proline.module.seq.orm.SEDbIdentifier;
import fr.proline.module.seq.orm.SEDbInstance;
import fr.proline.repository.util.JPAUtils;

public class SeqContext {

	private final EntityManager m_seqEM;

	private final SEDbInstance m_seDbInstance;

	private final Map<String, List<SEDbIdentifier>> m_existingSEDbIdents;

	private final Map<String, BioSequence> m_existingBioSequences;

	private final Repository m_repository;

	private final Map<String, RepositoryIdentifier> m_existingRepositoryIdents;

	public SeqContext(
		final EntityManager seqEM,
		final SEDbInstance seDbInstance,
		final Map<String, List<SEDbIdentifier>> existingSEDbIdents,
		final Map<String, BioSequence> existingBioSequences,
		final Repository repository,
		final Map<String, RepositoryIdentifier> existingRepositoryIdents) {

		JPAUtils.checkEntityManager(seqEM);

		m_seqEM = seqEM;

		if (seDbInstance == null) {
			throw new IllegalArgumentException("SeDbInstance is null");
		}

		m_seDbInstance = seDbInstance;

		if (existingSEDbIdents == null) {
			throw new IllegalArgumentException("ExistingSEDbIdents is null");
		}

		m_existingSEDbIdents = existingSEDbIdents;

		if (existingBioSequences == null) {
			throw new IllegalArgumentException("ExistingBioSequences is null");
		}

		m_existingBioSequences = existingBioSequences;

		m_repository = repository;

		m_existingRepositoryIdents = existingRepositoryIdents;
	}

	public EntityManager getSeqEM() {
		return m_seqEM;
	}

	public SEDbInstance getSEDbInstance() {
		return m_seDbInstance;
	}

	public Map<String, List<SEDbIdentifier>> getExistingSEDbIdents() {
		return m_existingSEDbIdents;
	}

	public Map<String, BioSequence> getExistingBioSequences() {
		return m_existingBioSequences;
	}

	/**
	 * Can be <code>null</code>
	 * 
	 * @return
	 */
	public Repository getRepository() {
		return m_repository;
	}

	/**
	 * Can be <code>null</code>
	 * 
	 * @return
	 */
	public Map<String, RepositoryIdentifier> getExistingRepositoryIdents() {
		return m_existingRepositoryIdents;
	}

}
