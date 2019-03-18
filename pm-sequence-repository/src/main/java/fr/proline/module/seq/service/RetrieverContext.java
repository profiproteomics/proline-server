package fr.proline.module.seq.service;

import java.util.List;
import java.util.Map;

import javax.persistence.EntityManager;

import fr.proline.module.seq.orm.*;
import fr.proline.module.seq.orm.DatabankInstance;
import fr.proline.module.seq.util.Counters;
import fr.proline.repository.util.JPAUtils;

public class RetrieverContext {

	private final EntityManager m_seqEM;
	private final DatabankInstance m_databankInstance;
	private final Map<String, List<DatabankProtein>> m_existingProteins;
	private final Map<String, BioSequence> m_existingBioSequences;
	private final Repository m_repository;
	private final Map<String, RepositoryProtein> m_existingRepositoryProteins;
	private Counters m_counters;

	public RetrieverContext(
		final EntityManager seqEM,
		final DatabankInstance seDbInstance,
		final Map<String, List<DatabankProtein>> existingSEDbIdents,
		final Map<String, BioSequence> existingBioSequences,
		final Repository repository,
		final Map<String, RepositoryProtein> existingRepositoryIdents,
		final Counters counters) {

		assert (seDbInstance != null)  : "SeDbInstance is null";
		assert (existingSEDbIdents != null) : "ExistingSEDbIdents is null";
		assert (existingBioSequences != null) : "ExistingBioSequences is null";

		JPAUtils.checkEntityManager(seqEM);

		m_seqEM = seqEM;
		m_databankInstance = seDbInstance;
		m_existingProteins = existingSEDbIdents;
		m_existingBioSequences = existingBioSequences;
		m_repository = repository;
		m_existingRepositoryProteins = existingRepositoryIdents;
		m_counters = (counters == null) ? new Counters("Retriever") : counters;
	}

	public EntityManager getSeqEM() {
		return m_seqEM;
	}

	public DatabankInstance getDatabankInstance() {
		return m_databankInstance;
	}

	public Map<String, List<DatabankProtein>> getExistingProteins() {
		return m_existingProteins;
	}

	public Map<String, BioSequence> getExistingBioSequences() {
		return m_existingBioSequences;
	}

	public Repository getRepository() {
		return m_repository;
	}

	public Map<String, RepositoryProtein> getExistingRepositoryIdents() {
		return m_existingRepositoryProteins;
	}

	public Counters getCounters() {
		return m_counters;
	}
}
