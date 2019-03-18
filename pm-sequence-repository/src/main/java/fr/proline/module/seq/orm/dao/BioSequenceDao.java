package fr.proline.module.seq.orm.dao;

import java.util.Collection;
import java.util.List;

import javax.persistence.EntityManager;

import fr.proline.core.orm.util.JPARepositoryUtils;
import fr.proline.module.seq.orm.BioSequence;
import fr.proline.repository.util.JPAUtils;

public final class BioSequenceDao {

	public static List<BioSequence> findBioSequenceByHashes(
		final EntityManager seqEM,
		final Collection<String> hashes) {

		JPAUtils.checkEntityManager(seqEM);

		return JPARepositoryUtils.executeInQueryAsBatch(
			seqEM.createNamedQuery("findBioSequenceByHashes", BioSequence.class), "hashes", hashes);
	}

}
