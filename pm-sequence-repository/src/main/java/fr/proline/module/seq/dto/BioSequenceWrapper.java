package fr.proline.module.seq.dto;

import java.io.Serializable;

import fr.proline.module.seq.orm.Alphabet;
import fr.proline.module.seq.util.PeptideUtils;
import fr.proline.util.StringUtils;

public class BioSequenceWrapper implements Serializable {

    private static final long serialVersionUID = 2L;

    private final String m_sequence;

    private final Object m_lazyLock = new Object();

    private Double m_mass;

    private Double m_pi;

    private final SEDbInstanceWrapper m_seDbInstance;

    private final String m_seDbRelease;

    private final RepositoryIdentifierWrapper m_repositoryIdentifier;

    public BioSequenceWrapper(final String sequence, final SEDbInstanceWrapper seDbInstance,
	    final String seDbRelease, final RepositoryIdentifierWrapper repositoryIdentifier) {

	if (StringUtils.isEmpty(sequence)) {
	    throw new IllegalArgumentException("Invalid sequence");
	}

	m_sequence = sequence;

	if (seDbInstance == null) {
	    throw new IllegalArgumentException("SEDbInstance wrapper is null");
	}

	m_seDbInstance = seDbInstance;

	if (StringUtils.isEmpty(seDbRelease)) {
	    throw new IllegalArgumentException("Invalid seDbRelease");
	}

	m_seDbRelease = seDbRelease;

	m_repositoryIdentifier = repositoryIdentifier;
    }

    public String getSequence() {
	return m_sequence;
    }

    public double getMass() {
	double mass = 0.0;

	synchronized (m_lazyLock) {

	    if (m_mass == null) {
		final SEDbInstanceWrapper seDbInstance = getSEDbInstance(); // Should not be null

		final Alphabet alphabet = seDbInstance.getAlphabet();

		if (alphabet == Alphabet.AA) {
		    final String sequence = getSequence(); // Should not be null
		    mass = PeptideUtils.calculateMolecularWeight(sequence);
		}

		m_mass = Double.valueOf(mass); // Cache calculated value
	    } else {
		mass = m_mass.doubleValue();
	    }

	} // End of synchronized block on m_lazyLock

	return mass;
    }

    public double getPI() {
	double pI = 0.0;

	synchronized (m_lazyLock) {

	    if (m_pi == null) {
		final SEDbInstanceWrapper seDbInstance = getSEDbInstance(); // Should not be null

		final Alphabet alphabet = seDbInstance.getAlphabet();

		if (alphabet == Alphabet.AA) {
		    final String sequence = getSequence(); // Should not be null
		    pI = PeptideUtils.calculateIsoelectricPoint(sequence);
		}

		m_pi = Double.valueOf(pI); // Cache calculated value
	    } else {
		pI = m_pi.doubleValue();
	    }

	} // End of synchronized block on m_lazyLock

	return pI;
    }

    public SEDbInstanceWrapper getSEDbInstance() {
	return m_seDbInstance;
    }

    public String getSEDbRelease() {
	return m_seDbRelease;
    }

    /**
     * Can be <code>null</code>.
     * 
     * @return
     */
    public RepositoryIdentifierWrapper getRepositoryIdentifier() {
	return m_repositoryIdentifier;
    }

}
