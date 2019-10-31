package fr.proline.module.seq.dto;

import java.io.Serializable;

import fr.proline.module.seq.orm.Alphabet;
import fr.proline.module.seq.util.PeptideUtils;
import fr.profi.util.StringUtils;

public class DBioSequence implements Serializable {

	private static final long serialVersionUID = 3L;

	private final long m_sequenceId;

	private final String m_sequence;

	private final Object m_lazyLock = new Object();

	/* All mutable fields are @GuardedBy("m_lazyLock") */
	private Double m_mass;

	private Double m_pi;

	private final DDatabankInstance m_seDbInstance;

	private final String m_seDbRelease;

	private final DRepositoryProtein m_repositoryIdentifier;

	public DBioSequence(
		final long sequenceId,
		final String sequence,
		final DDatabankInstance seDbInstance,
		final String seDbRelease,
		final DRepositoryProtein repositoryIdentifier) {

		m_sequenceId = sequenceId;

		if (StringUtils.isEmpty(sequence)) {
			throw new IllegalArgumentException("Invalid sequence");
		}

		m_sequence = sequence;

		if (seDbInstance == null) {
			throw new IllegalArgumentException("DatabankInstance wrapper is null");
		}

		m_seDbInstance = seDbInstance;

		if (StringUtils.isEmpty(seDbRelease)) {
			throw new IllegalArgumentException("Invalid seDbRelease");
		}

		m_seDbRelease = seDbRelease;

		m_repositoryIdentifier = repositoryIdentifier;
	}

	/**
	 * Id of the BioSequence Entity.
	 * 
	 * @return Id of the BioSequence Entity in SEQ Db.
	 */
	public long getSequenceId() {
		return m_sequenceId;
	}

	/**
	 * Value of the sequence.
	 * 
	 * @return Value of the sequence (normalized to Upper Case [A-Z] without whitespace chars).
	 */
	public String getSequence() {
		return m_sequence;
	}

	public double getMass() {
		double mass = 0.0;

		synchronized (m_lazyLock) {

			if (m_mass == null) {
				final DDatabankInstance seDbInstance = getSEDbInstance(); // Should not be null

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
				final DDatabankInstance seDbInstance = getSEDbInstance(); // Should not be null

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

	public DDatabankInstance getSEDbInstance() {
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
	public DRepositoryProtein getRepositoryIdentifier() {
		return m_repositoryIdentifier;
	}

}
