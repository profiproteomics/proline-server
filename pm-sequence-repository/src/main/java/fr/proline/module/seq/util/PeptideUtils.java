package fr.proline.module.seq.util;

import org.biojava3.aaproperties.IPeptideProperties;
import org.biojava3.aaproperties.PeptidePropertiesImpl;
import org.biojava3.core.sequence.ProteinSequence;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class PeptideUtils {

    private static final Logger LOG = LoggerFactory.getLogger(PeptideUtils.class);

    private static final IPeptideProperties PEPTIDE_PROPERTIES = new PeptidePropertiesImpl();

    private PeptideUtils() {
    }

    public static boolean checkSequence(final String normalizedSequence) {
	boolean valid = false;

	if (normalizedSequence != null) {
	    valid = true; // Optimistic initialization

	    final int sequenceLength = normalizedSequence.length();

	    for (int i = 0; valid && (i < sequenceLength); ++i) {
		final char currentResidue = normalizedSequence.charAt(i);

		if ((currentResidue < 'A') || (currentResidue > 'Z')) {
		    valid = false;
		}

	    }

	}

	return valid;
    }

    public static double calculateMolecularWeight(final String sequence) {

	if (!checkSequence(sequence)) {
	    throw new IllegalArgumentException("Invalid sequence");
	}

	double molecularWeight = 0.0;

	try {
	    final ProteinSequence protSequence = new ProteinSequence(sequence);
	    molecularWeight = PEPTIDE_PROPERTIES.getMolecularWeight(protSequence);
	} catch (Exception ex) {
	    LOG.error("Error calculating molecularWeight", ex);
	}

	return molecularWeight;
    }

    public static double calculateIsoelectricPoint(final String sequence) {

	if (!checkSequence(sequence)) {
	    throw new IllegalArgumentException("Invalid sequence");
	}

	double isoelectricPoint = 0.0;

	try {
	    final ProteinSequence protSequence = new ProteinSequence(sequence);
	    isoelectricPoint = PEPTIDE_PROPERTIES.getIsoelectricPoint(protSequence);
	} catch (Exception ex) {
	    LOG.error("Error calculating isoelectricPoint", ex);
	}

	return isoelectricPoint;
    }

}
