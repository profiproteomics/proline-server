package fr.proline.module.seq.util;

import org.biojava.nbio.aaproperties.Constraints;
import org.biojava.nbio.aaproperties.IPeptideProperties;
import org.biojava.nbio.aaproperties.PeptidePropertiesImpl;
import org.biojava.nbio.core.sequence.ProteinSequence;
import org.biojava.nbio.core.sequence.compound.AminoAcidCompound;
import org.biojava.nbio.core.sequence.compound.AminoAcidCompoundSet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class PeptideUtils {

	private static final Logger LOG = LoggerFactory.getLogger(PeptideUtils.class);

	private static final IPeptideProperties PEPTIDE_PROPERTIES = new PeptidePropertiesImpl();

	static {
		
		AminoAcidCompoundSet aaSet = new AminoAcidCompoundSet();
		AminoAcidCompound U = aaSet.getCompoundForString("U");
		AminoAcidCompound J = aaSet.getCompoundForString("J");
		AminoAcidCompound B = aaSet.getCompoundForString("B");
		AminoAcidCompound X = aaSet.getCompoundForString("X");
		AminoAcidCompound Z = aaSet.getCompoundForString("Z");
			
		Constraints.aa2MolecularWeight.put(U, new Double(U.getMolecularWeight()));
		Constraints.aa2MolecularWeight.put(J, (aaSet.getCompoundForString("I").getMolecularWeight() + aaSet.getCompoundForString("L").getMolecularWeight())/2.0);
		Constraints.aa2MolecularWeight.put(B, (aaSet.getCompoundForString("N").getMolecularWeight() + aaSet.getCompoundForString("D").getMolecularWeight())/2.0);
		Constraints.aa2MolecularWeight.put(Z, (aaSet.getCompoundForString("E").getMolecularWeight() + aaSet.getCompoundForString("Q").getMolecularWeight())/2.0);
		Constraints.aa2MolecularWeight.put(X, 110.0);
		
	}
	
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
