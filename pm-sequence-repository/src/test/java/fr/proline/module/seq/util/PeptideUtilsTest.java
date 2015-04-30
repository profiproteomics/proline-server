package fr.proline.module.seq.util;

import static org.junit.Assert.*;

import org.junit.Test;

public class PeptideUtilsTest {

    private static final String FAKE_SEQUENCE = "ARNDCEQGHILKMFPSTWYV";

    @Test
    public void testMass() {
	final double mass = PeptideUtils.calculateMolecularWeight(FAKE_SEQUENCE);
	assertTrue("MolecularWeight", mass > 0.0);
	
    }

    @Test
    public void testPI() {
	final double pI = PeptideUtils.calculateIsoelectricPoint(FAKE_SEQUENCE);
	assertTrue("IsoelectricPoint", pI > 0.0);
    }

}
