package fr.proline.module.seq.util;

import static org.junit.Assert.*;

import org.junit.Test;

import fr.proline.module.seq.util.HashUtil;

public class HashTest {

    @Test
    public void testHash() {
	doTest("TOTO");
	doTest("TATA");
    }

    private static void doTest(final String sequence) {
	final String hash = HashUtil.calculateSHA256(sequence);

	System.out.printf(">%s<  %d%n", hash, hash.length());
    }

}
