package fr.proline.module.seq.util;

import static fr.proline.module.seq.Constants.LATIN_1_CHARSET;

import java.security.MessageDigest;
import java.util.Formatter;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import fr.proline.util.StringUtils;

public class HashUtil {

    private static final Logger LOG = LoggerFactory.getLogger(HashUtil.class);

    private static final String SHA_256 = "SHA-256";

    private static final int HASH_LENGTH = 64;

    /**
     * Calculate standard SHA-256 hash from given string (assume Latin 1 encoding).
     * 
     * @param sequence
     *            Protein sequence (only A-Z ASCII/Latin 1 Upper Case).
     * @return SHA-256 hash as Upper Case hex string.
     */
    public static String calculateSHA256(final String sequence) {

	if (StringUtils.isEmpty(sequence)) {
	    throw new IllegalArgumentException("Invalid sequence");
	}

	String result = null;

	try {
	    final MessageDigest md = MessageDigest.getInstance(SHA_256);

	    final byte[] sequenceBytes = sequence.getBytes(LATIN_1_CHARSET);

	    md.update(sequenceBytes);

	    final byte[] digest = md.digest();

	    final StringBuilder buff = new StringBuilder(HASH_LENGTH);
	    final Formatter formatter = new Formatter(buff);

	    for (final byte b : digest) {
		formatter.format("%02X", b);
	    }

	    // Do not close Formatter on StringBuilder

	    result = buff.toString();
	} catch (Exception ex) {
	    LOG.error("Error computing SHA-256 hash", ex);
	}

	return result;
    }

}
