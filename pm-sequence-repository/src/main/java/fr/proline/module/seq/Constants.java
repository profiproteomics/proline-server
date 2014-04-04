package fr.proline.module.seq;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class Constants {

    public static final String LATIN_1_CHARSET = "ISO-8859-1";

    private static final Logger LOG = LoggerFactory.getLogger(Constants.class);

    /* Private constructor (Utility class) */
    private Constants() {
    }

    public static int calculateNThreads() {
	final int nThreads = Runtime.getRuntime().availableProcessors() / 2 + 1;
	LOG.debug("Using {} thread(s)", nThreads);

	return nThreads;
    }

}
