package fr.proline.module.seq.service;

import org.apache.commons.logging.Log;
import org.junit.Ignore;
import org.junit.Test;

/**
 * Manual test : requires PostgreSQL connection.
 * 
 * @author LMN
 * 
 */
@Ignore
public final class TestRetrieveService {
	
    private static final long PROJECT_ID = 16L;

    /* Private constructor (Utility class) */
    private TestRetrieveService() {}
    public static void main(final String[] args) {
	RetrieveService.retrieveBioSequencesForProject(PROJECT_ID);
	BioSequenceRetriever.waitExecutorShutdown();
	System.out.println("\nMain terminated !");
    }

}
