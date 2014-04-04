package fr.proline.module.seq.service;

import org.junit.Ignore;

@Ignore
public class TestRetrieveService {

    private static final long PROJECT_ID = 19L;

    /* Private constructor (Utility class) */
    private TestRetrieveService() {
    }

    public static void main(final String[] args) {
	RetrieveService.retrieveBioSequencesForProject(PROJECT_ID);

	BioSequenceRetriever.waitExecutorShutdown();

	System.out.println("\nMain terminated !");
    }

}
