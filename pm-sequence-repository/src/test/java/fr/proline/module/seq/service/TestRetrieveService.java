package fr.proline.module.seq.service;

import org.junit.Ignore;

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
	private TestRetrieveService() {
	}

	public static void main(final String[] args) {
		try {
			RetrieveService.retrieveBioSequencesForProject(PROJECT_ID, false);
			BioSequenceRetriever.waitExecutorShutdown();
			System.out.println("\nMain terminated !");
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
}
