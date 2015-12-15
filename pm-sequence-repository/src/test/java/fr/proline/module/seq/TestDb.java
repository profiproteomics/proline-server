package fr.proline.module.seq;

import org.junit.Ignore;

import fr.proline.repository.IDatabaseConnector;

/**
 * Manual test : requires PostgreSQL connection.
 * 
 * @author LMN
 * 
 */
@Ignore
public final class TestDb {

	private TestDb() {
	}

	public static void main(final String[] args) {
		final IDatabaseConnector seqConnector = DatabaseAccess.getSEQDatabaseConnector(true);

		if (seqConnector == null) {
			System.out.println("SEQ DB connector");
		} else {
			System.out.println("SEQ DB connector : " + seqConnector);
		}

	}

}
