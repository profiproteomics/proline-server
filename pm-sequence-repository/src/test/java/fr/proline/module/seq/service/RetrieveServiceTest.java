package fr.proline.module.seq.service;

import fr.proline.module.seq.AbstractDatabaseTest;
import fr.proline.module.seq.DatabaseAccess;
import fr.proline.repository.util.DatabaseUtils;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.List;

import static org.junit.Assert.fail;

/**
 * Unit test
 * 
 * @author CBy
 * 
 */

public final class RetrieveServiceTest extends AbstractDatabaseTest  {

  private static final Logger logger = LoggerFactory.getLogger(RetrieveServiceTest.class);

  private static final long PROJECT_ID = 1L;

	@Before
	public void setUp() {

	}

	@Test
	public void testRetrieveInEmptySeqDb() {
		try {
			super.initDBTestCase();
			ProjectHandler projectHandler = new ProjectHandler(PROJECT_ID);
			List<Long> rsmIds = projectHandler.findAllRSMIds();
			projectHandler.close();
      logger.info("--- Starting retrieveBioSequences Unit Test ---");
			int persistedProteinsCount = RetrieveService.retrieveBioSequences(PROJECT_ID, false, rsmIds);

// Uncomment the following line to generate a sample seq db
//			DatabaseUtils.writeDataSetXML(DatabaseAccess.getSEQDatabaseConnector(false), "seq-db.xml");

			DatabaseAccess.closeSEQDatabaseConnector();

			Assert.assertEquals("Expected peptides proteins count", 45, persistedProteinsCount);

			logger.info("\nMain terminated !");
		} catch (Exception e) {
			e.printStackTrace();
			Assert.fail(e.getMessage());
		}
	}

	@Test
	public void testRetrieveInPartiallyFilledSeqDb() {
		try {
			super.initDBTestCase();
			super.initSeqDBTestCase("/dbunit_samples/SmallRuns_XIC/seq_db/seq-db_5missing.xml");
			ProjectHandler projectHandler = new ProjectHandler(PROJECT_ID);
			List<Long> rsmIds = projectHandler.findAllRSMIds();
			projectHandler.close();
			logger.info("--- Starting retrieveBioSequences Unit Test ---");
			int persistedProteinsCount = RetrieveService.retrieveBioSequences(PROJECT_ID, false, rsmIds);
			DatabaseAccess.closeSEQDatabaseConnector();

			Assert.assertEquals("Expected peptides proteins count", 5, persistedProteinsCount);

			logger.info("\nMain terminated !");
		} catch (Exception e) {
			e.printStackTrace();
			Assert.fail(e.getMessage());
		}
	}

	@Test
	public void testRetrieveOutdatedDbRelease() {
		try {
			super.initDBTestCase();
			super.initSeqDBTestCase("/dbunit_samples/SmallRuns_XIC/seq_db/seq-db_outdated_release.xml");
			ProjectHandler projectHandler = new ProjectHandler(PROJECT_ID);
			List<Long> rsmIds = projectHandler.findAllRSMIds();
			projectHandler.close();
			logger.info("--- Starting retrieveBioSequences Unit Test ---");
			int persistedProteinsCount = RetrieveService.retrieveBioSequences(PROJECT_ID, false, rsmIds);

			DatabaseAccess.closeSEQDatabaseConnector();

			Assert.assertEquals("Expected peptides proteins count", 45, persistedProteinsCount);

			logger.info("\nMain terminated !");
		} catch (Exception e) {
			e.printStackTrace();
			Assert.fail(e.getMessage());
		}
	}


	/**
	 * 2 diffs :
	 *   identifier 45: sequence is different from the one registered in the previous release
	 *   identifier 44: same sequence, but protein description is different
	 */
	@Test
	public void testRetrieveOutdatedDbWith2Diffs() {
		try {
			super.initDBTestCase();
			super.initSeqDBTestCase("/dbunit_samples/SmallRuns_XIC/seq_db/seq-db_outdated_2diffs.xml");
			ProjectHandler projectHandler = new ProjectHandler(PROJECT_ID);
			List<Long> rsmIds = projectHandler.findAllRSMIds();
			projectHandler.close();
			logger.info("--- Starting retrieveBioSequences Unit Test (testRetrieveOutdatedDbWith2Diffs) ---");
			int persistedProteinsCount = RetrieveService.retrieveBioSequences(PROJECT_ID, false, rsmIds);

			DatabaseUtils.writeDataSetXML(DatabaseAccess.getSEQDatabaseConnector(false), "seq-db-OUT-outdated_2diffs.xml");

			DatabaseAccess.closeSEQDatabaseConnector();

			Assert.assertEquals("Expected peptides proteins count", 45, persistedProteinsCount);

			logger.info("\nMain terminated !");
		} catch (Exception e) {
			e.printStackTrace();
			Assert.fail(e.getMessage());
		}
	}


	/**
	 * seq_db contains all the biosequences and protein identifiers but the MSI RS references an older databank version
	 *
	 */
	@Test
	public void testRetrieveOutdatedDbButMissingFasta() {
		try {
			super.initDBTestCase("/dbunit_samples/SmallRuns_XIC/uds-db.xml","/dbunit_samples/SmallRuns_XIC/msi-db_outdated.xml");
			super.initSeqDBTestCase("/dbunit_samples/SmallRuns_XIC/seq_db/seq-db.xml");
			ProjectHandler projectHandler = new ProjectHandler(PROJECT_ID);
			List<Long> rsmIds = projectHandler.findAllRSMIds();
			projectHandler.close();
			logger.info("--- Starting retrieveBioSequences Unit Test ---");
			int persistedProteinsCount = RetrieveService.retrieveBioSequences(PROJECT_ID, false, rsmIds);

//			DatabaseUtils.writeDataSetXML(DatabaseAccess.getSEQDatabaseConnector(false), "seq-db_OUT-outdated_missing_fasta.xml");

			DatabaseAccess.closeSEQDatabaseConnector();

			Assert.assertEquals("Expected peptides proteins count", 0, persistedProteinsCount);

			logger.info("\nMain terminated !");
		} catch (Exception e) {
			e.printStackTrace();
			Assert.fail(e.getMessage());
		}
	}


	/**
	 * seq_db contains all biosequence from an older db than the one referenced by msi
	 * and 1 protein from the fasta file did not contain description
	 *
	 */
	@Test
	public void testRetrieveNoDescriptionInSeqDB() {
		File fasta = new File("./target/test-classes/fasta/fake_66660101.fasta");

		try {
			fasta.renameTo(new File("./target/test-classes/fasta/ups1_ups2_66660101.fasta"));

			super.initDBTestCase("/dbunit_samples/SmallRuns_XIC/uds-db.xml","/dbunit_samples/SmallRuns_XIC/msi-db_newer.xml");
			super.initSeqDBTestCase("/dbunit_samples/SmallRuns_XIC/seq_db/seq-db_nodesc.xml");
			ProjectHandler projectHandler = new ProjectHandler(PROJECT_ID);
			List<Long> rsmIds = projectHandler.findAllRSMIds();
			projectHandler.close();
			logger.info("--- Starting retrieveBioSequences Unit Test ---");
			int persistedProteinsCount = RetrieveService.retrieveBioSequences(PROJECT_ID, false, rsmIds);

			DatabaseUtils.writeDataSetXML(DatabaseAccess.getSEQDatabaseConnector(false), "seq-db_OUT.xml");

			DatabaseAccess.closeSEQDatabaseConnector();

			Assert.assertEquals("Expected peptides proteins count", 45, persistedProteinsCount);

			logger.info("\nMain terminated !");
		} catch (Exception e) {
			e.printStackTrace();
			Assert.fail(e.getMessage());
		} finally {
			fasta = new File("./target/test-classes/fasta/ups1_ups2_66660101.fasta");
			fasta.renameTo(new File("./target/test-classes/fasta/fake_66660101.fasta"));
		}
	}

	/**
	 * seq_db contains all biosequence from another db than the one referenced by msi
	 * 1 protein from the fasta file did not contain description
	 *
	 * WARNING : for this test ups1_ups2_66660101.fasta is mandatory. We then rename fake_6660101 to ups1_ups2_66660101
	 * then rollback to avoid side effect on other tests
	 */
	@Test
	public void testRetrieveNoDescriptionInFasta() {
		File fasta = new File("./target/test-classes/fasta/fake_66660101.fasta");
		try {
			fasta.renameTo(new File("./target/test-classes/fasta/ups1_ups2_66660101.fasta"));

			super.initDBTestCase("/dbunit_samples/SmallRuns_XIC/uds-db.xml","/dbunit_samples/SmallRuns_XIC/msi-db_newer.xml");
			super.initSeqDBTestCase("/dbunit_samples/SmallRuns_XIC/seq_db/seq-db.xml");
			ProjectHandler projectHandler = new ProjectHandler(PROJECT_ID);
			List<Long> rsmIds = projectHandler.findAllRSMIds();
			projectHandler.close();
			logger.info("--- Starting retrieveBioSequences Unit Test ---");
			int persistedProteinsCount = RetrieveService.retrieveBioSequences(PROJECT_ID, false, rsmIds);

			DatabaseUtils.writeDataSetXML(DatabaseAccess.getSEQDatabaseConnector(false), "seq-db_OUT.xml");

			DatabaseAccess.closeSEQDatabaseConnector();

			Assert.assertEquals("Expected peptides proteins count", 45, persistedProteinsCount);

			logger.info("\nMain terminated !");
		} catch (Exception e) {
			e.printStackTrace();
			Assert.fail(e.getMessage());
		} finally {
			fasta = new File("./target/test-classes/fasta/ups1_ups2_66660101.fasta");
			fasta.renameTo(new File("./target/test-classes/fasta/fake_66660101.fasta"));
		}
	}



	/**
	 * seq_db missed 5 biosequences and protein identifiers but the MSI RS references a newer databank version
	 *
	 */
	@Test
	public void testRetrieveNewerDbButMissingFasta5missing() {
		try {
			super.initDBTestCase("/dbunit_samples/SmallRuns_XIC/uds-db.xml","/dbunit_samples/SmallRuns_XIC/msi-db_newer.xml");
			super.initSeqDBTestCase("/dbunit_samples/SmallRuns_XIC/seq_db/seq-db_5missing.xml");
			ProjectHandler projectHandler = new ProjectHandler(PROJECT_ID);
			List<Long> rsmIds = projectHandler.findAllRSMIds();
			projectHandler.close();
			logger.info("--- Starting retrieveBioSequences Unit Test (testRetrieveNewerDbButMissingFasta5missing)");
			int persistedProteinsCount = RetrieveService.retrieveBioSequences(PROJECT_ID, false, rsmIds);

//			DatabaseUtils.writeDataSetXML(DatabaseAccess.getSEQDatabaseConnector(false), "seq-db_OUT-newer_missing_fasta-5missing.xml");

			DatabaseAccess.closeSEQDatabaseConnector();

			Assert.assertEquals("Expected peptides proteins count", 5, persistedProteinsCount);

			logger.info("\nMain terminated !");
		} catch (Exception e) {
			e.printStackTrace();
			Assert.fail(e.getMessage());
		}
	}

	/**
	 * seq_db contains all the biosequences and protein identifiers but the MSI RS references an newer databank version
	 *
	 */
	@Test
	public void testRetrieveNewerDbButMissingFastaEmptySeq() {
		try {
			super.initDBTestCase("/dbunit_samples/SmallRuns_XIC/uds-db.xml","/dbunit_samples/SmallRuns_XIC/msi-db_newer.xml");
			ProjectHandler projectHandler = new ProjectHandler(PROJECT_ID);
			List<Long> rsmIds = projectHandler.findAllRSMIds();
			projectHandler.close();
			logger.info("--- Starting retrieveBioSequences Unit Test ---");
			int persistedProteinsCount = RetrieveService.retrieveBioSequences(PROJECT_ID, false, rsmIds);

//			DatabaseUtils.writeDataSetXML(DatabaseAccess.getSEQDatabaseConnector(false), "seq-db_OUT-newer_missing_fasta_empty_seq.xml");

			DatabaseAccess.closeSEQDatabaseConnector();

			Assert.assertEquals("Expected peptides proteins count", 45, persistedProteinsCount);

			logger.info("\nMain terminated !");
		} catch (Exception e) {
			e.printStackTrace();
			Assert.fail(e.getMessage());
		}
	}

	/**
	 * seq_db contains all the biosequences and protein identifiers but the MSI RS references an older databank version
	 *
	 */
	@Test
	public void testRetrieveOutdatedDbButMissingFastaEmptySeq() {
		try {
			super.initDBTestCase("/dbunit_samples/SmallRuns_XIC/uds-db.xml","/dbunit_samples/SmallRuns_XIC/msi-db_outdated.xml");
			ProjectHandler projectHandler = new ProjectHandler(PROJECT_ID);
			List<Long> rsmIds = projectHandler.findAllRSMIds();
			projectHandler.close();
			logger.info("--- Starting retrieveBioSequences Unit Test ---");
			int persistedProteinsCount = RetrieveService.retrieveBioSequences(PROJECT_ID, false, rsmIds);

//			DatabaseUtils.writeDataSetXML(DatabaseAccess.getSEQDatabaseConnector(false), "seq-db_OUT-outdated_missing_fasta_empty.xml");

			DatabaseAccess.closeSEQDatabaseConnector();

			Assert.assertEquals("Expected peptides proteins count", 45, persistedProteinsCount);

			logger.info("\nMain terminated !");
		} catch (Exception e) {
			e.printStackTrace();
			Assert.fail(e.getMessage());
		}
	}

}