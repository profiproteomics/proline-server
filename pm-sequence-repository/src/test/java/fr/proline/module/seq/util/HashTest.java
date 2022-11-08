package fr.proline.module.seq.util;

import org.junit.Assert;
import org.junit.Test;

public class HashTest {

	@Test
	public void testHash() {
		doTest("ADDRADLAKYICENQDSISSKLKECCE");
		doCrcTest("ADDRADLAKYICENQDSISSKLKECCE", "8EC531CD3A5506C9");
		doTest("VNFEDNDNRSVLKGGPFSDSYRLFQFHFHWGSTNEHGSEH");
		doCrcTest("VNFEDNDNRSVLKGGPFSDSYRLFQFHFHWGSTNEHGSEH", "07FDB83387CD9FB9");
		doTest("MQIFVKTLTGKTITLEVEPSDTIENVKAKIQDKEGIPPDQQRLIFAGKQLEDGRTLSDYNIQKESTLHLVLRLRGGAKKRKKKSYTTPKKNKHKRKKVKLAVLKYYKVDENGKISRLRRECPSDECGAGVFMASHFDRHYCGKCCLTYCFNKPEDK");
		doCrcTest("MQIFVKTLTGKTITLEVEPSDTIENVKAKIQDKEGIPPDQQRLIFAGKQLEDGRTLSDYNIQKESTLHLVLRLRGGAKKRKKKSYTTPKKNKHKRKKVKLAVLKYYKVDENGKISRLRRECPSDECGAGVFMASHFDRHYCGKCCLTYCFNKPEDK","617BC63DF3A904F7");
	}

	private static void doTest(final String sequence) {
		final String hash = HashUtil.calculateSHA256(sequence);
		Assert.assertNotNull(hash);
		Assert.assertEquals(64, hash.length());
		System.out.println(hash);
	}

	private static void doCrcTest(final String sequence, String expected) {
		final String hash = HashUtil.calculateCRC64(sequence);
		Assert.assertNotNull(hash);
		Assert.assertEquals(expected, hash);
		System.out.println(hash);
	}
}
