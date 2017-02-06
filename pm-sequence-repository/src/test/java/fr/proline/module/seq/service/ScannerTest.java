package fr.proline.module.seq.service;

import static org.junit.Assert.*;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.junit.Test;

import fr.proline.module.seq.service.FastaPathsScanner;

public class ScannerTest {

	private static final String FASTA_FILE_PATH = ".";

	@Test
	public void testScanPaths() {
		List<String> fastaPath = new ArrayList<>();
		fastaPath.add(FASTA_FILE_PATH);
		Map<String, List<File>> foundFastaFiles;
		try {
			foundFastaFiles = FastaPathsScanner.scanPaths(new FastaPathsScanner(),
				fastaPath);
			assertNotNull("FoundFastaFiles Map", foundFastaFiles);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
}
