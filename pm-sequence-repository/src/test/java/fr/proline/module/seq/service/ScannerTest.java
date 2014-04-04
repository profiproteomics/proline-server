package fr.proline.module.seq.service;

import static org.junit.Assert.*;

import java.io.File;
import java.util.List;
import java.util.Map;

import org.junit.Test;

import fr.proline.module.seq.service.FastaPathsScanner;

public class ScannerTest {

    private static final String FASTA_FILE_PATH = ".";

    @Test
    public void testScanPaths() {
	final Map<String, List<File>> foundFastaFiles = FastaPathsScanner.scanPaths(new FastaPathsScanner(),
		new String[] { FASTA_FILE_PATH, });

	assertNotNull("FoundFastaFiles Map", foundFastaFiles);
    }

}
