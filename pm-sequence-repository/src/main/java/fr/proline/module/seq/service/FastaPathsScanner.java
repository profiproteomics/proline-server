package fr.proline.module.seq.service;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import fr.proline.module.seq.Constants;
import fr.profi.util.StringUtils;
import fr.profi.util.ThreadLogger;

public class FastaPathsScanner {

	private static final Logger LOG = LoggerFactory.getLogger(FastaPathsScanner.class);

	private static final String FASTA_SUFFIX = "FASTA";// Normalized to Upper Case

	/**
	 * <em>Scan</em> entry-point method.
	 * 
	 * @param paths
	 *            List of abstract paths to scan. Must not be <code>null</code> or empty and must contains valid path names (existing regular files or
	 *            directories). If an abstract path is a directory it will be scanned recursively.
	 */
	public static Map<String, List<File>> scanPaths(final FastaPathsScanner scanner, final List<String> paths) throws Exception {

		assert (scanner != null) : "Scanner cannot be null to scan paths";
		assert ((paths != null) && (!paths.isEmpty())) : "Invalid paths array";

		/* Check each given path */
		final List<File> filePaths = new ArrayList<>(paths.size());

		for (final String pathname : paths) {

			if (pathname == null) {
				throw new IllegalArgumentException("Path is null");
			}

			final File filePath = new File(pathname);

			if (filePath.exists()) {
				filePaths.add(filePath);
			} else {
				LOG.warn("Non existant path [{}]", pathname);
			}

		}

		final int nPaths = filePaths.size();

		if (nPaths == 0) {
			throw new IllegalArgumentException("No valid paths");
		}

		Map<String, List<File>> result = null;

		final ExecutorService executor = Executors.newFixedThreadPool(Constants.calculateNThreads());
		final ConcurrentMap<File, Boolean> traversedDirs = new ConcurrentHashMap<File, Boolean>();

		/* @GuardedBy("itself") */
		final Map<String, List<File>> foundFastaFiles = new HashMap<String, List<File>>();
		final List<Future<?>> futures = new ArrayList<>(nPaths);

		for (final File filePath : filePaths) {

			final Runnable task = new Runnable() {

				public void run() {
					final Thread currentThread = Thread.currentThread();

					if (!(currentThread.getUncaughtExceptionHandler() instanceof ThreadLogger)) {
						currentThread.setUncaughtExceptionHandler(new ThreadLogger(LOG));
					}

					final String absolutePathname = filePath.getAbsolutePath();
					LOG.debug("Scanning [{}]", absolutePathname);

					try {
						scanner.scan(filePath, traversedDirs, foundFastaFiles);
					} catch (IOException e) {
						throw new RuntimeException(e);
					}

					LOG.debug("[{}] scan terminated", absolutePathname);
				}

			};

			final Future<?> future = executor.submit(task);
			futures.add(future);
		}

		/* Wait (blocking) for all futures to complete */
		for (final Future<?> f : futures) {
			f.get();// Return null
		}

		executor.shutdown();

		if (executor.awaitTermination(Long.MAX_VALUE, TimeUnit.SECONDS)) {

			if (LOG.isDebugEnabled()) {
				LOG.debug("Number of traversed dirs: {}", traversedDirs.size());
			}

			synchronized (foundFastaFiles) {
				result = foundFastaFiles;
			} // End of synchronized block on foundFastaFiles

			LOG.info("{} Fasta fileName(s) found in search paths", result.size());
		} else {
			LOG.error("FastaFilesScanner Executor timed out");
		}

		return result;
	}

	/**
	 * Default implementation checks if fileName ends with "FASTA" ignoring case.
	 * 
	 * @param fileName
	 * @return <code>true</code> if it is the name of a possible FASTA file.
	 */
	protected boolean isFastaFile(final String fileName) {

		if (StringUtils.isEmpty(fileName)) {
			throw new IllegalArgumentException("Invalid fileName");
		}

		return fileName.toUpperCase().endsWith(FASTA_SUFFIX);
	}

	/**
	 * <em>Scan</em> method called recursively.
	 * 
	 * @param file
	 *            Abstract file to scan : if it denotes a directory, elements are scanned recursively. Must not be <code>null</code>.
	 * @param traversedDirs
	 *            Map of already traversed directories to avoid loop in presence of symbolic links. Must not be <code>null</code>.
	 * @throws IOException 
	 * 
	 */
	private void scan(
					final File file,
					final ConcurrentMap<File, Boolean> traversedDirs,
					final Map<String, List<File>> foundFastaFiles) throws IOException {

		assert (file != null) : "scan() file is null";
		assert (traversedDirs != null) : "scan() traversedDirs Map is null";

		final String absolutePathname = file.getAbsolutePath();

		if (file.isFile()) {
			handleFile(file, foundFastaFiles);
		} else if (file.isDirectory()) {

			boolean alreadyTraversed = true;// Don't want to traverse a non canonisable pathname
			final File canonicalPathname = file.getCanonicalFile();
			alreadyTraversed = (traversedDirs.put(canonicalPathname, Boolean.TRUE) == Boolean.TRUE);

			if (alreadyTraversed) {// Do not recurse in UNIX symbolic links
				LOG.info("Already traversed dir [{}]", absolutePathname);
			} else {
				final File[] files = file.listFiles();

				if (files == null) {
					LOG.warn("Cannot list [{}]", absolutePathname);
				} else {
					for (final File f : files) {
						scan(f, traversedDirs, foundFastaFiles);
					}
				}
			}
		} else {
			LOG.warn("Unknown path type [{}]", absolutePathname);
		}
	}

	private void handleFile(final File file, final Map<String, List<File>> foundFastaFiles) {

		assert ((file != null) && file.isFile()) : "handleFile() invalid file";
		assert (foundFastaFiles != null) : "handleFile() foundFastaFiles Map is null";

		final String fileName = file.getName();

		if (isFastaFile(fileName)) {

			synchronized (foundFastaFiles) {
				List<File> files = foundFastaFiles.get(fileName);

				if (files == null) {
					files = new ArrayList<>(1);// Assume 1 FASTA file by fileName

					foundFastaFiles.put(fileName, files);
				} else if (!files.isEmpty()) {
					LOG.warn("There are multiple files with name [{}]", fileName);
				}

				files.add(file);
			}
		}
	}
}
