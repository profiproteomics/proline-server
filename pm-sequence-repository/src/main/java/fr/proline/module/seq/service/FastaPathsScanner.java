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
	public static Map<String, List<File>> scanPaths(final FastaPathsScanner scanner, final String[] paths) {

		if (scanner == null) {
			throw new IllegalArgumentException("Scanner is null");
		}

		if ((paths == null) || (paths.length == 0)) {
			throw new IllegalArgumentException("Invalid paths array");
		}

		/* Check each given path */
		final List<File> filePaths = new ArrayList<>(paths.length);

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

					if (LOG.isDebugEnabled()) {
						LOG.debug("Scanning [{}]", absolutePathname);
					}

					scanner.scan(filePath, traversedDirs, foundFastaFiles);

					LOG.info("[{}] scan terminated", absolutePathname);
				}

			};

			final Future<?> future = executor.submit(task);
			futures.add(future);
		}

		/* Wait (blocking) for all futures to complete */
		for (final Future<?> f : futures) {

			try {
				f.get();// Return null
			} catch (Exception ex) {
				LOG.error("Error trying to get Future result", ex);
			}

		}

		try {
			executor.shutdown();

			if (executor.awaitTermination(Long.MAX_VALUE, TimeUnit.SECONDS)) {

				if (LOG.isDebugEnabled()) {
					LOG.debug("Number of traversed dirs: {}", traversedDirs.size());
				}

				synchronized (foundFastaFiles) {
					result = foundFastaFiles;
				} // End of synchronized block on foundFastaFiles

				LOG.info("Found FASTA file names: {}", result.size());
			} else {
				LOG.error("FastaFilesScanner Executor timed out");
			}

		} catch (Exception ex) {
			LOG.error("Error ending FastaFilesScanner Executor", ex);
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
	 * @param skippedDirs
	 *            List of regex <code>Pattern</code> matching directory names to skip while processing paths.
	 * @param traversedDirs
	 *            Map of already traversed directories to avoid loop in presence of symbolic links. Must not be <code>null</code>.
	 * @param data
	 *            Internal data : can be used by client <code>handleFile</code> method implementation. Note : client method has responsibility to handle locking
	 *            and synchronization on <code>data</code> object.
	 */
	private void scan(
		final File file,
		final ConcurrentMap<File, Boolean> traversedDirs,
		final Map<String, List<File>> foundFastaFiles) {
		assert (file != null) : "scan() file is null";
		assert (traversedDirs != null) : "scan() traversedDirs Map is null";

		final String absolutePathname = file.getAbsolutePath();

		if (file.isFile()) {
			handleFile(file, foundFastaFiles);
		} else if (file.isDirectory()) {
			boolean alreadyTraversed = true;// Don't want to traverse a non canonisable pathname

			try {
				final File canonicalPathname = file.getCanonicalFile();

				alreadyTraversed = (traversedDirs.put(canonicalPathname, Boolean.TRUE) == Boolean.TRUE);
			} catch (IOException ioEx) {
				LOG.error("Error retrieving [" + absolutePathname + "] canonical pathname", ioEx);
			}

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

				} // End if (files array is not null)

			} // End if (directory not already traversed)

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
			} // End of synchronized block on foundFastaFiles

		}

	}

}
