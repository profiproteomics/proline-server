package fr.proline.module.seq.service;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import fr.profi.util.StringUtils;
import fr.proline.module.seq.config.SeqRepoConfig;

/**
 * Builds DataSource from <em>sourcePath</em>.
 * 
 * @author LMN
 * 
 */
public class DataSourceBuilder {

	private static final Logger LOG = LoggerFactory.getLogger(DataSourceBuilder.class);

	private final Object m_foundFastaFilesLock = new Object();

	/* @GuardedBy("m_foundFastaFilesLock") */
	private Map<String, List<File>> m_foundFastaFiles;

	public void forceRescanFastaFiles() {

		synchronized (m_foundFastaFilesLock) {
			m_foundFastaFiles = null;// Invalidate all cached m_foundFastaFiles
		} // End of synchronized block on m_foundFastaFilesLock

	}

	public DataSource buildFastaSource(final String sourceFileName, final Pattern proteinIdentifierPattern, final Pattern repositoryIdentifierPattern) throws Exception {

		assert !StringUtils.isEmpty(sourceFileName) : "Invalid sourceFileName";
		assert (proteinIdentifierPattern != null) : "SeDbIdentPattern is null";

		DataSource fastaSource = null;
		final Map<String, List<File>> fastaFiles = getFastaFiles();

		if ((fastaFiles != null) && !fastaFiles.isEmpty()) {
			final List<File> files = fastaFiles.get(sourceFileName);

			if ((files == null) || files.isEmpty()) {
				LOG.warn("Cannot find [{}] in given fastaFilePaths", sourceFileName);
			} else {
				final File foundFastaFile = retrieveLatest(files);
				fastaSource = new FastaSource(foundFastaFile, proteinIdentifierPattern, repositoryIdentifierPattern);
			}
		}

		return fastaSource;
	}

	public List<File> locateFastaFile(final String namePart) throws Exception {

		assert !StringUtils.isEmpty(namePart) : "Invalid namePart";

		List<File> result = new ArrayList<>();

		final Map<String, List<File>> fastaFiles = getFastaFiles();
		if ((fastaFiles != null) && !fastaFiles.isEmpty()) {
			final Set<Map.Entry<String, List<File>>> entries = fastaFiles.entrySet();

			for (final Map.Entry<String, List<File>> entry : entries) {
				final String fastaFileName = entry.getKey();

				if (fastaFileName.contains(namePart)) {
					result.addAll(entry.getValue());
				}
			}
		}

		return result;
	}

	protected Map<String, List<File>> getFastaFiles() throws Exception {

		Map<String, List<File>> fastaFiles = null;

		synchronized (m_foundFastaFilesLock) {

			if (m_foundFastaFiles == null) {
				final List<String> localFASTAPaths = SeqRepoConfig.getInstance().getFastaDirectories();

				if ((localFASTAPaths != null) && (!localFASTAPaths.isEmpty())) {
					fastaFiles = FastaPathsScanner.scanPaths(new FastaPathsScanner(), localFASTAPaths);

					m_foundFastaFiles = fastaFiles;// Cache scanned files Map
				} else {
					LOG.error("No valid localFASTAPaths configured");
				}
			} else {
				fastaFiles = m_foundFastaFiles;
			}
		}

		return fastaFiles;
	}

	private static File retrieveLatest(final List<File> files) {
		assert (files != null) : "retrieveLatest() files List is null";

		File latest = null;

		for (final File f : files) {

			if (latest == null) {
				latest = f;
			} else {

				if (f.lastModified() > latest.lastModified()) {
					LOG.info("Use latest version of [{}]", f.getAbsolutePath());
					latest = f;
				}
			}
		}

		return latest;
	}

}
