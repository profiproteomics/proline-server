package fr.proline.module.seq.service;

import static fr.proline.module.seq.Constants.LATIN_1_CHARSET;
import static fr.proline.util.StringUtils.LINE_SEPARATOR;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import fr.proline.module.seq.dto.SEDbIdentifierWrapper;
import fr.proline.module.seq.util.PeptideUtils;

/**
 * DataSource to parse BioSequences from FASTA files.
 * 
 * @author LMN
 * 
 */
public class FastaSource implements DataSource {

    private static final Logger LOG = LoggerFactory.getLogger(FastaSource.class);

    private static final int MESSAGE_BUILDER_SIZE = 1024;

    private final File m_fastaFile;

    private final Date m_sourceCreationTime = new Date();

    private final Pattern m_seDbIdentPattern;

    private final Pattern m_repositoryIdentPattern;

    public FastaSource(final File fastaFile, final Pattern seDbIdentPattern,
	    final Pattern repositoryIdentPattern) {

	if ((fastaFile == null) || !fastaFile.isFile()) {
	    throw new IllegalArgumentException("Invalid fastaFile");
	}

	m_fastaFile = fastaFile;

	if (seDbIdentPattern == null) {
	    throw new IllegalArgumentException("SEDbIdentPattern is null");
	}

	m_seDbIdentPattern = seDbIdentPattern;

	m_repositoryIdentPattern = repositoryIdentPattern;
    }

    public Date getLastModifiedTime() {
	Date result = null;

	final long lastModified = m_fastaFile.lastModified();

	if (lastModified == 0L) {
	    result = (Date) m_sourceCreationTime.clone();
	} else {
	    result = new Date(lastModified);
	}

	return result;
    }

    public Map<SEDbIdentifierWrapper, String> retrieveSequences(
	    final Map<String, List<SEDbIdentifierWrapper>> identByValues) {
	return parseFile(identByValues);
    }

    /* Private methods */
    private Map<SEDbIdentifierWrapper, String> parseFile(
	    final Map<String, List<SEDbIdentifierWrapper>> identByValues) {
	final String fastaAbsolutePathname = m_fastaFile.getAbsolutePath();

	final Map<SEDbIdentifierWrapper, String> foundSequences = new HashMap<>();

	long lineIndex = 0L;

	BufferedReader br = null;

	try {
	    InputStream is = new FileInputStream(m_fastaFile);
	    br = new BufferedReader(new InputStreamReader(is, LATIN_1_CHARSET));

	    final Map<String, List<SEDbIdentifierWrapper>> remainingSEDbIdentifiers = new HashMap<>(
		    identByValues);
	    final int nIdentValues = remainingSEDbIdentifiers.size();

	    SEDbIdentifierWrapper readingSEDbIdentifier = null;
	    StringBuilder sequenceBuilder = null;

	    if (LOG.isDebugEnabled()) {
		LOG.debug(
			"Reading [{}] Searching {} distinct SEDbIdentifier.values with \"{}\" SEDbIdent and {} RepositoryIdent Regex",
			fastaAbsolutePathname, nIdentValues, m_seDbIdentPattern.pattern(),
			(m_repositoryIdentPattern == null) ? "NO"
				: '\"' + m_repositoryIdentPattern.pattern() + '\"');
	    }

	    final long start = System.currentTimeMillis();

	    String rawLine = br.readLine();

	    while (rawLine != null) {
		final String trimmedLine = rawLine.trim();

		if (!trimmedLine.isEmpty()) {

		    if (trimmedLine.startsWith(">")) {
			/* Fasta header */

			if (readingSEDbIdentifier != null) {
			    addSequence(readingSEDbIdentifier, sequenceBuilder, foundSequences,
				    remainingSEDbIdentifiers);

			    readingSEDbIdentifier = null; // Reset current readingSeDbIdentifier and sequence
			    sequenceBuilder = null;
			}

			if (remainingSEDbIdentifiers.isEmpty()) {
			    LOG.debug("All identifiers found from [" + fastaAbsolutePathname + ']');

			    break;
			} else {

			    final SEDbIdentifierWrapper seDbIdentifier = checkHeader(rawLine,
				    remainingSEDbIdentifiers);
			    if (seDbIdentifier != null) {
				/* Found a seDbIdentifier */
				readingSEDbIdentifier = seDbIdentifier;
				sequenceBuilder = new StringBuilder();
			    }

			}
		    } else {
			/* Continuing sequence */

			if (sequenceBuilder != null) { // Reading readingSEDbIdentifier sequence
			    sequenceBuilder.append(trimmedLine);
			}

		    }

		} // End if (line is not empty)

		++lineIndex;

		rawLine = br.readLine();
	    } // End reading loop

	    /* Handle last sequence */
	    if (readingSEDbIdentifier != null) {
		addSequence(readingSEDbIdentifier, sequenceBuilder, foundSequences, remainingSEDbIdentifiers);
	    }

	    final long end = System.currentTimeMillis();

	    final long duration = end - start;

	    final String message = String.format(
		    "[%s] %d lines parsed in %d ms (%,.1f lines/s) found %d sequences on %d",
		    fastaAbsolutePathname, lineIndex, duration, ((double) (lineIndex * 1000)) / duration,
		    foundSequences.size(), nIdentValues);
	    LOG.info(message);
	} catch (Exception ex) {
	    LOG.error(String.format("Error reading [%s] current line index: %d", fastaAbsolutePathname,
		    lineIndex), ex);
	} finally {

	    if (br != null) {
		try {
		    br.close();
		} catch (IOException exClose) {
		    LOG.error("Error closing [" + fastaAbsolutePathname + ']', exClose);
		}
	    }

	}

	return foundSequences;
    }

    private static void addSequence(final SEDbIdentifierWrapper readingSEDbIdentifier,
	    final StringBuilder sequenceBuilder, final Map<SEDbIdentifierWrapper, String> foundSequences,
	    final Map<String, List<SEDbIdentifierWrapper>> remainingSEDbIdentifiers) {
	final String identValue = readingSEDbIdentifier.getValue();

	String normalizedSequence = sequenceBuilder.toString().toUpperCase();

	/* Remove potential '*' char (translation stop marker) */
	final int starIndex = normalizedSequence.indexOf('*');
	if (starIndex != -1) {
	    normalizedSequence = normalizedSequence.substring(0, starIndex);
	}

	if (PeptideUtils.checkSequence(normalizedSequence)) {
	    foundSequences.put(readingSEDbIdentifier, normalizedSequence);

	    remainingSEDbIdentifiers.remove(identValue);
	} else {
	    LOG.warn("Invalid Sequence for [{}] :\n{}", identValue, normalizedSequence);
	}

    }

    private SEDbIdentifierWrapper checkHeader(final String header,
	    final Map<String, List<SEDbIdentifierWrapper>> remainingSEDbIdentifiers) {
	SEDbIdentifierWrapper foundSEDbIdent = null;

	final Matcher matcher = m_seDbIdentPattern.matcher(header);

	if (matcher.find()) {

	    if (matcher.groupCount() < 1) {
		throw new IllegalArgumentException("Invalid SEDbIdentifier Regex");
	    }

	    final String identValue = matcher.group(1).trim(); // SEDbIdentifier value should be trimmed

	    final List<SEDbIdentifierWrapper> possibleIdentifiers = remainingSEDbIdentifiers.get(identValue);
	    if ((possibleIdentifiers != null) && !possibleIdentifiers.isEmpty()) {

		for (final SEDbIdentifierWrapper sdi : possibleIdentifiers) {
		    final String description = sdi.getDescription();

		    if ((description != null) && header.contains(description)) {
			/* Check exact description match */
			foundSEDbIdent = sdi;

			break;
		    }

		} // End first loop for each possibleIdentifiers

		if (foundSEDbIdent == null) {

		    for (final SEDbIdentifierWrapper sdi : possibleIdentifiers) {

			if (sdi.getDescription() == null) {
			    /* Retrieve first SEDbIdentWrapper without description */
			    foundSEDbIdent = sdi;

			    final int nPossibleIdentifiers = possibleIdentifiers.size();
			    if (nPossibleIdentifiers > 1) {
				foundSEDbIdent.setInferred(true);
				LOG.warn(
					"There are {} SEDbIdentWrapper (inferred) for [{}] taking first with no description",
					nPossibleIdentifiers, identValue);
			    }

			    break;
			} // End if (current SEDbIdentWrapper description is null)

		    } // End second loop for each possibleIdentifiers

		} // End if (foundSEDbIdent is null after first loop)

		if (foundSEDbIdent == null) {
		    /* Build Warning LOG message */
		    final StringBuilder messageBuilder = new StringBuilder(MESSAGE_BUILDER_SIZE);
		    messageBuilder.append("No valid description match for [").append(identValue);
		    messageBuilder.append("] taking first SEDbIdentWrapper (inferred)");
		    messageBuilder.append(LINE_SEPARATOR);

		    messageBuilder
			    .append("Parsed FASTA Header, then expected SEDbIdentWrapper descriptions :");
		    messageBuilder.append(LINE_SEPARATOR);

		    messageBuilder.append(header);
		    messageBuilder.append(LINE_SEPARATOR);

		    for (final SEDbIdentifierWrapper sdi : possibleIdentifiers) {
			final String description = sdi.getDescription();

			if (description == null) {
			    messageBuilder.append("NULL");
			} else {
			    messageBuilder.append('[').append(description).append(']');
			}

			messageBuilder.append(LINE_SEPARATOR);
		    }

		    LOG.warn(messageBuilder.toString());

		    /* Retrieve arbitrar first SEDbIdentWrapper */
		    foundSEDbIdent = possibleIdentifiers.get(0);
		    foundSEDbIdent.setInferred(true);
		}

		parseRepositoryIdent(header, foundSEDbIdent);
	    } // End if (possibleIdentifiers is not empty)

	} // End if (m_seDbIdentPattern is found)

	return foundSEDbIdent;
    }

    protected void parseRepositoryIdent(final String header, final SEDbIdentifierWrapper seDbIdentifier) {

	if (m_repositoryIdentPattern != null) {

	    final Matcher matcher = m_repositoryIdentPattern.matcher(header);

	    if (matcher.find()) {

		if (matcher.groupCount() < 1) {
		    throw new IllegalArgumentException("Invalid RepositoryIdentifier Regex");
		}

		final String repositoryIdent = matcher.group(1).trim();
		if (!repositoryIdent.isEmpty()) {
		    seDbIdentifier.setRepositoryIdentifier(repositoryIdent);
		}

	    }

	}

    }

}
