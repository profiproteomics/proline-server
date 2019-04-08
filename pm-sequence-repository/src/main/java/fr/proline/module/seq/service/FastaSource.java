package fr.proline.module.seq.service;

import static fr.proline.module.seq.Constants.LATIN_1_CHARSET;
import static fr.profi.util.StringUtils.LINE_SEPARATOR;

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

import fr.proline.module.seq.dto.DDatabankProtein;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
	private final Pattern m_proteinIdentifierPattern;
	private final Pattern m_repositoryIdentPattern;

	public FastaSource(final File fastaFile, final Pattern proteinIdentifierPattern, final Pattern repositoryIdentPattern) {

		assert ((fastaFile != null) && fastaFile.isFile()) : "Invalid fastaFile";
		assert (proteinIdentifierPattern != null) : "SEDbIdentPattern is null";

		m_fastaFile = fastaFile;
		m_proteinIdentifierPattern = proteinIdentifierPattern;
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

	public Map<DDatabankProtein, String> retrieveSequences(final Map<String, List<DDatabankProtein>> proteinsByIdentifier) throws IOException {
		return parseFile(proteinsByIdentifier);
	}

	/* Private methods */
	private Map<DDatabankProtein, String> parseFile(final Map<String, List<DDatabankProtein>> proteinsByIdentifier)	throws IOException {

		final String fastaAbsolutePathname = m_fastaFile.getAbsolutePath();
		final Map<DDatabankProtein, String> foundSequences = new HashMap<>();

		long lineIndex = 0L;
		BufferedReader reader = null;

		try {
			InputStream is = new FileInputStream(m_fastaFile);
			reader = new BufferedReader(new InputStreamReader(is, LATIN_1_CHARSET));

			final Map<String, List<DDatabankProtein>> remainingProteinIdentifiers = new HashMap<>(proteinsByIdentifier);
			final int remainingProteinIdentifiersCount = remainingProteinIdentifiers.size();

			DDatabankProtein currentProtein = null;
			StringBuilder sequenceBuilder = null;

				LOG.debug("Searching {} distinct Proteins with \"{}\" Regex in [{}] ",
								remainingProteinIdentifiersCount,
								m_proteinIdentifierPattern.pattern(),
								fastaAbsolutePathname);

			final long start = System.currentTimeMillis();
			String rawLine = reader.readLine();

			while (rawLine != null) {
				final String trimmedLine = rawLine.trim();

				if (!trimmedLine.isEmpty()) {

					if (trimmedLine.startsWith(">")) {
						/* Current line is a protein header */
						if (currentProtein != null) {
							// register the previously parsed entry
							addSequence(currentProtein, sequenceBuilder, foundSequences, remainingProteinIdentifiers);
							currentProtein = null;// Reset current readingSeDbIdentifier and sequence
							sequenceBuilder = null;
						}
						if (remainingProteinIdentifiers.isEmpty()) {
							LOG.debug("All identifiers found from [" + fastaAbsolutePathname + ']');
							break;
						} else {
							final DDatabankProtein proteinIdentifier = checkHeader(rawLine, remainingProteinIdentifiers);
							if (proteinIdentifier != null) {
								/* Found a proteinIdentifier */
								currentProtein = proteinIdentifier;
								sequenceBuilder = new StringBuilder();
							}
						}
					} else {
						/* Continue to read AA sequence */
						if (sequenceBuilder != null) {// Reading currentProtein sequence
							sequenceBuilder.append(trimmedLine);
						}
					}
				} // End if (line is not empty)

				lineIndex++;
				rawLine = reader.readLine();
			} // End reading loop

			/* Handle last sequence */
			if (currentProtein != null) {
				addSequence(currentProtein, sequenceBuilder, foundSequences, remainingProteinIdentifiers);
			}

			final long duration = System.currentTimeMillis() - start;

			final String message = String.format(
					"[%s] %d lines parsed in %d ms (%,.1f lines/s) found %d sequences on %d", fastaAbsolutePathname,
					lineIndex, duration, ((double) (lineIndex * 1000)) / duration, foundSequences.size(), remainingProteinIdentifiersCount);
			LOG.info(message);

		} finally {
			if (reader != null) {
				try {
					reader.close();
				} catch (IOException exClose) {
					LOG.error("Error closing [" + fastaAbsolutePathname + ']', exClose);
				}
			}
		}

		return foundSequences;
	}

	private static void addSequence(
			final DDatabankProtein protein,
			final StringBuilder sequenceBuilder, final Map<DDatabankProtein, String> foundSequences,
			final Map<String, List<DDatabankProtein>> remainingProteinIdentifiers) {

		final String identifier = protein.getIdentifier();
		String normalizedSequence = sequenceBuilder.toString().toUpperCase();
		/* Remove white spaces from sequence */
		if (normalizedSequence.contains(" ")) {
			LOG.info("White spaces will be replaced by '' in the Sequence for [{}].",identifier);
			normalizedSequence = normalizedSequence.replaceAll("\\s+", "");
		}
		/* Remove potential '*' char (translation stop marker) */
		final int starIndex = normalizedSequence.indexOf('*');
		if (starIndex != -1) {
			normalizedSequence = normalizedSequence.substring(0, starIndex);
		}

		if (PeptideUtils.checkSequence(normalizedSequence)) {
			foundSequences.put(protein, normalizedSequence);
			remainingProteinIdentifiers.remove(identifier);
		} else {
			LOG.warn("Invalid Sequence for [{}] :\n{}", identifier, normalizedSequence);
		}

	}

	private DDatabankProtein checkHeader(final String header, final Map<String, List<DDatabankProtein>> remainingProteinIdentifiers) {

		DDatabankProtein foundProtein = null;
		String descriptionFromFasta = null;
		final Matcher matcher = m_proteinIdentifierPattern.matcher(header);

		if (matcher.find()) {
			if (matcher.groupCount() < 1) {
				throw new IllegalArgumentException("Invalid DatabankProtein Regex");
			}

			final String identifier = matcher.group(1).trim();// DatabankProtein value should be trimmed

			if ((header != null) && (!header.isEmpty()) && (header.trim().length() > identifier.trim().length())) {
				descriptionFromFasta = header.substring(header.indexOf(identifier) + identifier.length() + 1);
			}

			final List<DDatabankProtein> possibleIdentifiers = remainingProteinIdentifiers.get(identifier);

			if ((possibleIdentifiers != null) && !possibleIdentifiers.isEmpty()) {

				// First iteration, search for an entry with the same description (more precisely: a description contained in
				// the header).
				for (final DDatabankProtein sdi : possibleIdentifiers) {
					final String description = sdi.getDescription();
					if ((description != null) && header.contains(description)) {
						foundProtein = sdi;
						break;
					}
				}

				// If not found, start a second iteration searching for the first one with no description and eventually set it
				// the description found in the header
				if (foundProtein == null) {
					LOG.debug("Cannot find a Protein with a matching description for [{}], trying to search for one with no description", identifier);
					for (final DDatabankProtein sdi : possibleIdentifiers) {
						if (sdi.getDescription() == null) {
							LOG.debug("A Protein with no description is found for [{}]", possibleIdentifiers.size(), identifier);
							if ((descriptionFromFasta != null) && (!descriptionFromFasta.isEmpty())) {
								foundProtein = new DDatabankProtein(identifier, descriptionFromFasta);
							} else {
								//*** VDS Both description are null ...  choose first one with no description
								foundProtein = sdi;
								final int nPossibleIdentifiers = possibleIdentifiers.size();
								if (nPossibleIdentifiers > 1) {
									//TODO Cby: mais les autres possibleIdentifiers ont peut etre une description mais qui ne matche pas ?
									foundProtein.setInferred(true);
									LOG.debug("There are {} Proteins (inferred) for [{}] taking the first one with no description", nPossibleIdentifiers, identifier);
								}
							}
							break;
						} // End if (current SEDbIdentWrapper description is null)
					} // End second loop for each possibleIdentifiers
				} // End if (foundProtein is null after first loop)

				//  *** VDS MSI description not null && still not found MSI description not in Fasta Description Header and
				//
				if (foundProtein == null) {
					/* Build Warning LOG message */
					final StringBuilder messageBuilder = new StringBuilder(MESSAGE_BUILDER_SIZE);
					messageBuilder.append("No valid description match for [").append(identifier);
					messageBuilder.append("] taking first protein (inferred)");
					messageBuilder.append(LINE_SEPARATOR);

					messageBuilder.append("Parsed FASTA Header, then expected protein descriptions :");
					messageBuilder.append(LINE_SEPARATOR);

					messageBuilder.append(header);
					messageBuilder.append(LINE_SEPARATOR);

					for (final DDatabankProtein sdi : possibleIdentifiers) {
						final String description = sdi.getDescription();

						if (description == null) {
							messageBuilder.append("NULL");
						} else {
							messageBuilder.append('[').append(description).append(']');
						}

						messageBuilder.append(LINE_SEPARATOR);
					}

					LOG.debug(messageBuilder.toString());

					/* Retrieve arbitrar first SEDbIdentWrapper */
					//***  VDS Warning : Use MSI protein with description from MSI instead of FASTA to set in SeqDB !
					foundProtein = possibleIdentifiers.get(0);
					foundProtein.setInferred(true);
					LOG.warn("Arbitrarily select the first ProteinIdentifier as the one matching to the identifier parsed in the fasta file ??");
				}

				parseRepositoryIdent(header, foundProtein);
			} // End if (possibleIdentifiers is not empty)

		} // End if (m_proteinIdentifierPattern is found)

		return foundProtein;
	}

	protected void parseRepositoryIdent(final String header, final DDatabankProtein seDbIdentifier) {

		if (m_repositoryIdentPattern != null) {

			final Matcher matcher = m_repositoryIdentPattern.matcher(header);

			if (matcher.find()) {

				if (matcher.groupCount() < 1) {
					throw new IllegalArgumentException("Invalid RepositoryProtein Regex");
				}

				final String repositoryIdent = matcher.group(1).trim();
				if (!repositoryIdent.isEmpty()) {
					seDbIdentifier.setRepositoryIdentifier(repositoryIdent);
				}
			}
		}
	}

}
