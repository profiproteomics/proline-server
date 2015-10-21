package fr.proline.module.seq.service;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import fr.proline.module.seq.ServiceConfiguration;
import fr.profi.util.StringUtils;

public final class UniProtHelper {

	public static final String UNIPROT_NAME = "UniProt";

	public static final String UNIPROT_REPOSITORY_URL = "http://www.uniprot.org/uniprot/";

	public static final String ENTRY_NAME_REGEX = ">\\w{2}\\|[^\\|]*\\|(\\S+)";

	private static final Logger LOG = LoggerFactory.getLogger(UniProtHelper.class);

	/* Private constructor (Utility class) */
	private UniProtHelper() {
	}

	public static boolean isUniProtFastaFileName(final String fastaFileName) {

		if (StringUtils.isEmpty(fastaFileName)) {
			throw new IllegalArgumentException("Invalid fastaFileName");
		}

		boolean result = false;

		if (isStandardUniProtName(fastaFileName)) {
			LOG.debug("[{}] has standard UniProt name", fastaFileName);

			result = true;
		} else {
			final String[] uniprotSEDbNames = ServiceConfiguration.getUniProtSEDbNames();

			if ((uniprotSEDbNames != null) && (uniprotSEDbNames.length > 0)) {
				for (final String regex : uniprotSEDbNames) {

					if (find(fastaFileName, regex)) {
						LOG.debug("[{}] matches UniProt Regex \"{}\"", fastaFileName, regex);
						result = true;

						break;
					}

				} // End loop for each regex
			}

		}

		return result;
	}

	private static boolean isStandardUniProtName(final String fastaFileName) {
		assert(!StringUtils.isEmpty(fastaFileName)) : "isStandardUniProtName() invalid fastaFileName";

		final String normalizedName = fastaFileName.toUpperCase();

		return ((normalizedName.contains("UNI") && normalizedName.contains("PROT"))
				|| normalizedName.contains("SPROT") || normalizedName.contains("SWISS") || normalizedName
						.contains("TREMBL"));
	}

	private static boolean find(final String fastaFileName, final String regex) {
		assert(!StringUtils.isEmpty(fastaFileName)) : "find() invalid fastaFileName";
		assert(regex != null) : "find() regex is null";

		boolean result = false;

		try {
			final Pattern pattern = Pattern.compile(regex, Pattern.CASE_INSENSITIVE);
			final Matcher matcher = pattern.matcher(fastaFileName);

			result = matcher.find();
		} catch (Exception ex) {
			LOG.error("Error finding with \"" + regex + "\" Regex", ex);
		}

		return result;
	}

}
