package fr.proline.module.seq;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import fr.proline.util.PropertiesUtils;
import fr.proline.util.StringUtils;

public final class ServiceConfiguration {

    private static final Logger LOG = LoggerFactory.getLogger(ServiceConfiguration.class);

    private static final String PROPERTIES_FILE_NAME = "retrieve-service.properties";

    private static final String MODULE_PREFIX = "fr.proline.module.seq";

    private static final String UDS_DB_CONFIGURATION_FILE_KEY = "udsDbConfigurationFile";

    private static final String LOCAL_FASTA_PATHS_KEY = "localFASTAPaths";

    private static final String DEFAULT_RELEASE_REGEX_KEY = "defaultReleaseRegex";

    private static final String UNIPROT_SE_DB_NAMES_KEY = "uniProtSEDbNames";

    private static final String UNIPROT_SE_DB_IDENT_REGEX_KEY = "uniProtSEDbIdentifierRegex";

    private static final String NO_PROPERTIES_MESSAGE = "No valid [" + PROPERTIES_FILE_NAME + "] found";

    private static final String NO_PROPERTY_MESSAGE = "Property \"{}\" NOT found";

    private static final String PARTS_SEPARATOR = ";";

    private static final Object CONFIGURATION_LOCK = new Object();

    /* @GuardedBy("CONFIGURATION_LOCK") */
    private static Properties properties;

    /* Private constructor (Utility class) */
    private ServiceConfiguration() {
    }

    public static void forcePropertiesFileReload() {

	synchronized (CONFIGURATION_LOCK) {
	    properties = null;
	} // End of synchronized block on CONFIGURATION_LOCK

    }

    public static String getUDSDbConfigurationFileName() {
	String result = null;

	final Properties props = getProperties();

	if (props == null) {
	    LOG.error(NO_PROPERTIES_MESSAGE);
	} else {
	    final String key = MODULE_PREFIX + '.' + UDS_DB_CONFIGURATION_FILE_KEY;

	    result = props.getProperty(key);

	    if (StringUtils.isEmpty(result)) {
		LOG.error(NO_PROPERTY_MESSAGE, key);
	    }

	}

	return result;
    }

    public static String[] getLocalFASTAPaths() {
	String[] paths = null;

	final Properties props = getProperties();

	if (props == null) {
	    LOG.error(NO_PROPERTIES_MESSAGE);
	} else {
	    final String key = MODULE_PREFIX + '.' + LOCAL_FASTA_PATHS_KEY;

	    final String rawPaths = props.getProperty(key);

	    if (rawPaths == null) {
		LOG.error(NO_PROPERTY_MESSAGE, key);
	    } else {
		paths = split(rawPaths);
	    }

	}

	return paths;
    }

    public static String getDefaultReleaseRegex() {
	String result = null;

	final Properties props = getProperties();

	if (props == null) {
	    LOG.error(NO_PROPERTIES_MESSAGE);
	} else {
	    final String key = MODULE_PREFIX + '.' + DEFAULT_RELEASE_REGEX_KEY;

	    result = props.getProperty(key);

	    if (result == null) {
		LOG.error(NO_PROPERTY_MESSAGE, key);
	    }

	}

	return result;
    }

    public static String[] getUniProtSEDbNames() {
	String[] names = null;

	final Properties props = getProperties();

	if (props == null) {
	    LOG.error(NO_PROPERTIES_MESSAGE);
	} else {
	    final String key = MODULE_PREFIX + '.' + UNIPROT_SE_DB_NAMES_KEY;

	    final String rawNames = props.getProperty(key);

	    if (rawNames == null) {
		LOG.info(NO_PROPERTY_MESSAGE, key);
	    } else {
		names = split(rawNames);
	    }

	}

	return names;
    }

    public static String getUniProtSEDbIdentRegex() {
	String result = null;

	final Properties props = getProperties();

	if (props == null) {
	    LOG.error(NO_PROPERTIES_MESSAGE);
	} else {
	    final String key = MODULE_PREFIX + '.' + UNIPROT_SE_DB_IDENT_REGEX_KEY;

	    result = props.getProperty(key);

	    if (result == null) {
		LOG.error(NO_PROPERTY_MESSAGE, key);
	    }

	}

	return result;
    }

    /* Private methods */
    private static Properties getProperties() {
	Properties result = null;

	synchronized (CONFIGURATION_LOCK) {

	    if (properties == null) {
		result = PropertiesUtils.loadProperties(PROPERTIES_FILE_NAME);

		properties = result; // Cache properties Map
	    } else {
		result = properties;
	    }

	} // End of synchronized block on CONFIGURATION_LOCK

	return result;
    }

    /**
     * Split parts and trim each part.
     * 
     * @param rawString
     * @return
     */
    private static String[] split(final String rawString) {
	final String[] parts = rawString.split(PARTS_SEPARATOR, -1);

	final int nParts = parts.length;

	final List<String> trimmedParts = new ArrayList<>(nParts);

	for (int i = 0; i < nParts; ++i) {
	    final String rawPart = parts[i];

	    trimmedParts.add(rawPart.trim());
	}

	return trimmedParts.toArray(new String[nParts]);
    }

}
