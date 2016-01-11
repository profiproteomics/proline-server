package fr.proline.module.seq.util;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class RegExUtil {
	
	private static final Logger LOG = LoggerFactory.getLogger(RegExUtil.class);
	
	public static String parseReleaseVersion(final String fastaFileName, final String releaseRegEx) {
		assert (fastaFileName != null) : "parseReleaseVersion() fastaFileName is null";
		assert (releaseRegEx != null) : "parseReleaseVersion() releaseRegEx is null";
				
		Pattern releasePattern = Pattern.compile(releaseRegEx, Pattern.CASE_INSENSITIVE);			

		String result = null;
		if(releasePattern != null) {
    		final Matcher matcher = releasePattern.matcher(fastaFileName);
    
    		if (matcher.find()) {
    
    			if (matcher.groupCount() < 1) {
    				throw new IllegalArgumentException("Invalid Release version Regex");
    			}
    
    			result = matcher.group(1).trim();
    		} else {
    			LOG.warn("Cannot parse fastaFileName [{}] with \"{}\" Regex", fastaFileName,
    				releasePattern.pattern());
    		}
		}
		return result;
	}
}
