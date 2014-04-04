package fr.proline.module.seq.util;

public final class FileUtils {

    /* Private constructor (Utility class) */
    private FileUtils() {
    }

    public static String[] splitFilePath(final String fullFilePath) {

	if (fullFilePath == null) {
	    throw new IllegalArgumentException("FullFilePath is null");
	}

	final boolean isLinux = fullFilePath.contains("/");
	final boolean isWindows = fullFilePath.contains("\\");

	if (isLinux && isWindows) {
	    throw new IllegalArgumentException("Cannot guess Linux or Windows file name separator from ["
		    + fullFilePath + ']');
	}

	String[] result = null;

	if (isLinux) {
	    result = fullFilePath.split("/+");
	} else if (isWindows) {
	    result = fullFilePath.split("\\\\+");
	} else {
	    result = new String[] { fullFilePath, };
	}

	return result;
    }

    public static String extractFileName(final String fullFilePath) {
	final String[] pathParts = FileUtils.splitFilePath(fullFilePath);

	String fileName = null;

	final int nParts = pathParts.length;

	if (nParts > 0) {
	    fileName = pathParts[nParts - 1];
	} else {
	    throw new IllegalArgumentException("Invalid fullFilePath [" + fullFilePath + ']');
	}

	return fileName;
    }

}
