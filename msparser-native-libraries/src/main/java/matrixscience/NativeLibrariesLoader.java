package matrixscience;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import fr.proline.util.ThreadLogger;
import fr.proline.util.system.OSInfo;
import fr.proline.util.system.OSType;

/**
 * Utility class used to load the appropriate Mascot Parser native libraries (libmsparserj.so or
 * msparserj.dll) into running JVM depending on OS Type and arch.
 * 
 * @author LMN
 * 
 */
public final class NativeLibrariesLoader {

    /* Constants */
    private static final Logger LOG = LoggerFactory.getLogger(NativeLibrariesLoader.class);

    private static final String LINUX_LIBRARY_NAME = "libmsparserj.so";

    private static final String LINUX_I386_LIBRARY_PATHNAME = "native/Linux/i386/" + LINUX_LIBRARY_NAME;

    private static final String LINUX_AMD64_LIBRARY_PATHNAME = "native/Linux/amd64/" + LINUX_LIBRARY_NAME;

    private static final String WINDOWS_LIBRARY_NAME = "msparserj.dll";

    private static final String WINDOWS_X86_LIBRARY_PATHNAME = "native/Windows/x86/" + WINDOWS_LIBRARY_NAME;

    private static final String WINDOWS_AMD64_LIBRARY_PATHNAME = "native/Windows/amd64/"
	    + WINDOWS_LIBRARY_NAME;

    private static final String TMP_DIR_KEY = "java.io.tmpdir";

    private static final int BUFFER_SIZE = 8192;

    private static final Object LOADER_LOCK = new Object();

    /* Static class variables */

    /* All mutable fields are @GuardedBy("LOADER_LOCK") */

    private static boolean loaded;

    /* Private constructor (Utility class) */
    private NativeLibrariesLoader() {
    }

    /* Public class methods */
    /**
     * Loads the appropriate Mascot Parser native libraries into running JVM. The method can be called
     * multiple times : the native library is loaded only once.
     * 
     * @return <code>true</code> if the appropriate native libraries have been effectively loaded.
     */
    public static boolean loadNativeLibraries() {
	boolean result = false;

	synchronized (LOADER_LOCK) {

	    if (!loaded) {
		final Thread currentThread = Thread.currentThread();

		if (!(currentThread.getUncaughtExceptionHandler() instanceof ThreadLogger)) {
		    currentThread.setUncaughtExceptionHandler(new ThreadLogger(LOG));
		}

		LOG.debug("Loading Mascot Parser native libraries");

		String libraryPathname = null;
		String targetLibraryName = null;

		final OSType osType = OSInfo.getOSType();

		if (osType != null) {
		    LOG.debug("Detected OS Type {}", osType);
		}

		switch (osType) {

		case LINUX_I386:
		    libraryPathname = LINUX_I386_LIBRARY_PATHNAME;
		    targetLibraryName = LINUX_LIBRARY_NAME;
		    break;

		case LINUX_AMD64:
		    libraryPathname = LINUX_AMD64_LIBRARY_PATHNAME;
		    targetLibraryName = LINUX_LIBRARY_NAME;
		    break;

		case WINDOWS_X86:
		    libraryPathname = WINDOWS_X86_LIBRARY_PATHNAME;
		    targetLibraryName = WINDOWS_LIBRARY_NAME;
		    break;

		case WINDOWS_AMD64:
		    libraryPathname = WINDOWS_AMD64_LIBRARY_PATHNAME;
		    targetLibraryName = WINDOWS_LIBRARY_NAME;
		    break;

		}

		if ((libraryPathname != null) && (targetLibraryName != null)) {
		    final File nativeLibrary = extractLibrary(libraryPathname, targetLibraryName);

		    if ((nativeLibrary != null) && nativeLibrary.isFile()) {
			final String nativeLibraryAbsolutePathname = nativeLibrary.getAbsolutePath();

			LOG.debug("Loading [{}]", nativeLibraryAbsolutePathname);

			System.load(nativeLibraryAbsolutePathname);

			LOG.info("Library [{}] successfully loaded", nativeLibraryAbsolutePathname);

			loaded = true;
		    } else {
			LOG.warn("Invalid extracted [{}] file", targetLibraryName);
		    }

		} else {
		    LOG.warn("No known native library for OS Type {}", osType);
		} // End if ((libraryPathname is null) OR (targetLibraryName is null))

	    } // End if (not loaded)

	    result = loaded;
	} // End of synchronized block on LOADER_LOCK

	return result;
    }

    /* Private methods */
    private static File extractLibrary(final String libraryPathname, final String targetLibraryName) {
	File result = null;

	InputStream is = null;
	OutputStream os = null;

	try {
	    final ClassLoader classLoader = Thread.currentThread().getContextClassLoader();

	    is = classLoader.getResourceAsStream(libraryPathname);

	    if (is == null) {
		LOG.warn("Cannot find [{}] library", libraryPathname);
	    } else {
		final String tmpDirPathname = System.getProperty(TMP_DIR_KEY);

		if (tmpDirPathname == null) {
		    LOG.warn("Unable to retrieve \"{}\" system property", TMP_DIR_KEY);
		} else {
		    final File tmpDir = new File(tmpDirPathname);

		    if (tmpDir.isDirectory()) {
			final File targetLibraryFile = new File(tmpDir, targetLibraryName);

			LOG.debug("Extracting [{}] to [{}]", libraryPathname,
				targetLibraryFile.getAbsoluteFile());

			os = new FileOutputStream(targetLibraryFile);

			final byte[] buffer = new byte[BUFFER_SIZE];

			int readLength = is.read(buffer);

			while (readLength != -1) {
			    os.write(buffer, 0, readLength);

			    readLength = is.read(buffer);
			} // End of copy loop

			result = targetLibraryFile;
		    } else {
			LOG.warn("Invalid temp directory [{}]", tmpDirPathname);
		    }

		} // End if (tmpDirPathname is not null)

	    } // End if (is is not null)

	} catch (Exception ex) {
	    LOG.error("Error extracting [" + libraryPathname + ']', ex);
	} finally {

	    if (os != null) {
		try {
		    os.close();
		} catch (IOException exClose) {
		    result = null; // Invalidate returned file

		    LOG.error("Error closing [" + targetLibraryName + "] OutputStream", exClose);
		}
	    }

	    if (is != null) {
		try {
		    is.close();
		} catch (IOException exClose) {
		    LOG.error("Error closing [" + libraryPathname + "] InputStream", exClose);
		}
	    }

	}

	return result;
    }

}
