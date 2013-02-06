package matrixscience;

import static org.junit.Assert.*;

import org.junit.Test;

public class NativeLibrariesLoaderTest {

    @Test
    public void test() {
	final boolean loaded = NativeLibrariesLoader.loadNativeLibraries();

	assertTrue("Mascot Parser native libraries loaded", loaded);
    }

}
