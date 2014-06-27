package fr.proline.module.seq.dto;

import java.io.Serializable;

import fr.proline.module.seq.orm.Alphabet;
import fr.profi.util.StringUtils;

public class SEDbInstanceWrapper implements Serializable {

    private static final long serialVersionUID = 2L;

    private final String m_name;

    private final Alphabet m_alphabet;

    private final String m_sourcePath;

    public SEDbInstanceWrapper(final String name, final Alphabet alphabet, final String sourcePath) {

	if (StringUtils.isEmpty(name)) {
	    throw new IllegalArgumentException("Invalid name");
	}

	m_name = name;

	m_alphabet = alphabet;

	if (StringUtils.isEmpty(sourcePath)) {
	    throw new IllegalArgumentException("Invalid sourcePath");
	}

	m_sourcePath = sourcePath;
    }

    public String getName() {
	return m_name;
    }

    public String getSourcePath() {
	return m_sourcePath;
    }

    /**
     * Can be <code>null</code>.
     * 
     * @return
     */
    public Alphabet getAlphabet() {
	return m_alphabet;
    }

    @Override
    public int hashCode() {
	return getSourcePath().hashCode();
    }

    @Override
    public boolean equals(final Object obj) {
	boolean result = false;

	if (this == obj) {
	    result = true;
	} else if (obj instanceof SEDbInstanceWrapper) {
	    final SEDbInstanceWrapper otherInstance = (SEDbInstanceWrapper) obj;

	    result = (getName().equals(otherInstance.getName()) && getSourcePath().equals(
		    otherInstance.getSourcePath()));
	}

	return result;
    }

}
