package fr.proline.module.seq.dto;

import java.io.Serializable;

import fr.proline.module.seq.orm.Alphabet;
import fr.profi.util.StringUtils;

public class DDatabankInstance implements Serializable {

	private static final long serialVersionUID = 2L;

	private final String m_name;

	private final Alphabet m_alphabet;

	private final String m_sourcePath;

	private String m_release = null;

	public DDatabankInstance(final String name, final Alphabet alphabet, final String sourcePath) {

		assert !StringUtils.isEmpty(name) : "Invalid name";
 		assert !StringUtils.isEmpty(sourcePath): "Invalid sourcePath";

		m_name = name;
		m_alphabet = alphabet;
		m_sourcePath = sourcePath;

	}

	public String getName() {
		return m_name;
	}

	public String getSourcePath() {
		return m_sourcePath;
	}

	public Alphabet getAlphabet() {
		return m_alphabet;
	}

	public String getRelease() {
		return m_release;
	}

	public void setRelease(String release){
		this.m_release = release;
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
		} else if (obj instanceof DDatabankInstance) {
			final DDatabankInstance otherInstance = (DDatabankInstance) obj;
			result = (getName().equals(otherInstance.getName()) && getSourcePath().equals(otherInstance.getSourcePath()));
		}

		return result;
	}

}
