package fr.proline.module.seq;

import static fr.profi.util.StringUtils.LINE_SEPARATOR;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.junit.Ignore;

import fr.proline.module.seq.dto.BioSequenceWrapper;
import fr.proline.module.seq.dto.RepositoryIdentifierWrapper;

/**
 * Manual test : requires PostgreSQL connection.
 * 
 * @author LMN
 * 
 */
@Ignore
public final class TestBioSequenceProvider {

    private static final String SEPARATOR = " | ";

    /* Private constructor (Utility class) */
    private TestBioSequenceProvider() {
    }

    public static void main(final String[] args) {
	final List<String> values = new ArrayList<>();
	values.add("F6ZQA3_MOUSE");
	values.add("NCK1_strep-tag");
	values.add("#C#Q9U6Y5");
	values.add("Human");
	values.add("###REV###H0QFK6_ECOLI");
	values.add("K7EIV0_HUMAN");
	values.add("ZZZ toto");

	final Map<String, List<BioSequenceWrapper>> result = BioSequenceProvider
		.findBioSequencesBySEDbIdentValues(values);

	for (final Map.Entry<String, List<BioSequenceWrapper>> entry : result.entrySet()) {
	    final String seDbIdentValue = entry.getKey();

	    final List<BioSequenceWrapper> bioSequences = entry.getValue();

	    for (final BioSequenceWrapper bsw : bioSequences) {
		System.out.println();
		System.out.println(formatBioSequenceWrapper(seDbIdentValue, bsw));
	    }
	}
    }
    private static String formatBioSequenceWrapper(final String seDbIdentValue,
	    final BioSequenceWrapper bioSequenceW) {
	assert (!StringUtils.isEmpty(seDbIdentValue)) : "formatBioSequenceWrapper() invalid seDbIdentValue";
	assert (bioSequenceW != null) : "formatBioSequenceWrapper() bioSequenceW is null";
	final StringBuilder buff = new StringBuilder();
	buff.append(seDbIdentValue);
	buff.append(SEPARATOR);

	buff.append(bioSequenceW.getSEDbInstance().getName());
	buff.append(SEPARATOR);

	buff.append(bioSequenceW.getSEDbRelease());
	buff.append(SEPARATOR);

	buff.append(bioSequenceW.getSEDbInstance().getSourcePath());
	buff.append(SEPARATOR);

	buff.append(bioSequenceW.getSequenceId());
	buff.append(SEPARATOR);

	buff.append(bioSequenceW.getSequence());
	buff.append(SEPARATOR);

	buff.append(bioSequenceW.getMass());
	buff.append(SEPARATOR);

	buff.append(bioSequenceW.getPI());

	final RepositoryIdentifierWrapper repositoryIdentW = bioSequenceW.getRepositoryIdentifier();

	if (repositoryIdentW != null) {
	    buff.append(LINE_SEPARATOR);
	    buff.append("    ");
	    buff.append(repositoryIdentW.getRepositoryName());
	    buff.append(SEPARATOR);

	    buff.append(repositoryIdentW.getRepositoryURL());
	    buff.append(SEPARATOR);

	    buff.append(repositoryIdentW.getRepositoryIdentifierValue());
	}
	return buff.toString();
    }

}
