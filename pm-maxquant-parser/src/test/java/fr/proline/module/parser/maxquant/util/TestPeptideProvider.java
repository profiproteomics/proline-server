package fr.proline.module.parser.maxquant.util;

import fr.proline.core.om.model.msi.LocatedPtm;
import fr.proline.core.om.model.msi.Peptide;
import fr.proline.core.om.provider.msi.IPeptideProvider;
import fr.proline.core.om.provider.msi.PeptideFakeProvider;
import scala.Option;
import scala.Tuple2;
import scala.collection.Seq;

public class TestPeptideProvider implements IPeptideProvider {

	@Override
	public Option<Peptide> getPeptide(long arg0) {
		return PeptideFakeProvider.getPeptide(arg0);
	}

	@Override
	public Option<Peptide> getPeptide(String arg0, LocatedPtm[] arg1) {
		return PeptideFakeProvider.getPeptide(arg0, arg1);
	}

	@Override
	public Peptide[] getPeptides(Seq<Object> arg0) {
		return PeptideFakeProvider.getPeptides(arg0);
	}

	@Override
	public Option<Peptide>[] getPeptidesAsOptions(Seq<Object> arg0) { 
		return PeptideFakeProvider.getPeptidesAsOptions(arg0);
	}

	@Override
	public Option<Peptide>[] getPeptidesAsOptionsBySeqAndPtms(Seq<Tuple2<String, LocatedPtm[]>> arg0) {
		return PeptideFakeProvider.getPeptidesAsOptionsBySeqAndPtms(arg0);
	}

}
