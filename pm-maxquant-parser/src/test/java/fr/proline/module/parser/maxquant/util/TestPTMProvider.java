package fr.proline.module.parser.maxquant.util;

import fr.proline.core.om.model.msi.PtmDefinition;
import fr.proline.core.om.provider.msi.IPTMProvider;
import fr.proline.core.om.provider.msi.PTMFakeProvider;
import scala.Enumeration.Value;
import scala.Option;
import scala.collection.Seq;

public class TestPTMProvider implements IPTMProvider {

	@Override
	public Option<PtmDefinition> getPtmDefinition(long arg0) {
		return PTMFakeProvider.getPtmDefinition(arg0);
	}

	@Override
	public Option<PtmDefinition> getPtmDefinition(String arg0, char arg1, Value arg2) {
		return PTMFakeProvider.getPtmDefinition(arg0, arg1, arg2);
	}

	@Override
	public Option<PtmDefinition> getPtmDefinition(double arg0, double arg1, char arg2, Value arg3) {
		return PTMFakeProvider.getPtmDefinition(arg0, arg1, arg2, arg3);
	}

	@Override
	public PtmDefinition[] getPtmDefinitions(Seq<Object> arg0) {
		return PTMFakeProvider.getPtmDefinitions(arg0);
	}

	@Override
	public Option<PtmDefinition>[] getPtmDefinitionsAsOptions(Seq<Object> arg0) {
		return PTMFakeProvider.getPtmDefinitionsAsOptions(arg0);
	}

	@Override
	public Option<Object> getPtmId(String arg0) {
		return PTMFakeProvider.getPtmId(arg0);
	}
	

}
