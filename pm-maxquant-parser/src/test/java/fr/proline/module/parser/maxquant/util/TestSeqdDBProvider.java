package fr.proline.module.parser.maxquant.util;

import fr.proline.core.om.model.msi.SeqDatabase;
import fr.proline.core.om.provider.msi.ISeqDatabaseProvider;
import fr.proline.core.om.provider.msi.SeqDbEmptyFakeProvider;
import scala.Option;
import scala.collection.Seq;

public class TestSeqdDBProvider implements ISeqDatabaseProvider {
	
	public TestSeqdDBProvider(){
		
	}

	@Override
	public Option<SeqDatabase> getSeqDatabase(long arg0) {
		return SeqDbEmptyFakeProvider.getSeqDatabase(arg0);
	}

	@Override
	public Option<SeqDatabase> getSeqDatabase(String arg0, String arg1) {
		return SeqDbEmptyFakeProvider.getSeqDatabase(arg0, arg1);
	}

	@Override
	public SeqDatabase[] getSeqDatabases(Seq<Object> arg0) {
		return SeqDbEmptyFakeProvider.getSeqDatabases(arg0);
	}

	@Override
	public Option<SeqDatabase>[] getSeqDatabasesAsOptions(Seq<Object> arg0) {		
		return SeqDbEmptyFakeProvider.getSeqDatabasesAsOptions(arg0);
	}
	

}
