package fr.proline.module.parser.maxquant.model;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import fr.proline.core.om.model.msi.IPeaklistContainer;
import fr.proline.core.om.model.msi.Ms2Query;
import fr.proline.core.om.model.msi.PeptideMatch;
import fr.proline.core.om.model.msi.ResultSet;
import fr.proline.core.om.model.msi.Spectrum;
import scala.Function1;
import scala.runtime.BoxedUnit;

public class MQPeaklistContainer implements IPeaklistContainer {

	private ResultSet m_rs;
	Map<Long,Spectrum> m_spectraById;
	
	public MQPeaklistContainer(ResultSet rs, Map<Long,Spectrum> spectraById) {
		m_rs = rs;
		m_spectraById = spectraById;
	}
	
	
	@Override
	public void eachSpectrum(Function1<Spectrum, BoxedUnit> onEachSpectrum) {
//		logger.info("Start iterate over MSQueries");
		PeptideMatch[] allPepMatches =m_rs.peptideMatches();
		List<Long> queryDone = new ArrayList<>();
		
		for(PeptideMatch pm : allPepMatches){
			if(!queryDone.contains(pm.msQueryId())){
				queryDone.add(pm.msQueryId());
				//Suppose associated msQuery is an Ms2Query!
				Ms2Query msQ = (Ms2Query) pm.msQuery();
				onEachSpectrum.apply(m_spectraById.get(msQ.spectrumId()));					
			}			
		}
		
	}
	
	

}
