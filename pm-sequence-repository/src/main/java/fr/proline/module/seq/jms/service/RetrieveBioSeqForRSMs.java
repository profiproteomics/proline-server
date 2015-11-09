package fr.proline.module.seq.jms.service;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever;

import fr.proline.jms.service.api.AbstractRemoteProcessService;
import fr.proline.module.seq.service.RetrieveService;

/**
 *  Define JMS Service to :
 *  Retrieve BioSequence for all validated protein of specified RSMS. This service will also
 *  calculate coverage and protein mass if necessary
 *  
 *  Input Params :
 *    project_id : The id of the project to get bioSequence for .
 *    result_summaries_ids : The list of the result summaries to retrieve be bioSequence for
 *    force_update : specify if biosequences are retrieve and properties are calculate even if they already exit
 *    
 *  Output Params :
 *    "OK" if service run successfully
 *    Error message if service was not successful
 */
public class RetrieveBioSeqForRSMs extends AbstractRemoteProcessService {
	
	@SuppressWarnings("unused")
	private static final Logger LOG = LoggerFactory.getLogger(RetrieveBioSeqForRSMs.class);
	private static String serviceName = "proline/seq/RetrieveBioSeqForRSMs";
	private static String serviceVersion = "1.0";
	private static Boolean isDefaultVersion = true;
	
	@Override
	public String serviceName() {
		return serviceName;
	}

	@Override
	public String serviceVersion() {
		return serviceVersion;
	}

	@Override
	public boolean defaultVersion() {
		return isDefaultVersion;
	}

	@Override
	public Object doProcess(NamedParamsRetriever paramsRetriever) throws Exception {
		if(paramsRetriever == null || paramsRetriever.size() <=0)
			throw new IllegalArgumentException(" No parameter specified");
						
	    Boolean force = (paramsRetriever.hasParam("force_update")) ? paramsRetriever.getBoolean("force_update") : false;
		Long projectId = paramsRetriever.getLong("project_id");
	    List<Object> rsmsIdsStr = paramsRetriever.getList("result_summaries_ids");
	    List<Long> rsmsIds = new ArrayList<Long>();
	    rsmsIdsStr.forEach( idAsStr -> rsmsIds.add(new Long(idAsStr.toString() )) );

		RetrieveService.retrieveBioSequencesForRsms(projectId, rsmsIds, force);
		
		return true; // If get at the end, no exception thrown !
	}
	
	

}
