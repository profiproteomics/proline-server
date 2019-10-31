package fr.proline.module.seq.jms.service;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever;

import fr.profi.util.jsonrpc.IJSONRPC2Method;
import fr.profi.util.jsonrpc.IJSONRPC2MethodParameter;
import fr.profi.util.jsonrpc.JSONRPC2Method;
import fr.profi.util.jsonrpc.JSONRPC2MethodParameter;
import fr.profi.util.jsonrpc.JSONRPC2Utils;
import fr.proline.jms.service.api.AbstractRemoteProcessingService;
import fr.proline.jms.service.api.RemoteServiceIdentity;
import fr.proline.module.seq.service.RetrieveService;
import fr.proline.module.seq.util.ScalaUtil;
import scala.Option;
import scala.Some;
import scala.collection.JavaConversions;
import scala.collection.Seq;


/**
 * Define JMS Service to : Retrieve BioSequence for all validated protein of specified RSMS. This service will also calculate coverage and protein mass if
 * necessary
 * 
 * Input Params : project_id : The id of the project to get bioSequence for . result_summaries_ids : The list of the result summaries to retrieve be bioSequence
 * for force_update : specify if biosequences are retrieve and properties are calculate even if they already exit
 * 
 * Output Params : boolean true if service run successfully false otherwise
 */
public class RetrieveJMSService extends AbstractRemoteProcessingService {

	@SuppressWarnings("unused")
	private static final Logger LOG = LoggerFactory.getLogger(RetrieveJMSService.class);
	private static String serviceName = "RetrieveBioSeqForRSMs";
	private static String serviceDescription = "Retrieve BioSequence for all validated protein of specified Identification Summaries. This service will also calculate coverage and protein mass if necessary";
	private static String serviceNamespace = "proline/seq";
	private static String serviceVersion = "1.0";
	private static Boolean isDefaultVersion = true;

	@Override
	public String serviceNamespace() {
		return serviceNamespace;
	}
	
	@Override
	public String serviceLabel() {
		return serviceName;
	}

	@Override
	public String serviceVersion() {
		return serviceVersion;
	}

	@Override
	public boolean isDefaultVersion() {
		return isDefaultVersion;
	}

	@Override
	public Option<String> serviceDescription() {
		return new Some<String>(serviceDescription);
	}

	@Override
	public Object doProcess(NamedParamsRetriever paramsRetriever) throws Exception {
		if (paramsRetriever == null || paramsRetriever.size() <= 0)
			throw new IllegalArgumentException(" No parameter specified");

		Boolean force = (paramsRetriever.hasParam("force_update")) ? paramsRetriever.getBoolean("force_update") : false;
		Long projectId = paramsRetriever.getLong("project_id");
		List<Object> rsmsIdsStr = paramsRetriever.getList("result_summaries_ids");
		List<Long> rsmsIds = new ArrayList<Long>();
		rsmsIdsStr.forEach(idAsStr -> rsmsIds.add(new Long(idAsStr.toString())));

		RetrieveService.retrieveBioSequences(projectId, force, rsmsIds);

		return true; // If get at the end, no exception thrown !
	}


	@Override
	public Seq<IJSONRPC2Method> methodDefinitions() {
		//Create Parameters project_id  result_summaries_ids force_update
		
		ArrayList<IJSONRPC2MethodParameter> parameters = new ArrayList<>();
		JSONRPC2MethodParameter  PROJECT_ID_PARAM   =  JSONRPC2Utils.buildStringMethodParameter("project_id","The id of the project to get bioSequence for", false, ScalaUtil.<String>none());		
		parameters.add(PROJECT_ID_PARAM);
		
		JSONRPC2MethodParameter  RSM_IDS_PARAM =  JSONRPC2Utils.buildArrayLongMethodParameter("result_summaries_ids", "The list of the result summaries to retrieve be bioSequence", false, ScalaUtil.<String>none());		
		parameters.add(RSM_IDS_PARAM);

		JSONRPC2MethodParameter  FORCE_UPDATE_PARAM =  JSONRPC2Utils.buildBooleanMethodParameter("force_update", "specify if biosequences are retrieve and properties are calculate even if they already exit", true, new Some<String>("false"));		
		parameters.add(FORCE_UPDATE_PARAM);
		
		Seq<IJSONRPC2MethodParameter> params = JavaConversions.asScalaBuffer(parameters).toList();

		JSONRPC2Method meth = new JSONRPC2Method( RemoteServiceIdentity.PROCESS_METHOD_NAME(),serviceDescription,params, JSONRPC2Utils.buildBooleanMethodReturn("true: if operation was successful"), false);
		ArrayList<IJSONRPC2Method> methods = new ArrayList<>();
		methods.add(meth);
		return JavaConversions.asScalaBuffer(methods).toList(); 
	}



}
