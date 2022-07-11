package fr.proline.module.seq.jms.service;

import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever;
import fr.profi.util.jsonrpc.IJSONRPC2Method;
import fr.profi.util.jsonrpc.IJSONRPC2MethodParameter;
import fr.profi.util.jsonrpc.JSONRPC2Method;
import fr.profi.util.jsonrpc.JSONRPC2Utils;
import fr.proline.jms.service.api.AbstractRemoteProcessingService;
import fr.proline.jms.service.api.RemoteServiceIdentity;
import fr.proline.module.seq.service.RetrieveService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.Option;
import scala.Some;
import scala.collection.JavaConverters;
import scala.collection.Seq;

import java.util.ArrayList;
import java.util.List;

public class RetrieveInfoService extends AbstractRemoteProcessingService {

    private static final Logger LOG = LoggerFactory.getLogger(RetrieveInfoService.class);
    private static String serviceName = "isServiceAlive";
    private static String serviceDescription = "Service to permit to test if Retrieve BioSequence service is available";
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
    public Option<String> serviceDescription() {
        return new Some<String>(serviceDescription);
    }

    @Override
    public boolean isNodeSpecific() {
        return false;
    }

    @Override
    public Object doProcess(NamedParamsRetriever paramsRetriever) throws Exception {
            return true;
    }

    @Override
    public Seq<IJSONRPC2Method> methodDefinitions() {
        ArrayList<IJSONRPC2MethodParameter> parameters = new ArrayList<>();

        Seq<IJSONRPC2MethodParameter> params = JavaConverters.asScalaBuffer(parameters).toList();

        JSONRPC2Method meth = new JSONRPC2Method( RemoteServiceIdentity.PROCESS_METHOD_NAME(),serviceDescription,params, JSONRPC2Utils.buildBooleanMethodReturn("true: if service is available"), false);
        ArrayList<IJSONRPC2Method> methods = new ArrayList<>();
        methods.add(meth);
        return JavaConverters.asScalaBuffer(methods).toList();
    }

}
