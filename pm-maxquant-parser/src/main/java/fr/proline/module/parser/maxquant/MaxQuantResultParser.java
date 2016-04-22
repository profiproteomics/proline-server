package fr.proline.module.parser.maxquant;

import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import fr.profi.util.StringUtils;
import fr.profi.util.serialization.ProfiJson;
import fr.proline.api.service.IServiceWrapper;
import fr.proline.context.DatabaseConnectionContext;
import fr.proline.context.MsiDbConnectionContext;
import fr.proline.core.dal.tables.msi.MsiDbPeaklistSoftwareColumns;
import fr.proline.core.om.model.msi.InstrumentConfig;
import fr.proline.core.om.model.msi.MsQuery;
import fr.proline.core.om.model.msi.Peaklist;
import fr.proline.core.om.model.msi.PeaklistSoftware;
import fr.proline.core.om.model.msi.PeptideMatch;
import fr.proline.core.om.model.msi.PtmDefinition;
import fr.proline.core.om.model.msi.ResultSet;
import fr.proline.core.om.model.msi.SearchSettings;
import fr.proline.core.om.model.msi.Spectrum;
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext;
import fr.proline.core.om.provider.msi.IPTMProvider;
import fr.proline.core.om.provider.msi.ISeqDatabaseProvider;
import fr.proline.core.om.provider.msi.impl.SQLInstrumentConfigProvider;
import fr.proline.core.om.storer.msi.IPeaklistWriter;
import fr.proline.core.om.storer.msi.IRsStorer;
import fr.proline.core.om.storer.msi.RsStorer;
import fr.proline.core.om.storer.msi.impl.SQLMsiSearchWriter;
import fr.proline.core.om.storer.msi.impl.StorerContext;
import fr.proline.module.parser.maxquant.model.MQPeaklistContainer;
import fr.proline.module.parser.maxquant.model.ResultSetsDataMapper;
import fr.proline.repository.util.JDBCWork;
import scala.Option;
import scala.collection.JavaConversions;

public class MaxQuantResultParser extends IServiceWrapper {
	
	protected static Logger logger = LoggerFactory.getLogger(MaxQuantResultParser.class);
	
	private ProviderDecoratedExecutionContext m_parserContext;
	private Long m_instrumConfigId;
	private Long m_peaklistSoftwareId;
	private String m_resultFolder;
	
	//Created value to return !
	private StringBuffer m_warningMsg;	
	private List<Long> m_resultSetsIds;
	
	public MaxQuantResultParser(ProviderDecoratedExecutionContext parserContext, Long instrumConfigId,Long peaklistSoftwareId, String resultFolder ){
		m_parserContext = parserContext;
		m_instrumConfigId = instrumConfigId;
		m_peaklistSoftwareId = peaklistSoftwareId;
		m_resultFolder = resultFolder;					
	}
	
	@Override
	public boolean runService() {
		
		logger.info("Running MaxQuant Parser on "+m_resultFolder);
		m_resultSetsIds = new ArrayList<Long>();
		m_warningMsg = new StringBuffer();
		
		MsiDbConnectionContext msiDbCtx = m_parserContext.getMSIDbConnectionContext();
		Boolean localMSITransaction= false;
		Boolean msiTransacOk= false;
		StorerContext storerContext = null;
		
		try {
			// Check if a transaction is already initiated
			if (!msiDbCtx.isInTransaction()) {
				msiDbCtx.beginTransaction();
		        localMSITransaction = true;
		        msiTransacOk = false;
			} 
		
			// Retrieve the instrument configuration
			SQLInstrumentConfigProvider instConfigProvider = new SQLInstrumentConfigProvider(m_parserContext.getUDSDbConnectionContext());
			Option<InstrumentConfig> instrumentConfigOpt = instConfigProvider.getInstrumentConfig(m_instrumConfigId);
			if(instrumentConfigOpt.isEmpty()){
				throw new RuntimeException("can't find an Instrument Config for id = " + m_instrumConfigId);
			}
			
			PeaklistSoftware peaklistSoft = getOrCreatePeaklistSoftware();		
			
			//Parse Search Parameters and meta data
			logger.info("Parse Experiment properties ");
			ISeqDatabaseProvider seqDbProvider= m_parserContext.getProvider(ISeqDatabaseProvider.class);
			IPTMProvider ptmProvider= m_parserContext.getProvider(IPTMProvider.class);
			ExperimentPropertiesReader propReader = new ExperimentPropertiesReader(m_resultFolder, seqDbProvider, ptmProvider, instrumentConfigOpt.get(), peaklistSoft);
			Map<String, Long> rsIdByName = propReader.getResultSetIds();		
			
			SearchSettings ss = propReader.getSearchSettings();
			PtmDefinition[] varPtms = ss.variablePtmDefs();
			PtmDefinition[] fixedPtms = ss.fixedPtmDefs();
			PtmDefinition[] allPtms = Arrays.copyOf(varPtms, varPtms.length+fixedPtms.length);
			System.arraycopy(fixedPtms, 0, allPtms, varPtms.length, fixedPtms.length);
	
			//Parse ResultSet data : queries to protein matches
			logger.info("Parse and create ResultSet MS Data ");
			MSDataReader dataReader = new MSDataReader(m_resultFolder, m_parserContext, instrumentConfigOpt.get(), peaklistSoft);
			ResultSetsDataMapper rsMapper= dataReader.parseMSData2ResulSets(rsIdByName, allPtms, m_warningMsg);
			
			logger.info("Create Experiment properties ");
			List<ResultSet> resultSets = propReader.parseAndCreateResultSetParams(rsMapper);
								
			logger.info("Storing ResultSets");
			storerContext = StorerContext.apply(m_parserContext);
			m_resultSetsIds = storeResultSets(resultSets, instrumentConfigOpt.get(), rsMapper, storerContext);
			logger.info("Storing ResultSets DONE "+m_resultSetsIds);
			// Commit transaction if it was initiated locally
			if (localMSITransaction) {
				msiDbCtx.commitTransaction();
			}
			msiTransacOk = true;
		    	  
		}catch( Throwable t){
			logger.error("Error while importing resultFile", t);
			throw new RuntimeException(t);
		} finally{
		
			if (storerContext != null) {
		          storerContext.clearContext();
			}

			if (localMSITransaction && !msiTransacOk) {
				logger.info("Rollbacking MSI Db Transaction");

				try {
		            msiDbCtx.rollbackTransaction();
				} catch (Exception ex){
					logger.error("Error rollbacking MSI Db Transaction", ex);
				}

        	}

		}
		return true;
		
	}
	
	public List<Long> getCreatedResultSetIds(){
		return m_resultSetsIds;	
	}
	
	public String getWarningMessage(){
		if(m_warningMsg!=null)
			return m_warningMsg.toString();
		return null;
	}
	

	 private PeaklistSoftware getOrCreatePeaklistSoftware() { 

		 MsiDbConnectionContext msiDbCtx = this.m_parserContext.getMSIDbConnectionContext();
		 fr.proline.core.om.provider.msi.impl.SQLPeaklistSoftwareProvider msiPklSoftProvider = new fr.proline.core.om.provider.msi.impl.SQLPeaklistSoftwareProvider(msiDbCtx);
		 fr.proline.core.om.provider.uds.impl.SQLPeaklistSoftwareProvider udsPklSoftProvider = new fr.proline.core.om.provider.uds.impl.SQLPeaklistSoftwareProvider(this.m_parserContext.getUDSDbConnectionContext());

		 Option<PeaklistSoftware> udsPklSoftOpt = udsPklSoftProvider.getPeaklistSoftware(m_peaklistSoftwareId);
		 if(udsPklSoftOpt.isEmpty())
			 throw new RuntimeException("can't find a peaklist software for id = " + m_peaklistSoftwareId);

		 // Try to retrieve peaklist software from the MSidb
		 Option<PeaklistSoftware> msiPklSoftOpt = msiPklSoftProvider.getPeaklistSoftware(m_peaklistSoftwareId);
		 PeaklistSoftware pklSoft = udsPklSoftOpt.get();
		 if (msiPklSoftOpt.isEmpty()) {
			 // If it doesn't exist => retrieve from the UDSdb      			 
			 JDBCWork inserWork = new JDBCWork() {
				
				@Override
				public void execute(Connection connection) throws SQLException {
					
					String properties = (pklSoft.properties() != null && pklSoft.properties().isDefined()) ? ProfiJson.serialize(pklSoft.properties()) : null;

					StringBuilder sb = new StringBuilder(" INSERT INTO peaklist_software (");
					sb.append(MsiDbPeaklistSoftwareColumns.ID()).append(",");
					sb.append(MsiDbPeaklistSoftwareColumns.NAME()).append(",");
					sb.append(MsiDbPeaklistSoftwareColumns.VERSION()).append(",");
					sb.append(MsiDbPeaklistSoftwareColumns.SERIALIZED_PROPERTIES());
					sb.append(") VALUES (").append(pklSoft.id()).append(",");
					sb.append("'").append(pklSoft.name()).append("' ,");
					if(StringUtils.isEmpty(pklSoft.version()))
						sb.append((String)null).append(",");
					else 
						sb.append("'").append(pklSoft.version()).append("' ,");					
					if(StringUtils.isEmpty(properties))
						sb.append(properties).append(")");
					else
						sb.append("'").append(properties).append("' )");
					Statement stmt = connection.createStatement();
					logger.warn(" WILL EXECUTE "+sb.toString());
					stmt.execute(sb.toString());					
				}
			};
			
			try {
				msiDbCtx.doWork(inserWork, false);
			} catch (SQLException e) {
				throw new RuntimeException(" Error inserting peaklist software in MSI : "+e.getMessage());
			}
		 }
		 return pklSoft;
	 }
	
	private List<Long> storeResultSets(List<ResultSet> resultSets, InstrumentConfig instrumentConfig,ResultSetsDataMapper rsMapper, StorerContext storerContext){
		ArrayList<Long> rsIds = new ArrayList<>();
		DatabaseConnectionContext msiDbContext = m_parserContext.getMSIDbConnectionContext();
		IRsStorer rsStorer = RsStorer.apply(msiDbContext);

		SQLMsiSearchWriter.insertInstrumentConfig(instrumentConfig, storerContext);
			
		
		for(ResultSet nextRS : resultSets){
			Long rsId = storeResultFile(nextRS, rsMapper, storerContext, rsStorer);
			rsIds.add(rsId);
		}
			
				
		return rsIds;		
	}

	private Long storeResultFile(ResultSet nextRS, ResultSetsDataMapper rsMapper, StorerContext storerContext,IRsStorer rsStorer) {
		String rsName = nextRS.name();
		logger.info("-- Storing  ResultSet "+rsName);
		if(nextRS.peptideMatches() == null || nextRS.peptideMatches().length <= 0)
			throw new RuntimeException(rsName+ " ResultSet has NO PeptideMatch");
		if(nextRS.proteinMatches() == null || nextRS.proteinMatches().length <= 0)
			throw new RuntimeException(rsName+ " ResultSet has NO ProteinMatch");
			
		IPeaklistWriter pklWriter =	rsStorer.getOrBuildPeaklistWriter(storerContext);
		
		// Insert the peaklist information
		Peaklist pl = nextRS.msiSearch().get().peakList();
	   	long peakListId = pklWriter.insertPeaklist(pl, storerContext);
	   	pl.id_$eq(peakListId);

	   	// TO DO ?  Create Map Query -> List peptideMatch ?? Why VDS ?!
//	   	Map<MsQuery,List<PeptideMatch>> pepMatchesByQuery = null;
	   	List<MsQuery> queries = new ArrayList<MsQuery>();
	   	for(PeptideMatch pm : nextRS.peptideMatches()){
	   		MsQuery q = pm.msQuery();
	   		if(!queries.contains(q)){
	   			queries.add(q);
	   		}	   			
	   	}
 	   	
	   	
	   	// Insert spectra contained in result file
	   	Map<Long, Spectrum> spectraById = rsMapper.getSpectrumByIdForRs(rsName);
	   	MQPeaklistContainer plContainer = new MQPeaklistContainer(nextRS, spectraById); 
	    logger.info("Storing spectra...");
	    pklWriter.insertSpectra(peakListId, plContainer, storerContext);
	    
	   	//TODO : Compute PrettyRank computePrettyRanks(rs.peptideMatches, separated = true)
	    Long rsId = rsStorer.storeResultSet(nextRS, JavaConversions.asScalaBuffer(queries).toList(), storerContext);
	    
	    
		return rsId;
	}
}
