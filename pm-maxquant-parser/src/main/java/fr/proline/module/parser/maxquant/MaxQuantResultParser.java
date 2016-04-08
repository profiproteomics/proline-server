package fr.proline.module.parser.maxquant;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import fr.proline.api.service.IServiceWrapper;
import fr.proline.context.DatabaseConnectionContext;
import fr.proline.core.om.model.msi.InstrumentConfig;
import fr.proline.core.om.model.msi.MsQuery;
import fr.proline.core.om.model.msi.Peaklist;
import fr.proline.core.om.model.msi.PeptideMatch;
import fr.proline.core.om.model.msi.PtmDefinition;
import fr.proline.core.om.model.msi.ResultSet;
import fr.proline.core.om.model.msi.SearchSettings;
import fr.proline.core.om.model.msi.Spectrum;
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext;
import fr.proline.core.om.provider.msi.impl.SQLInstrumentConfigProvider;
import fr.proline.core.om.storer.msi.IPeaklistWriter;
import fr.proline.core.om.storer.msi.IRsStorer;
import fr.proline.core.om.storer.msi.RsStorer;
import fr.proline.core.om.storer.msi.impl.SQLMsiSearchWriter;
import fr.proline.core.om.storer.msi.impl.StorerContext;
import fr.proline.module.parser.maxquant.model.MQPeaklistContainer;
import fr.proline.module.parser.maxquant.model.ResultSetsDataMapper;
import scala.Option;

public class MaxQuantResultParser extends IServiceWrapper {
	
	protected static Logger logger = LoggerFactory.getLogger(MaxQuantResultParser.class);
	
	private ProviderDecoratedExecutionContext m_parserContext;
	private Long m_instrumConfigId;
	private Long m_peaklistSoftwareId;
	private String m_resultFolder;
	
	public MaxQuantResultParser(ProviderDecoratedExecutionContext parserContext, Long instrumConfigId,Long peaklistSoftwareId, String resultFolder ){
		m_parserContext = parserContext;
		m_instrumConfigId = instrumConfigId;
		m_peaklistSoftwareId = peaklistSoftwareId;
		m_resultFolder = resultFolder;
					
	}
	
	@Override
	public boolean runService() {
		
		logger.info("Running MaxQuant Parser on "+m_resultFolder);
		
		// Retrieve the instrument configuration
		SQLInstrumentConfigProvider instConfigProvider = new SQLInstrumentConfigProvider(m_parserContext.getUDSDbConnectionContext());
		Option<InstrumentConfig> instrumentConfigOpt = instConfigProvider.getInstrumentConfig(m_instrumConfigId);
		if(instrumentConfigOpt.isEmpty()){
			throw new RuntimeException("can't find an Instrument Config for id = " + m_instrumConfigId);
		}
		
		//Parse Search Parameters and meta data
		logger.info("Parse Experiment properties ");
		ExperimentPropertiesReader propReader = new ExperimentPropertiesReader(m_resultFolder, m_parserContext, instrumentConfigOpt.get(), m_peaklistSoftwareId);
		Map<String, Long> rsIdByName = propReader.getResultSetIds();		
		
		SearchSettings ss = propReader.getSearchSettings();
		PtmDefinition[] varPtms = ss.variablePtmDefs();
		PtmDefinition[] fixedPtms = ss.fixedPtmDefs();
		PtmDefinition[] allPtms = Arrays.copyOf(varPtms, varPtms.length+fixedPtms.length);
		System.arraycopy(fixedPtms, 0, allPtms, varPtms.length, fixedPtms.length);

		//Parse ResultSet data : queries to protein matches
		logger.info("Parse and create ResultSet MS Data ");
		MSDataReader dataReader = new MSDataReader(m_resultFolder, m_parserContext);
		ResultSetsDataMapper rsMapper= dataReader.parseMSData2ResulSets(rsIdByName, allPtms);
		
		logger.info("Create Experiment properties ");
		List<ResultSet> resultSets = propReader.parseAndCreateResultSetParams(rsMapper);
				
		
		logger.info("Storing ResultSets");
		storeResultSets(resultSets, instrumentConfigOpt.get()); 
		
		return true;
			
	}
	
	private List<Long> storeResultSets(List<ResultSet> resultSets, InstrumentConfig instrumentConfig){
		ArrayList<Long> rsIds = new ArrayList<>();
		Boolean localTransaction = false;
		Boolean msiTransactionOK  = false;
		DatabaseConnectionContext msiDbContext = m_parserContext.getMSIDbConnectionContext();
		IRsStorer rsStorer = RsStorer.apply(msiDbContext);
		StorerContext storerContext = null;
		
		try {
			if(!msiDbContext.isInTransaction()){
				msiDbContext.beginTransaction();
				localTransaction = true;		
			}
					  
			storerContext = StorerContext.apply(m_parserContext);
			SQLMsiSearchWriter.insertInstrumentConfig(instrumentConfig, storerContext);
			
			
			for(ResultSet nextRS : resultSets){
				Long rsId = storeResultFile(nextRS, storerContext, rsStorer);
				rsIds.add(rsId);
			}
			
			
			// Commit transaction if it was initiated locally
			if (localTransaction) {
				msiDbContext.commitTransaction();
			}

			msiTransactionOK = true;
		}catch(Exception e){
			throw new RuntimeException(e);
		} finally {
			if (storerContext != null) {
	          storerContext.clearContext();
			}

			if (localTransaction && !msiTransactionOK) {
				logger.info("Rollbacking MSI Db Transaction");

				try {
		            msiDbContext.rollbackTransaction();
				} catch (Exception ex){
					logger.error("Error rollbacking MSI Db Transaction", ex);
				}

        	}
		}
		
		return rsIds;		
	}

	private Long storeResultFile(ResultSet nextRS, StorerContext storerContext,IRsStorer rsStorer) {
		logger.info("-- Storing  ResultSet "+nextRS.name());
		if(nextRS.peptideMatches() == null || nextRS.peptideMatches().length <= 0)
			throw new RuntimeException(nextRS.name()+ " ResultSet has NO PeptideMatch");
		if(nextRS.proteinMatches() == null || nextRS.proteinMatches().length <= 0)
			throw new RuntimeException(nextRS.name()+ " ResultSet has NO ProteinMatch");
			
		IPeaklistWriter pklWriter =	rsStorer.getOrBuildPeaklistWriter(storerContext);
		
		// Insert the peaklist information
		Peaklist pl = nextRS.msiSearch().get().peakList();
	   	long peakListId = pklWriter.insertPeaklist(pl, storerContext);
	   	pl.id_$eq(peakListId);

	   	//TODO Create Map Query -> List peptideMatch
	   	Map<MsQuery,List<PeptideMatch>> pepMatchesByQuery = null;
	   	// Insert spectra contained in result file
	   	//TODO spectraById
	   	Map<Long, Spectrum> spectraById = null;
	   	MQPeaklistContainer plConatiner = new MQPeaklistContainer(nextRS, spectraById); 
	    logger.info("Storing spectra...");
	    pklWriter.insertSpectra(peakListId, plConatiner, storerContext);
	    
	   	//TODO : Compute PrettyRank computePrettyRanks(rs.peptideMatches, separated = true)
//	    rsStorer.storeResultSet(nextRS, pepMatchesByQuery.keySet(), storerContext);
	    rsStorer.storeResultSet(nextRS, storerContext);
	    
		return null;
	}
}
