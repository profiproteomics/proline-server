package fr.proline.module.parser.maxquant;

import fr.profi.api.service.IServiceWrapper;
import fr.profi.util.StringUtils;
import fr.profi.util.serialization.ProfiJson;
import fr.proline.context.DatabaseConnectionContext;
import fr.proline.context.MsiDbConnectionContext;
import fr.proline.core.dal.tables.msi.MsiDbPeaklistSoftwareColumns;
import fr.proline.core.om.model.lcms.*;
import fr.proline.core.om.model.msi.*;
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext;
import fr.proline.core.om.provider.msi.IPTMProvider;
import fr.proline.core.om.provider.msi.ISeqDatabaseProvider;
import fr.proline.core.om.provider.msi.impl.SQLFragmentationRuleProvider;
import fr.proline.core.om.provider.msi.impl.SQLInstrumentConfigProvider;
import fr.proline.core.om.storer.msi.IPeaklistWriter;
import fr.proline.core.om.storer.msi.IRsStorer;
import fr.proline.core.om.storer.msi.RsStorer;
import fr.proline.core.om.storer.msi.impl.SQLMsiSearchWriter;
import fr.proline.core.om.storer.msi.impl.StorerContext;
import fr.proline.module.parser.maxquant.model.MQPeaklistContainer;
import fr.proline.module.parser.maxquant.model.ResultSetsDataMapper;
import fr.proline.repository.util.JDBCWork;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.Option;
import scala.collection.JavaConversions;

import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class MaxQuantResultParser extends IServiceWrapper {
	
	private static Logger logger = LoggerFactory.getLogger(MaxQuantResultParser.class);
	
	private ProviderDecoratedExecutionContext m_parserContext;
	private Long m_instrumConfigId;
	private Long m_fragmentationRuleSetId;
	private Long m_peaklistSoftwareId;
	private String m_resultFolder;
	private String m_accessionRegexp;

	//Created value to return !
	private StringBuffer m_warningMsg;	
	private Map<String, Long> m_rsIdByName;
	private Map<String, Peptide> m_peptideByModSequence;
	
	
	public MaxQuantResultParser(ProviderDecoratedExecutionContext parserContext, Long instrumConfigId, Long peaklistSoftwareId, String resultFolder ){
		this(parserContext,instrumConfigId, peaklistSoftwareId, "*", resultFolder, -1l);
	}

	public MaxQuantResultParser(ProviderDecoratedExecutionContext parserContext, Long instrumConfigId, String accessionRegexp, String resultFolder , Long fragmentationRuleSetId ){
		this(parserContext, instrumConfigId, getMaxQuantPeakListSoftwareId(parserContext), accessionRegexp, resultFolder,fragmentationRuleSetId);
	}

	public MaxQuantResultParser(ProviderDecoratedExecutionContext parserContext, Long instrumConfigId, Long peaklistSoftwareId, String accessionRegexp, String resultFolder, Long fragmentationRuleSetId ){
		m_parserContext = parserContext;
		m_instrumConfigId = instrumConfigId;
		m_fragmentationRuleSetId = fragmentationRuleSetId;
		m_peaklistSoftwareId = peaklistSoftwareId;
		m_resultFolder = resultFolder;
		m_accessionRegexp = accessionRegexp;
	}



	@Override
	public boolean runService() {
		
		logger.info("Running MaxQuant Parser on "+m_resultFolder);
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

			// Retrieve the fragmentation rule set  configuration
			Option<FragmentationRuleSet>  fragRuleSetOpt = Option.empty();
			if(m_fragmentationRuleSetId >0) {
				SQLFragmentationRuleProvider fragmentationRulesetProvider = new SQLFragmentationRuleProvider(m_parserContext.getUDSDbConnectionContext());
				fragRuleSetOpt = fragmentationRulesetProvider.getFragmentationRuleSet(m_fragmentationRuleSetId);
			}


			PeaklistSoftware peaklistSoft = getOrCreatePeaklistSoftware();		
			
			//Parse Search Parameters and meta data
			logger.info("Parse Experiment properties ");
			ISeqDatabaseProvider seqDbProvider= m_parserContext.getProvider(ISeqDatabaseProvider.class);
			IPTMProvider ptmProvider= m_parserContext.getProvider(IPTMProvider.class);
			ExperimentPropertiesReader propReader = new ExperimentPropertiesReader(m_resultFolder, seqDbProvider, ptmProvider, instrumentConfigOpt.get(),fragRuleSetOpt, peaklistSoft);
			m_rsIdByName = propReader.getResultSetIds();		
			
			SearchSettings ss = propReader.getSearchSettings();
			PtmDefinition[] varPtms = ss.variablePtmDefs();
			PtmDefinition[] fixedPtms = ss.fixedPtmDefs();

			//Parse ResultSet data : queries to protein matches
			logger.info("Parse and create ResultSet MS Data ");
			MSDataReader dataReader = new MSDataReader(m_resultFolder, m_parserContext, m_fragmentationRuleSetId, peaklistSoft);
			ResultSetsDataMapper rsMapper= dataReader.parseMSData2ResulSets(m_rsIdByName, fixedPtms, m_accessionRegexp, m_warningMsg);
			m_peptideByModSequence = dataReader.getPeptidesByMQModifiedSequence();
			
			logger.info("Create Experiment properties ");
			List<ResultSet> resultSets = propReader.parseAndCreateResultSetParams(rsMapper);
								
			logger.info("Storing ResultSets");
			storerContext = StorerContext.apply(m_parserContext);
			
			m_rsIdByName = storeResultSets(resultSets, instrumentConfigOpt.get(), rsMapper, storerContext);
			
			logger.info("Storing ResultSets DONE "+m_rsIdByName.values());
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
	
	
	public MapSet buildMapSet(String name, Map<String, Long> runIdByrsName, Map<String, Long> pseudoScanIdByrsName) {
		
		PeptidesDataReader peptidesReader = new PeptidesDataReader(m_resultFolder, m_peptideByModSequence);
		Long start = System.currentTimeMillis();
		peptidesReader.parseQuantitationData(m_rsIdByName, m_warningMsg);
		logger.info("Peptides parsed in "+(System.currentTimeMillis() - start)+" ms");
		
		long mapSetId = MapSet.generateNewId();
		int mapNumber = 0;
		
		PeakPickingSoftware pps = new PeakPickingSoftware(-1L,"MaxQuant","","MaxQuant", Option.empty());

		List<ProcessedMap> processedMaps = new ArrayList<>();
        Map<String, List<Feature>> processedFeatureByIon = new HashMap<>();

		for (String rsName: m_rsIdByName.keySet()) {
			
			Long mapId = RawMap.generateNewId();
			List<Feature> features = peptidesReader.getFeaturesByRSName().get(rsName);
			List<Feature>  processedFeatures = new ArrayList<>(features.size());

			Map<String, List<Feature>> featuresByPeptideId = features.stream().collect(Collectors.groupingBy(f -> f.relations().peptideId()+"_"+f.charge(), Collectors.mapping((Feature f) -> f, Collectors.toList())));

			for (Map.Entry<String, List<Feature>> e : featuresByPeptideId.entrySet()) {
                List<Feature> peptideFeatures = e.getValue();

                for (Feature f : peptideFeatures) {
					f.relations().rawMapId_$eq(mapId);
					f.relations().firstScanId_$eq(pseudoScanIdByrsName.get(rsName));
					f.relations().lastScanId_$eq(pseudoScanIdByrsName.get(rsName));
					f.relations().apexScanId_$eq(pseudoScanIdByrsName.get(rsName));
				}

				if (peptideFeatures.size() == 1) {
                    Feature feature = peptideFeatures.get(0);
                    processedFeatures.add(feature);
                    MapUtils.insertOrUpdate(processedFeatureByIon, e.getKey(), feature);
                } else {
				    logger.debug("Build a cluster feature");
                    Feature feature = clusterizeFeatures(peptideFeatures, pseudoScanIdByrsName.get(rsName));
                    //f.relations().processedMapId_$eq(mapId);
                    processedFeatures.add(feature);
                    peptideFeatures.forEach(f -> f.isClusterized_$eq(true));
                    MapUtils.insertOrUpdate(processedFeatureByIon, e.getKey(), feature);
                }
			}


			RawMap map = new RawMap(
					mapId, 
					rsName, 
					true,
					new java.util.Date(), 
					features.toArray(new Feature[features.size()]),
					Option.empty(), 
					runIdByrsName.get(rsName), 
					pps,
					"",
					Option.empty(),
					Option.empty(),
					Option.empty());
			
			processedMaps.add(map.toProcessedMap(mapNumber++, mapSetId, processedFeatures.toArray(new Feature[processedFeatures.size()])));
			
		}
		
		List<Feature> masterFeatures = new ArrayList<>(processedFeatureByIon.size());

		Long masterMapId = ProcessedMap.generateNewId();
		for(List<Feature> childFt : processedFeatureByIon.values()) {
			Feature bestFt = childFt.stream().max((ft1,ft2) -> Double.compare(ft1.intensity(), ft2.intensity())).get();
			Feature masterFt = bestFt.toMasterFeature(Feature.generateNewId(), childFt.toArray(new Feature[childFt.size()]));
			masterFt.relations().peptideId_$eq(bestFt.relations().peptideId());
			masterFeatures.add(masterFt);
			if (childFt.size() > 2) {
				logger.info("More than 2 Features for this master Feature for peptideId: "+masterFt.relations().peptideId());
				for (Feature f : childFt) {
					logger.info("PeptideId: "+f.relations().peptideId() +" Feature "+f.apexIntensity()+ " proc map:"+f.relations().processedMapId()+", scan: "+f.relations().firstScanInitialId());
				}
				
				
			}
		}
		
		ProcessedMap masterMap = new ProcessedMap(
				masterMapId, 
				"masterMap", 
				true, 
				new java.util.Date(), 
				masterFeatures.toArray(new Feature[masterFeatures.size()]), 
				Option.empty(), 
				mapNumber++, 
				new java.util.Date(),
				true,
				false,
				mapSetId,
				null, // rawMapReferences
				"",
				Option.empty(),
				Option.empty(),
				false,
				1.0f,
				Option.empty(),
				Option.empty());
		
		return new MapSet(MapSet.generateNewId(), name, new java.util.Date(), processedMaps.toArray(new ProcessedMap[processedMaps.size()]), masterMap, 0, null, Option.empty());
	}


    private Feature clusterizeFeatures(List<Feature> peptideFeatures, Long pseudoScanId) {

	    peptideFeatures.sort((f1, f2) -> Float.compare(f1.elutionTime(), f2.elutionTime()));
        Feature prototype = peptideFeatures.get(0);
        Feature firstFt = peptideFeatures.get(0);
        Feature lastFt = peptideFeatures.get(peptideFeatures.size()-1);
        float intensity = (float)peptideFeatures.stream().mapToDouble(f -> f.apexIntensity()).sum();
        int ms2Count = peptideFeatures.stream().mapToInt(f -> f.ms2Count()).sum();
        int ms1Count = peptideFeatures.stream().mapToInt(f -> f.ms1Count()).sum();

        //create a cluster of features
        return new Feature(
                Feature.generateNewId(),
                prototype.moz(),
                prototype.charge(),
                prototype.elutionTime(),
                intensity,
                intensity,
                prototype.duration(),
                0.0f,
                ms1Count,
                ms2Count,
                false,
                Option.empty(),
                new FeatureRelations(
                        null,
                        1,
                        Option.empty(),
                        null,
                        firstFt.relations().firstScanInitialId(),
                        lastFt.relations().lastScanInitialId(),
                        prototype.relations().apexScanInitialId(), //TODO : expected value is apexScanInitialId,
						pseudoScanId,
						pseudoScanId,
						pseudoScanId,
                        0,
                        0,
                        0,
                        0,
                        0,
                        0,
                        -1,
                        prototype.relations().peptideId()
                ),
                null,
                peptideFeatures.toArray(new Feature[peptideFeatures.size()]),
                null,
                Option.empty(),
                Option.empty(),
                Option.empty(),
                false,
                2,
                Option.empty());
    }

    public Map<Long, String> getCreatedResultSetIds(){
		return m_rsIdByName.entrySet().stream().collect(Collectors.toMap(Map.Entry::getValue, Map.Entry::getKey));
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
	
	private Map<String,Long> storeResultSets(List<ResultSet> resultSets, InstrumentConfig instrumentConfig,ResultSetsDataMapper rsMapper, StorerContext storerContext){
		Map<String,Long> rsIdbyName = new HashMap<>();
		DatabaseConnectionContext msiDbContext = m_parserContext.getMSIDbConnectionContext();
		IRsStorer rsStorer = RsStorer.apply(msiDbContext,true);

		SQLMsiSearchWriter.insertInstrumentConfig(instrumentConfig, storerContext);
					
		for(ResultSet nextRS : resultSets){
			Long rsId = storeResultFile(nextRS, rsMapper, storerContext, rsStorer);
			rsIdbyName.put(nextRS.name(), rsId);
		}
			

		return rsIdbyName;		
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
	   	List<MsQuery> queries = new ArrayList<>();
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

		return rsStorer.storeResultSet(nextRS, JavaConversions.asScalaBuffer(queries).toList(), storerContext);
	}

	private static Long getMaxQuantPeakListSoftwareId(ProviderDecoratedExecutionContext parserContext) {
		fr.proline.core.om.provider.uds.impl.SQLPeaklistSoftwareProvider udsPklSoftProvider = new fr.proline.core.om.provider.uds.impl.SQLPeaklistSoftwareProvider(parserContext.getUDSDbConnectionContext());
		Option<PeaklistSoftware> udsPklSoftOpt = udsPklSoftProvider.getPeaklistSoftware("MaxQuant", null);
		if(udsPklSoftOpt.isEmpty())
			throw new RuntimeException("can't find a peaklist software for MaxQuant software");
		return udsPklSoftOpt.get().id();
	}
}
