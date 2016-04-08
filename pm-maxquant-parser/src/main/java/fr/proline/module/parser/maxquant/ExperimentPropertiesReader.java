package fr.proline.module.parser.maxquant;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.apache.commons.io.FilenameUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import fr.profi.util.serialization.ProfiJson;
import fr.proline.context.MsiDbConnectionContext;
import fr.proline.core.dal.tables.msi.MsiDbPeaklistSoftwareColumns;
import fr.proline.core.om.model.msi.Enzyme;
import fr.proline.core.om.model.msi.InstrumentConfig;
import fr.proline.core.om.model.msi.MSISearch;
import fr.proline.core.om.model.msi.MSMSSearchSettings;
import fr.proline.core.om.model.msi.Peaklist;
import fr.proline.core.om.model.msi.PeaklistSoftware;
import fr.proline.core.om.model.msi.Peptide;
import fr.proline.core.om.model.msi.PeptideMatch;
import fr.proline.core.om.model.msi.ProteinMatch;
import fr.proline.core.om.model.msi.PtmDefinition;
import fr.proline.core.om.model.msi.ResultSet;
import fr.proline.core.om.model.msi.ResultSetProperties;
import fr.proline.core.om.model.msi.SearchSettings;
import fr.proline.core.om.model.msi.SeqDatabase;
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext;
import fr.proline.core.om.provider.msi.ISeqDatabaseProvider;
import fr.proline.module.parser.maxquant.model.MaxQuantParams;
import fr.proline.module.parser.maxquant.model.MsMsParameters;
import fr.proline.module.parser.maxquant.model.ParameterGroup;
import fr.proline.module.parser.maxquant.model.ResultSetsDataMapper;
import fr.proline.repository.util.JDBCWork;
import scala.Option;



public class ExperimentPropertiesReader {

	protected static Logger logger = LoggerFactory.getLogger(ExperimentPropertiesReader.class);
	
	protected final static String MQ_PROP_FILENAME = "mqpar.xml";

	//Alternate properties files
	protected final static String SUMMARY_FILENAME = "combined/txt/summary.txt";
	protected final static String EXP_DESIGN_FILENAME = "combined/experimentalDesignTemplate.txt";
	protected final static String PARAMETER_FILENAME = "combined/txt/parameters.txt";

	//Summary file header
	protected final static String NAME_HEADER = "Name";
	protected final static String RAW_FILE_HEADER = "Raw file";
	protected final static String EXPERIMENT_HEADER = "Experiment";
	protected final static String ENZYME_HEADER = "Enzyme";
	protected final static String VARS_MOD_HEADER = "Variable modifications";
	//  val FIXED_MOD_HEADER = "Variable modifications";
	protected final static String MISSED_CLEAVAGES_HEADER = "Max. missed cleavages";
	protected final static String MSMS_HEADER = "MS/MS";
	protected final static String MSMS_SUBMITTED_HEADER = "MS/MS Submitted";
	protected final static String PEP_SEQ_IDENTIFIED_HEADER = "Peptide Sequences Identified";

	//Parameters Key
	protected final static String FASTA_FILES_PARAM_KEY = "Fasta file";
	protected final static String INCLUDE_CONTAM_PARAM_KEY = "Include contaminants";
	protected final static String VERSION_PARAM_KEY = "Version";

	protected static String[] headerOfInterest = {
			EXPERIMENT_HEADER,
			ENZYME_HEADER,
			VARS_MOD_HEADER,
			MISSED_CLEAVAGES_HEADER,
			MSMS_HEADER,
			MSMS_SUBMITTED_HEADER,
			PEP_SEQ_IDENTIFIED_HEADER
	};
	
	private ProviderDecoratedExecutionContext m_parserContext;	
	private InstrumentConfig m_instrumentConfig;
	private Long m_peaklistSoftwareId;
	private String m_mqResultFileFolder;
	private Map<String, Long>  m_rsIdByName;
	private MaxQuantParams m_mqParams = null;
	private SearchSettings m_globalSearchSettings;
	
	public ExperimentPropertiesReader(String mqFolder, ProviderDecoratedExecutionContext parserContext, InstrumentConfig instrumentConfig, Long peaklistSoftwareId) {
		m_parserContext = parserContext;
		m_instrumentConfig = instrumentConfig;
		m_peaklistSoftwareId = peaklistSoftwareId;
		m_mqResultFileFolder = mqFolder;
		File paramFile = new File(m_mqResultFileFolder,MQ_PROP_FILENAME);
		try {
			JAXBContext context = JAXBContext.newInstance(MaxQuantParams.class);
			Unmarshaller unmarshaller = context.createUnmarshaller();
			
			FileInputStream is = new FileInputStream(paramFile);
			m_mqParams = (MaxQuantParams) unmarshaller.unmarshal(is);
		} catch (JAXBException e) {
			throw new RuntimeException(" Error reading parameters file : "+e.getMessage());
		} catch (IOException ioe) {
			throw new RuntimeException(" Error reading parameters file : "+ioe.getMessage());
		}

		
	}

	public Map<String, Long> getResultSetIds(){
		if(m_rsIdByName == null){
			initResultSetIdByName();
		}
		
		return m_rsIdByName;
	}
	
	public SearchSettings getSearchSettings(){
		if(m_globalSearchSettings == null)
			initSearchSettings(m_mqParams);	
		return m_globalSearchSettings;
	}
	
	private void initResultSetIdByName(){
		m_rsIdByName = new HashMap<String, Long>();
		for(String nextFilePath : m_mqParams.getFilePaths()){
			String rsName = FilenameUtils.getBaseName(nextFilePath);
			Long rsId = ResultSet.generateNewId();
			m_rsIdByName.put(rsName, rsId);
		}
	}
	
	public List<ResultSet> parseAndCreateResultSetParams(ResultSetsDataMapper rsMapper) {
		List<ResultSet> resultSets = new ArrayList<>();

		File paramFile = new File(m_mqResultFileFolder,MQ_PROP_FILENAME);
		Date creationDate = new Date( paramFile.lastModified());		

		PeaklistSoftware peaklistSoftware = getOrCreatePeaklistSoftware();
		if(m_globalSearchSettings == null)
			initSearchSettings(m_mqParams);
		
		if(m_rsIdByName == null)
			initResultSetIdByName();
		
		for(String nextFilePath : m_mqParams.getFilePaths()){
			ResultSetProperties rsProp = new ResultSetProperties(Option.apply("SEPARATED"), null, null);
			MSISearch nextMSI = createMSISearch(nextFilePath, creationDate, peaklistSoftware );
			
			String rsName = FilenameUtils.getBaseName(nextFilePath);
			Long rsId = m_rsIdByName.get(rsName);
			
			List<Peptide> pepList = rsMapper.getPeptidesForRs(rsName);
			List<PeptideMatch> pepMList = rsMapper.getPeptideMatchesForRs(rsName);
			List<ProteinMatch> protMList = rsMapper.getProteinMatchesForRs(rsName);
			resultSets.add(new ResultSet(pepList.toArray(new Peptide[pepList.size()]),
										pepMList.toArray(new PeptideMatch[pepMList.size()]), 
										protMList.toArray(new ProteinMatch[protMList.size()]), 
										false, //isDecoy
										true, //isSearchResult
										true,//isValidatedContent 
										0, //mergedResultSummaryId
										rsId, rsName,//RS Name
										"", //RS Description
										false, //isQuantified
										nextMSI.id(), Option.apply(nextMSI), new MSISearch[0],
										0, null, //DecoyRS
										Option.apply(rsProp)));
			
		}
		
		return resultSets;
	}
	
	
	
	private void initSearchSettings(MaxQuantParams mqParams) {
		logger.info("Parse MaxQuant Search Settings information ...");		
		ParameterGroup pg = mqParams.getParameters().get(0);
		
//		IPTMProvider ptmProvider = m_parserContext.getProvider(IPTMProvider.class);
		
		//---- Create SeqDatabase List
		List<SeqDatabase> seqDbsList = new ArrayList<>();
		Date now = new Date();
		if(mqParams.getIncludeContaminants()) {
			SimpleDateFormat sdf = new SimpleDateFormat("YYYY_MM_dd");			
			String fileName = "contaminants_"+sdf.format(now);
			seqDbsList.add(new SeqDatabase(SeqDatabase.generateNewId(),
											fileName, // name
											"MaxQuant", // file Path
											0, //nbr seq
											now,  // release date 
											"Unknown", //version
											0, //nbr searched seq
											null,  // SeqDatabaseProperties
											null)); // SeqDatabaseSearchProperties   		        
		}
		
		List<String> fastaFileNames = mqParams.getFilePaths();
		for(String fileName : fastaFileNames){
			File seqFile = new File(fileName);
			ISeqDatabaseProvider seqDbProvider= m_parserContext.getProvider(ISeqDatabaseProvider.class);
			Option<SeqDatabase> seqDBOpt = seqDbProvider.getSeqDatabase(seqFile.getName(), seqFile.getPath());
			SeqDatabase seqDB = null;
			if(seqDBOpt.isEmpty()){
				//Create new one
				seqDB = new SeqDatabase(SeqDatabase.generateNewId(),
										fileName,// name
										seqFile.getPath(), // file Path
										0, //nbr seq
										now,  // release date 
										"Unknown", //version 
										0, //nbr searched seq
										null, // SeqDatabaseProperties
										null); // SeqDatabaseSearchProperties
			} else 
				seqDB = seqDBOpt.get();
			seqDbsList.add(seqDB);
		}

			
		//---- Create Enzyme List
		List<String> enzymeNames = pg.getEnzymes();
		Enzyme[] enzymes = new Enzyme[enzymeNames.size()];
		for(int i=0; i<enzymeNames.size();i++){
			enzymes[i] = new Enzyme(enzymeNames.get(i));
		}
		
		 //-- Retrieve the instrument configuration
		String instrTypeName = "Unknown";
		instrTypeName = m_instrumentConfig.msnAnalyzer();		
		
		
		Double ms2ErrorTol = 0.0d;
		String ms2ErrorTolUnit = "";
		for(MsMsParameters msmsParam : mqParams.getMsMsParameters()){
			if(instrTypeName.equalsIgnoreCase(msmsParam.getInstrumTypeName())){ 
				//TODO : MAP "4SECTOR", "FTMS", "TOF",  "FTICR","ISD", "QIT", "QUAD", "TRAP" with FTMS, ITMS, TOF ... or Unknown 

				ms2ErrorTol = msmsParam.getMatchTolerance().doubleValue();
				ms2ErrorTolUnit = msmsParam.getMsmsToleranceInPpm()? "ppm" : "Da";
				break;				
			}				
		}
		
		//TODO Create PTMDef for var & fixed
		PtmDefinition[] varPtms = new PtmDefinition[0];
		PtmDefinition[] fixedPtms = new PtmDefinition[0];
		
		String tolUnit = pg.getSearchTolInPpm() ? "ppm" : "Da";

		//---- Create MSMSSearchSetting specific properties
		MSMSSearchSettings ssProp = new MSMSSearchSettings("", ms2ErrorTol, ms2ErrorTolUnit);
		
		m_globalSearchSettings = new SearchSettings(
			SearchSettings.generateNewId(),
			"MaxQuant", mqParams.getVersion(),//software name & version 
			"", //taxo
			pg.getMaxMissedCleavages()," max charge "+pg.getMaxCharge(), 
			pg.getMainSearchTol().doubleValue(), tolUnit, //MS1ErrorTol
			false,  //isDecoy
			enzymes, 
			varPtms, fixedPtms, 
			seqDbsList.toArray(new SeqDatabase[seqDbsList.size()]),
			m_instrumentConfig, Option.apply(ssProp), 
			null, // PMFSearchSettings
			null);//SearchSettingsProperties
					
	}

	private MSISearch createMSISearch(String rawFileName, Date creationDate, PeaklistSoftware peaklistSoftware) {
		logger.info("Parse MaxQuant MSISearch information for "+rawFileName+"...");
				
		
		//TODO fileType : get raw file type (wiff, mzXml, raw...)
		Peaklist peaklist=  new Peaklist( Peaklist.generateNewId(), //id
				"MaxQuant RAW FILE", // fileType
				rawFileName, // path
				"", //rawFileIdentifier
				2, // msLevel
				"none", //spectrumDataCompression
				peaklistSoftware,// peaklistSoftware
				null);//PeaklistProperties
		
			//TODO User Name could be read from parameters.txt 
			//TODO queriesCount could be read from summary.txt
		 MSISearch msiSearch =	 new MSISearch
			 (MSISearch.generateNewId(),  //id
			 	"msms.txt",  //resultFileName
		 		m_globalSearchSettings, 
		 		peaklist, 
		 		creationDate, 
		 		m_mqParams.getName(), //title
		 		m_mqResultFileFolder, //resultFileDirectory
		 		-1, //jobNumber
		 		"proline", //userName
		 		"", //userEmail
		 		-1, //queriesCount
		 		0, //searchedSequencesCount
		 		null);//properties
		 
		return msiSearch;
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
					sb.append(pklSoft.name()).append(",");
					sb.append(pklSoft.version()).append(",");
					sb.append(properties).append(")");
					Statement stmt = connection.createStatement();
					stmt.executeQuery(sb.toString());					
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
}

