package fr.proline.module.parser.maxquant;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.apache.commons.io.FilenameUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.opencsv.CSVReader;

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
import fr.proline.core.om.provider.msi.IPTMProvider;
import fr.proline.core.om.provider.msi.ISeqDatabaseProvider;
import fr.proline.module.parser.maxquant.model.IMaxQuantParams;
import fr.proline.module.parser.maxquant.model.IMsMsParameters;
import fr.proline.module.parser.maxquant.model.ParameterGroup;
import fr.proline.module.parser.maxquant.model.ResultSetsDataMapper;

import scala.Option;



public class ExperimentPropertiesReader {

	protected static Logger logger = LoggerFactory.getLogger(ExperimentPropertiesReader.class);
	
	protected final static String MQ_PROP_FILENAME = "mqpar.xml";

	//Alternate properties files
	protected final static String SUMMARY_FILENAME = "combined/txt/summary.txt";
	protected final static String MSMSSCAN_FILENAME = "combined/txt/msmsScans.txt";
//	protected final static String EXP_DESIGN_FILENAME = "combined/experimentalDesignTemplate.txt";
	protected final static String PARAMETER_FILENAME = "combined/txt/parameters.txt";
	
	//MSMSScan file header
	protected final static String ANALYZER_HEADER = "Mass analyzer";
	
	//Summary file header
//	protected final static String NAME_HEADER = "Name";
	protected final static String RAW_FILE_HEADER = "Raw file";
//	protected final static String EXPERIMENT_HEADER = "Experiment";
//	protected final static String ENZYME_HEADER = "Enzyme";
//	protected final static String VARS_MOD_HEADER = "Variable modifications";
	//  val FIXED_MOD_HEADER = "Variable modifications";
//	protected final static String MISSED_CLEAVAGES_HEADER = "Max. missed cleavages";
//	protected final static String MSMS_HEADER = "MS/MS";
	protected final static String MSMS_SUBMITTED_HEADER = "MS/MS Submitted";
//	protected final static String PEP_SEQ_IDENTIFIED_HEADER = "Peptide Sequences Identified";

//	//Parameters Key
//	protected final static String FASTA_FILES_PARAM_KEY = "Fasta file";
//	protected final static String INCLUDE_CONTAM_PARAM_KEY = "Include contaminants";
//	protected final static String VERSION_PARAM_KEY = "Version";
//
	protected final static String UNKNOWN_ANALYZER = "Unknown";
	
//	protected static String[] headerOfInterest = {
//			EXPERIMENT_HEADER,
//			ENZYME_HEADER,
//			VARS_MOD_HEADER,
//			MISSED_CLEAVAGES_HEADER,
//			MSMS_HEADER,
//			MSMS_SUBMITTED_HEADER,
//			PEP_SEQ_IDENTIFIED_HEADER
//	};
	
	private ISeqDatabaseProvider m_seqDbProvider;	
	private IPTMProvider m_ptmProvider;
	private InstrumentConfig m_instrumentConfig;
	private PeaklistSoftware m_peaklistSoftware;
	private File m_mqResultFileFolder;
	private Map<String, Long>  m_rsIdByName;
	private IMaxQuantParams m_mqParams = null;
	private SearchSettings m_globalSearchSettings;
	
	public ExperimentPropertiesReader(URL folderURL, ISeqDatabaseProvider seqDbProvider, IPTMProvider ptmProvider, InstrumentConfig instrumentConfig, PeaklistSoftware peaklistSoftware) {
		m_seqDbProvider = seqDbProvider;
		m_ptmProvider = ptmProvider;
		m_instrumentConfig = instrumentConfig;
		m_peaklistSoftware = peaklistSoftware;
		try {
			m_mqResultFileFolder = new File(folderURL.toURI());
			initReader();
		} catch (URISyntaxException uriE) {
			throw new RuntimeException(" Error reading parameters file : "+uriE.getMessage());
		}
	
	}

	
	public ExperimentPropertiesReader(String mqFolder, ISeqDatabaseProvider seqDbProvider, IPTMProvider ptmProvider, InstrumentConfig instrumentConfig, PeaklistSoftware peaklistSoftware) {
		m_seqDbProvider = seqDbProvider;
		m_ptmProvider = ptmProvider;
		m_instrumentConfig = instrumentConfig;
		m_peaklistSoftware = peaklistSoftware;

		m_mqResultFileFolder = new File(mqFolder);
		initReader();
			
	
	}
	
	private void initReader(){
		try {			

			//Try to get MaxQuant Result
			File parametersFile = new File(m_mqResultFileFolder,PARAMETER_FILENAME);
			String mqVersion = readVersion(parametersFile);
			File paramFile = new File(m_mqResultFileFolder,MQ_PROP_FILENAME);
			
			if(mqVersion.startsWith("1.5")){					
				JAXBContext context = JAXBContext.newInstance(fr.proline.module.parser.maxquant.model.v1_5.MaxQuantParams.class);
				Unmarshaller unmarshaller = context.createUnmarshaller();			
				FileInputStream is = new FileInputStream(paramFile);				
				m_mqParams = (IMaxQuantParams) unmarshaller.unmarshal(is);
			}  else {
				JAXBContext context = JAXBContext.newInstance(fr.proline.module.parser.maxquant.model.v1_4.MaxQuantParams.class);
				Unmarshaller unmarshaller = context.createUnmarshaller();			
				FileInputStream is = new FileInputStream(paramFile);				
				m_mqParams = (IMaxQuantParams) unmarshaller.unmarshal(is);
				((fr.proline.module.parser.maxquant.model.v1_4.MaxQuantParams)m_mqParams).setVersion(mqVersion);
			}
			
			
		} catch (JAXBException e) {
			throw new RuntimeException(" Error reading parameters file : "+e.getMessage());
		} catch (IOException ioe) {
			throw new RuntimeException(" Error reading parameters file : "+ioe.getMessage());
		} 
	}

	
	private String readVersion(File paramFile) {
		if(!paramFile.exists()) throw new RuntimeException( "No parameter file defined. Can't load MQ result");
		CSVReader paramFileCSVReader =null;
		String version = null;
		try {
			paramFileCSVReader = new CSVReader(new FileReader(paramFile), '\t');
			paramFileCSVReader.readNext(); //Skip header
			String[] nextLine = paramFileCSVReader.readNext();			
			while (nextLine != null) {
				String paramName = nextLine[0];
				if(paramName.equals("Version")){
					version =  nextLine[1];
					break;
				}
			}
		} catch (IOException e) {
			if(paramFileCSVReader != null){
				try {
					paramFileCSVReader.close();
				} catch (IOException e1) {					
					logger.info("Error closing reader "+e1.getMessage());
				}	
			}
		}
		
		return version;
	}
	
	public Map<String, Long> getResultSetIds(){
		if(m_rsIdByName == null){
			initResultSetIdByName();
		}
		
		return m_rsIdByName;
	}
	
	public SearchSettings getSearchSettings(){
		if(m_globalSearchSettings == null)
			initSearchSettings();	
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

		if(m_globalSearchSettings == null)
			initSearchSettings();
		
		if(m_rsIdByName == null)
			initResultSetIdByName();
		
		Map<String, Integer> qCountByRS = getQueriesCount();
		
		for(String nextFilePath : m_mqParams.getFilePaths()){
			ResultSetProperties rsProp = new ResultSetProperties(Option.apply("SEPARATED"), null, null);
			MSISearch nextMSI = createMSISearch(nextFilePath, creationDate, m_peaklistSoftware, qCountByRS );
			
			String rsName = FilenameUtils.getBaseName(nextFilePath);
			Long rsId = m_rsIdByName.get(rsName);
			
			List<Peptide> pepList = rsMapper.getPeptidesForRs(rsName);
			List<PeptideMatch> pepMList = rsMapper.getPeptideMatchesForRs(rsName);
			List<ProteinMatch> protMList = new ArrayList<ProteinMatch>();
			rsMapper.getProteinMatchesForRs(rsName).values().forEach(prots4PepM -> {
							prots4PepM.forEach(protMatch -> {if(!protMList.contains(protMatch)) protMList.add(protMatch); });
						});
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
										0, Option.empty(), //DecoyRS
										Option.apply(rsProp)));
			
		}
		
		return resultSets;
	}
	
	
	
	private void initSearchSettings() {
		logger.info("Parse MaxQuant Search Settings information ...");		
		ParameterGroup pg = m_mqParams.getParameters().get(0);
		
		
		//---- Create SeqDatabase List
		List<SeqDatabase> seqDbsList = new ArrayList<>();
		Date now = new Date();
		if(m_mqParams.getIncludeContaminants()) {
			SimpleDateFormat sdf = new SimpleDateFormat("YYYY_MM_dd");			
			String fileName = "contaminants_"+sdf.format(now);
			seqDbsList.add(new SeqDatabase(SeqDatabase.generateNewId(),
											fileName, // name
											"MaxQuant", // file Path
											0, //nbr seq
											now,  // release date 
											"Unknown", //version
											0, //nbr searched seq
											Option.empty(),  // SeqDatabaseProperties
											Option.empty())); // SeqDatabaseSearchProperties   		        
		}
		
		List<String> fastaFileNames = m_mqParams.getFastaFiles();
		for(String fileName : fastaFileNames){
			File seqFile = new File(fileName);			
			Option<SeqDatabase> seqDBOpt = m_seqDbProvider.getSeqDatabase(seqFile.getName(), seqFile.getPath());
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
										Option.empty(), // SeqDatabaseProperties
										Option.empty()); // SeqDatabaseSearchProperties
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
		String instrTypeName =getAnalyzerName();
				
		Double ms2ErrorTol = 0.0d;
		String ms2ErrorTolUnit = "";

		for(IMsMsParameters msmsParam : m_mqParams.getMsMsParameters()){
			if(instrTypeName.equalsIgnoreCase(msmsParam.getInstrumTypeName())){ 
				ms2ErrorTol = msmsParam.getMatchTolerance().doubleValue();
				ms2ErrorTolUnit = msmsParam.getMsmsToleranceInPpm()? "ppm" : "Da";
				break;				
			}				
		}
		
		List<String> fixedModifs = m_mqParams.getFixedModifications();
		List<PtmDefinition> fixedPtms = new ArrayList<PtmDefinition>();		
		fixedModifs.forEach(item -> fixedPtms.addAll(PTMUtils.parsePTMString(m_ptmProvider, item)) );
		
		List<String> varModifs = pg.getVariableModifications();
		List<PtmDefinition> varPtms = new ArrayList<PtmDefinition>();	
		varModifs.forEach(item -> varPtms.addAll(PTMUtils.parsePTMString(m_ptmProvider, item)) );
		varPtms.toArray();
		
		String tolUnit = pg.getSearchTolInPpm() ? "ppm" : "Da";

		//---- Create MSMSSearchSetting specific properties
		MSMSSearchSettings ssProp = new MSMSSearchSettings(null, //ms2Charge
												ms2ErrorTol, ms2ErrorTolUnit);
		
		m_globalSearchSettings = new SearchSettings(
			SearchSettings.generateNewId(),
			"MaxQuant", m_mqParams.getVersion(),//software name & version 
			"", //taxo
			pg.getMaxMissedCleavages(),"max charge "+pg.getMaxCharge(), 
			pg.getMainSearchTol().doubleValue(), tolUnit, //MS1ErrorTol
			false,  //isDecoy
			enzymes, 
			varPtms.toArray(new PtmDefinition[varPtms.size()]), fixedPtms.toArray(new PtmDefinition[fixedPtms.size()]), 
			seqDbsList.toArray(new SeqDatabase[seqDbsList.size()]),
			m_instrumentConfig, Option.apply(ssProp), 
			Option.empty(), // PMFSearchSettings
			Option.empty());//SearchSettingsProperties
					
	}

	private String getAnalyzerName(){
		File msmsScanFile = new File(m_mqResultFileFolder, MSMSSCAN_FILENAME);
		String analyzer = UNKNOWN_ANALYZER;
		CSVReader msmsScanCSVReader  = null;
		try {
			msmsScanCSVReader= new CSVReader(new FileReader(msmsScanFile),'\t');
			//Get Analyzer col index
			String[] headers = msmsScanCSVReader.readNext();
			Integer analyzerIndex = -1;			
			for(int i=0; i<headers.length; i++){
				if(headers[i].equals(ANALYZER_HEADER)){
					analyzerIndex = i;
					break;
				}
			}
			
			//Get first row value for this column
			String[] nextRow = msmsScanCSVReader.readNext();
			analyzer = nextRow[analyzerIndex];
			msmsScanCSVReader.close();
		} catch (IOException e) {
			if(msmsScanCSVReader != null){
				try {
					msmsScanCSVReader.close();
				} catch (IOException e1) {					
					logger.info("Error closing reader "+e1.getMessage());
				}	
			}
				
			throw new RuntimeException("Error reading MSMSScan file.", e);
		}
		
		return analyzer;		
	}
	
	private MSISearch createMSISearch(String rawFileName, Date creationDate, PeaklistSoftware peaklistSoftware, Map<String, Integer> qCountByRS ) {
		logger.info("Parse MaxQuant MSISearch information for "+rawFileName+"...");
				
		
		//TODO fileType : get raw file type (wiff, mzXml, raw...)
		Peaklist peaklist=  new Peaklist( Peaklist.generateNewId(), //id
				"MaxQuant RAW FILE", // fileType
				rawFileName, // path
				"", //rawFileIdentifier
				2, // msLevel
				"none", //spectrumDataCompression
				peaklistSoftware,// peaklistSoftware
				Option.empty());//PeaklistProperties
		
			//TODO User Name could be read from parameters.txt 
		 MSISearch msiSearch =	 new MSISearch
			 (MSISearch.generateNewId(),  //id
			 	"msms.txt",  //resultFileName
		 		m_globalSearchSettings, 
		 		peaklist, 
		 		creationDate, 
		 		m_mqParams.getName(), //title
		 		m_mqResultFileFolder.getAbsolutePath(), //resultFileDirectory
		 		-1, //jobNumber
		 		"proline", //userName
		 		"", //userEmail
		 		qCountByRS.get(FilenameUtils.getBaseName(rawFileName)), //queriesCount
		 		0, //searchedSequencesCount
		 		Option.empty());//properties
		 
		return msiSearch;
	}
	
	private Map<String, Integer>  getQueriesCount(){
		File summFile = new File(m_mqResultFileFolder, SUMMARY_FILENAME);
		CSVReader reader  = null;
		Map<String, Integer> queriesCountByRS = new HashMap<>();
		try {
			reader= new CSVReader(new FileReader(summFile), '\t');
			//Get Submitted Queries count index
			String[] headers = reader.readNext();
			Integer rawFileIndex = -1;
			Integer queriesCountIndex = -1;
			for(int i=0; i<headers.length; i++){
				if(headers[i].equals(RAW_FILE_HEADER)){
					rawFileIndex = i;
				}
				if(headers[i].equals(MSMS_SUBMITTED_HEADER)){
					queriesCountIndex = i;
				}
			}
			
			//read value for each RawFile
			Set<String> rawFileNames =getResultSetIds().keySet();			
			String[] nextRow = reader.readNext();
			
			while(nextRow != null && queriesCountByRS.size()<rawFileNames.size()){
				if(rawFileNames.contains(nextRow[rawFileIndex])){ //Found a Raw File
					queriesCountByRS.put(nextRow[rawFileIndex], Integer.valueOf(nextRow[queriesCountIndex]));
				}				
				nextRow = reader.readNext();
			}
			
			reader.close();
		} catch (IOException e) {
			if(reader != null){
				try {
					reader.close();
				} catch (IOException e1) {					
					logger.info("Error closing reader "+e1.getMessage());
				}	
			}
				
			throw new RuntimeException("Error reading MSMSScan file.", e);
		}
		
		return queriesCountByRS;	
	}

	
}

