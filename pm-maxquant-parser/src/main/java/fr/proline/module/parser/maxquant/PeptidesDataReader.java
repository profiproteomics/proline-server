package fr.proline.module.parser.maxquant;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.opencsv.CSVReader;

import fr.proline.core.om.model.lcms.Feature;
import fr.proline.core.om.model.lcms.FeatureRelations;
import fr.proline.core.om.model.msi.Peptide;
import fr.proline.module.parser.maxquant.model.ResultSetsDataMapper;
import scala.Option;

public class PeptidesDataReader {

	protected static Logger logger = LoggerFactory.getLogger(PeptidesDataReader.class);

	private final static String MQ_PEPDATA_FILENAME = "combined/txt/allPeptides.txt";
	private final static String MQ_EVIDENCE_DATA_FILENAME = "combined/txt/evidence.txt";

	//allPeptides file header
	private final static String RAW_FILE = "RAW FILE";
	private final static String CHARGE = "CHARGE";
	private final static String MZ = "M/Z";
	private final static String TYPE = "TYPE";
	private final static String NUMBER_OF_DATA_POINTS = "NUMBER OF DATA POINTS";
	private final static String NUMBER_OF_SCANS = "NUMBER OF SCANS";
	private final static String NUMBER_OF_ISOTOPIC_PEAKS = "NUMBER OF ISOTOPIC PEAKS";
	private final static String MAX_INTENSITY_MZ_0 = "MAX INTENSITY M/Z 0";
	private final static String RETENTION_TIME = "RETENTION TIME";
	private final static String RETENTION_LENGTH = "RETENTION LENGTH";
	private final static String MIN_SCAN_NUMBER = "MIN SCAN NUMBER";
	private final static String MAX_SCAN_NUMBER = "MAX SCAN NUMBER";
	private final static String IDENTIFIED = "IDENTIFIED";
	private final static String MSMS_IDS = "MS/MS IDS";
	private final static String SEQUENCE = "SEQUENCE";
	private final static String MODIFICATIONS = "MODIFICATIONS";
	private final static String MODIFIED_SEQUENCE = "MODIFIED SEQUENCE";
	private final static String INTENSITY = "INTENSITY";
	private final static String INTENSITIES = "INTENSITIES";
	private final static String MSMS_COUNT = "MS/MS COUNT";
	private final static String MSMS_SCAN_NUMBERS = "MSMS SCAN NUMBERS";

	private static String[] headersOfInterest = { RAW_FILE, CHARGE, MZ, NUMBER_OF_DATA_POINTS, NUMBER_OF_SCANS, NUMBER_OF_ISOTOPIC_PEAKS, MAX_INTENSITY_MZ_0, RETENTION_TIME, RETENTION_LENGTH,
			MIN_SCAN_NUMBER, MAX_SCAN_NUMBER, IDENTIFIED, MSMS_IDS, SEQUENCE, MODIFICATIONS, MODIFIED_SEQUENCE, INTENSITY, INTENSITIES, MSMS_COUNT, MSMS_SCAN_NUMBERS };

	private static String[] evidenceHeadersOfInterest = { MODIFIED_SEQUENCE, RAW_FILE, TYPE, MAX_INTENSITY_MZ_0, RETENTION_TIME, INTENSITY };

	private Map<String, Peptide> m_pepByModSeq;
	private Map<String, List<Feature>> m_featuresByRSName;
	private File m_peptidesFile;
	private File m_evidenceFile;

	private static File URLtoFile(URL folderURL, String file) {
		try {
			return new File(new File(folderURL.toURI()),file);
		} catch (URISyntaxException uriE) {
			throw new RuntimeException(" Error reading parameters file : "+uriE.getMessage());
		}
	}
	
	public PeptidesDataReader(URL folderURL, Map<String, Peptide> pepByUniqueKey) {
		this(URLtoFile(folderURL, MQ_PEPDATA_FILENAME), URLtoFile(folderURL, MQ_EVIDENCE_DATA_FILENAME), pepByUniqueKey);
	}
	
	public PeptidesDataReader(String mqFolder, Map<String, Peptide> pepByUniqueKey) {
		this(new File(mqFolder,MQ_PEPDATA_FILENAME), new File(mqFolder,MQ_EVIDENCE_DATA_FILENAME), pepByUniqueKey);
	}

	private PeptidesDataReader(File pepFile, File evidenceFile, Map<String, Peptide> pepByUniqueKey) {
		m_pepByModSeq = pepByUniqueKey;
		m_peptidesFile = pepFile;
		m_evidenceFile = evidenceFile;
		if(!m_peptidesFile.exists())
			throw new RuntimeException("No path to allPeptidesfile.txt defined. Can't load MQ result");
		if(!m_evidenceFile.exists())
			throw new RuntimeException("No path to evidence.txt defined. Can't load MQ result");
		m_featuresByRSName = new HashMap<>();
	}

	private Map<String, String> parseMultiMatch() {

        CSVReader reader = null;
        Map<String, String> result = new HashMap<>();
		String[] nextRow = null;

        try {

            //Read evidence Headers
            reader = new CSVReader(new FileReader(m_evidenceFile),'\t');
            String[] headers = reader.readNext();
            Map<Integer,String> headerByIndex = new HashMap<Integer, String>();
            for(int i=0; i<headers.length; i++){
                if(Arrays.asList(evidenceHeadersOfInterest).contains(headers[i].toUpperCase()) )
                    headerByIndex.put(i, headers[i].toUpperCase());
            }

            //Read evidence Data
            nextRow = reader.readNext();
            while(nextRow != null){
                Map<String,String> valByHeader = new HashMap<>();
                for(int index =0; index < nextRow.length; index++){
                    String header = headerByIndex.get(index);
                    if(header != null){
                        String value = nextRow[index];
                        valByHeader.put(header, value);
                    }
                }

                String type = valByHeader.get(TYPE);
                if (type.equalsIgnoreCase("MULTI-MATCH") || type.equalsIgnoreCase("MULTI-SECPEP")) {
                    String rsName = valByHeader.get(RAW_FILE);
                    String mz = valByHeader.get(MAX_INTENSITY_MZ_0);
                    String intensity = valByHeader.get(INTENSITY);
                    String modSequence = valByHeader.get(MODIFIED_SEQUENCE);
                    result.put(rsName+mz+intensity, modSequence);

                }
                nextRow = reader.readNext();

            } //End go through rows
            reader.close();
        } catch ( IOException e) {
            if(reader != null){
                try {
                    reader.close();
                } catch (IOException e1) {
                    logger.info("Error closing reader "+e1.getMessage());
                }
            }
            throw new RuntimeException("Error reading MSMS file.", e);
        } catch (NumberFormatException nbe){
            throw new RuntimeException("Error parsing MSMS data.", nbe);
        }
        return result;
    }

	public ResultSetsDataMapper parseQuantitationData(Map<String, Long> rsIdByName, StringBuffer warningMsg) {
				
		CSVReader reader = null;
		int featuresCount = 0;
		String[] nextRow = null;

		Map<String, String> multiMatch = parseMultiMatch();

		try {
			
			//Read Headers
			reader = new CSVReader(new FileReader(m_peptidesFile),'\t');
			String[] headers = reader.readNext();
			Map<Integer,String> headerByIndex = new HashMap<Integer, String>();			
			for(int i=0; i<headers.length; i++){
				if(Arrays.asList(headersOfInterest).contains(headers[i].toUpperCase()) )
					headerByIndex.put(i, headers[i].toUpperCase());
			}
			
			//Read Data			
			
			nextRow = reader.readNext();
			while(nextRow != null){
				Map<String,String> valByHeader = new HashMap<>();
				for(int index =0; index < nextRow.length; index++){
					String header = headerByIndex.get(index);
					if(header != null){
						String value = nextRow[index];
						valByHeader.put(header, value);
					}
				}
								
				String rsName = valByHeader.get(RAW_FILE);
				String modSequence = valByHeader.get(MODIFIED_SEQUENCE);

				if (modSequence == null || modSequence.trim().isEmpty() ) {
                        String mz = valByHeader.get(MAX_INTENSITY_MZ_0);
                        String intensity = valByHeader.get(INTENSITY);
                        String key = rsName+mz+intensity;
                        if (multiMatch.containsKey(key)) {
                            modSequence = multiMatch.get(key);
                        }
                }

				if (m_pepByModSeq.containsKey(modSequence)) {
					Peptide peptide = m_pepByModSeq.get(modSequence);
					Feature feature = createFeature(peptide, valByHeader, rsIdByName);
					MapUtils.insertOrUpdate(m_featuresByRSName, rsName, feature);
					featuresCount++;
                }
				
				nextRow = reader.readNext();
				
			} //End go through rows
			
			reader.close();
		} catch ( IOException e) {
			if(reader != null){
				try {
					reader.close();
				} catch (IOException e1) {					
					logger.info("Error closing reader "+e1.getMessage());
				}	
			}
				
			throw new RuntimeException("Error reading MSMS file.", e);
		} catch (NumberFormatException nbe){
			throw new RuntimeException("Error parsing MSMS data.", nbe);
		} catch ( Throwable t) {
			logger.error("Error while parsing line "+String.join(",", nextRow), t);
			throw t;
		}
		
		logger.info("MaxQuant Parser read "+featuresCount+" features");
		return null;
	}
	
	public Map<String, List<Feature>> getFeaturesByRSName() {
		return m_featuresByRSName;
	}
	

	private Feature createFeature(Peptide peptide, Map<String, String> valByHeader, Map<String, Long> rsIdByName) {
		
		Float elutionTime = Float.parseFloat(valByHeader.get(RETENTION_TIME));
		Integer charge = Integer.valueOf(valByHeader.get(CHARGE));
		Integer minScan = Integer.valueOf(valByHeader.get(MIN_SCAN_NUMBER));
		Integer maxScan = Integer.valueOf(valByHeader.get(MAX_SCAN_NUMBER));
		Integer numberOfScans = Integer.valueOf(valByHeader.get(NUMBER_OF_SCANS));
		Integer msmsCount = Integer.valueOf(valByHeader.get(MSMS_COUNT));
		
		Float intensity = Float.parseFloat(valByHeader.get(INTENSITY));
		if (valByHeader.get(RETENTION_LENGTH) == null) {
			logger.info("stop");
		}
		Float duration = Float.parseFloat(valByHeader.get(RETENTION_LENGTH));
		Float moz =  Float.parseFloat(valByHeader.get(MZ));		
		String rawName = valByHeader.get(RAW_FILE);

		return new Feature(
				Feature.generateNewId(), 
				moz, 
				charge, 
				elutionTime,
				intensity,
				intensity,
				duration,
				0.0f,
				numberOfScans,
				msmsCount,
				false,
				Option.empty(),
				new FeatureRelations(
						null,
						1,
						Option.empty(),
						null,
						minScan,
						maxScan,
						minScan, //TODO : expected value is apexScanInitialId,
						0,
						0,
						0,
						0,
						0,
						0,
						0,
						0,
						0,
						-1, //TODO expected value is processedMapId
						peptide.id()
				),
				null,
				null,
				null,
				Option.empty(),
				Option.empty(),
				Option.empty(),
				false,
				2,
				Option.empty());
		
	}

	
}