package fr.proline.module.parser.maxquant;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.opencsv.CSVReader;

import fr.proline.core.om.model.msi.LocatedPtm;
import fr.proline.core.om.model.msi.Ms2Query;
import fr.proline.core.om.model.msi.Peptide;
import fr.proline.core.om.model.msi.PeptideMatch;
import fr.proline.core.om.model.msi.PeptideMatchResultSummaryProperties;
import fr.proline.core.om.model.msi.PeptideMatchScoreType;
import fr.proline.core.om.model.msi.ProteinMatch;
import fr.proline.core.om.model.msi.PtmDefinition;
import fr.proline.core.om.model.msi.Spectrum;
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext;
import fr.proline.core.om.provider.msi.IPeptideProvider;
import fr.proline.module.parser.maxquant.model.ResultSetsDataMapper;
import scala.Option;

public class MSDataReader {

	protected static Logger logger = LoggerFactory.getLogger(MSDataReader.class);

	protected final static String MQ_MSDATA_FILENAME = "msms.txt";

	//MSMS file header
	protected final static String RS_NAME_HEADER = "Raw file";
	protected final static String SCAN_NBR_HEADER = "Scan number";
	protected final static String SEQ_HEADER = "Sequence";
	protected final static String MASS_HEADER = "Mass";
	protected final static String MISSED_CLEAVAGES_HEADER = "Missed cleavages";
	protected final static String CHARGE_HEADER = "Charge";
	protected final static String NBR_MATCHES_HEADER = "Number of Matches";
	protected final static String TYPE_HEADER = "Type";
	protected final static String PTMS_HEADER = "Modifications";
	protected final static String PROTEINS_HEADER = "Proteins";
	protected final static String MASSES_HEADER = "Masses";
	protected final static String INTENSITIES_HEADER = "Intensities";
	protected final static String MOZ_HEADER = "m/z";
	protected final static String RT_HEADER = "Retention time";
	protected final static String MSMS_ID_HEADER = "id";
	protected final static String MOD_SEQ_HEADER = "Modified sequence";
	protected final static String SCORE_HEADER = "Modified sequence";
	//	protected final static String FRAGMENTATION_HEADER = "Fragmentation";
	//	protected final static String MSn_ANALYZER_HEADER = "Mass analyzer";
	//	protected final static String SCAN_INDEX_HEADER = "Scan index";
	//	protected final static String ENZYME_HEADER = "Enzyme";

	
	protected static String[] headerOfInterest = {SCORE_HEADER,MSMS_ID_HEADER, RS_NAME_HEADER, SCAN_NBR_HEADER, SEQ_HEADER,MOD_SEQ_HEADER, MASS_HEADER, MISSED_CLEAVAGES_HEADER, CHARGE_HEADER,
			NBR_MATCHES_HEADER, TYPE_HEADER, PTMS_HEADER, PROTEINS_HEADER, MASSES_HEADER, INTENSITIES_HEADER, MOZ_HEADER,RT_HEADER };

	private ProviderDecoratedExecutionContext m_parserContext;
	private File m_msmsFile;
	private ResultSetsDataMapper m_rsMapper;
	
	
	public MSDataReader(String mqFolder, ProviderDecoratedExecutionContext parserContext) {
		m_parserContext = parserContext;
		File m_msmsFile = new File(mqFolder,MQ_MSDATA_FILENAME);
		if(!m_msmsFile.exists())
			throw new RuntimeException("No msms file defined. Can't load MQ result");
		m_rsMapper = new ResultSetsDataMapper();
	}

	public ResultSetsDataMapper parseMSData2ResulSets(Map<String, Long> rsIdByName, PtmDefinition[] allPtms) {
				
		CSVReader reader = null;
		m_rsMapper.resetMaps();
		
		try {
			
			//Read Headers
			reader = new CSVReader(new FileReader(m_msmsFile));
			String[] headers = reader.readNext();
			Map<Integer,String> headerByIndex = new HashMap<Integer, String>();			
			for(int i=0; i<headers.length; i++){
				if(Arrays.asList(headerOfInterest).contains(headers[i]) )
					headerByIndex.put(i, headers[i]);
			}
			
			//Read Data			
			IPeptideProvider pepProvider = m_parserContext.getProvider(IPeptideProvider.class);
			Map<Integer, Spectrum> spectrumByScan = new HashMap<Integer, Spectrum>();
			Map<Integer, Ms2Query> queryByInitialId = new HashMap<Integer, Ms2Query>();
			
			String[] nextRow = reader.readNext();
			while(nextRow != null){
				Map<String,String> valByHeader = new HashMap<>();
				for(int index =0; index < nextRow.length; index++){
					String header = headerByIndex.get(index);
					if(header != null){
						String value = nextRow[index];
						valByHeader.put(header, value);
					}
				}
								
				String rsName = valByHeader.get(RS_NAME_HEADER);
				
				//Get or Create Spectrum
				Spectrum sp = getOrCreateSpectrum(spectrumByScan, valByHeader);
				
				//Get or Create Ms2Query
				Ms2Query query = getOrCreateMSQuery(queryByInitialId,sp, valByHeader);
				
				//Get or Create Peptide			
				Peptide peptide  = getOrCreatePeptide(allPtms, pepProvider, valByHeader);
				m_rsMapper.addPeptides(rsName, peptide);
				
				//Create PeptideMatch 
				PeptideMatch pepMatch = createPepMatch(peptide,query,rsIdByName, valByHeader);
				m_rsMapper.addPeptideMatches(rsName, pepMatch);
				
				List<ProteinMatch> protMatches = getOrCreateProteinMatch(rsIdByName, valByHeader);

				//TODO keep Map typical ProtMach-> all ProtMatch. Read from other files for seqMatch  
				
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
		
		
		return m_rsMapper;
	}

	private List<ProteinMatch> getOrCreateProteinMatch(Map<String, Long> rsIdByName, Map<String, String> valByHeader) {
		String allProtNameAndDesc = valByHeader.get(PROTEINS_HEADER);
		String rsName = valByHeader.get(RS_NAME_HEADER);
		String[] protNameAndDecrArr = allProtNameAndDesc.split(";");
		List<ProteinMatch> pMatches = new ArrayList<ProteinMatch>();
		for(String nextProt : protNameAndDecrArr){
			String pName =  nextProt;
			String pDesc =  "";
			int pipeIndex = nextProt.indexOf('|');
			if(pipeIndex>0){
				pName =  nextProt.substring(0,pipeIndex);
				pDesc =  nextProt.substring(pipeIndex+1);
			}
			boolean foundProt = false;
			Iterator<ProteinMatch> protMIt = m_rsMapper.getProteinMatchesForRs(rsName).iterator();
			while(protMIt.hasNext()){
				ProteinMatch pm = protMIt.next();
				if(pm.accession().equals(pName)){ //same accession ProteinMatch already created !
					pMatches.add(pm);
					foundProt = true; 
					break;
				}
			}
			if(!foundProt) {
				pMatches.add(new ProteinMatch(pName,
												pDesc,
												false, //isDecoy,
												false, //isLastBioSequence,
												ProteinMatch.generateNewId(),
												0, //taxon id											
												rsIdByName.get(rsName),
												0, //proteinId 
												null, //protein
												null, //set all or no seqDbs ?
												null, // geneName
												0, //score
												"maxquant:standard score", //score type
												0f, //coverage
												0, // FIXME: assign the right number for peptideMatchesCount
												null, //sequenceMatches,
												null // ProteinMatchProperties											
												));
			}
		}
		
		return pMatches;
	}

	private PeptideMatch createPepMatch(Peptide peptide, Ms2Query query,Map<String, Long> rsIdByName, Map<String, String> valByHeader) {
		
		Float score = Float.parseFloat(valByHeader.get(SCORE_HEADER));
		Integer charge = Integer.valueOf(valByHeader.get(CHARGE_HEADER));
		Integer missCle = Integer.valueOf(valByHeader.get(MISSED_CLEAVAGES_HEADER));
		Integer fragmMatchCount = Integer.valueOf(valByHeader.get(NBR_MATCHES_HEADER));
		Float moz =  Float.parseFloat(valByHeader.get(MOZ_HEADER));		
		Float deltaMass = moz - new Float(peptide.calculatedMass());
		String rawName = valByHeader.get(RS_NAME_HEADER);
		Long rsId = rsIdByName.get(rawName);
	
		PeptideMatch pm = new PeptideMatch(PeptideMatch.generateNewId(),
			-1, //TODO Rank to calculated once all Pep matches are read
			score,
			PeptideMatchScoreType.MASCOT_IONS_SCORE(),
			charge,
			deltaMass,
			Boolean.FALSE, //isDecoy !! TODO
			peptide,
			missCle,
			fragmMatchCount,
			query,
			true,
			rsId,
			0, //cd Prettyrank
			0,//sd Prettyrank
			null, Option.empty(), 0l,//children
			null, //PeptideMatchProperties
			Option.apply((PeptideMatchResultSummaryProperties) null) //PeptideMatchResultSummaryProperties
			);
		
		return pm;
	}

	private Peptide getOrCreatePeptide(PtmDefinition[] allPtms, IPeptideProvider pepProvider, Map<String, String> valByHeader) {
		String ptmsString = valByHeader.get(PTMS_HEADER);
		String seq = valByHeader.get(SEQ_HEADER);
		String modSeq = valByHeader.get(MOD_SEQ_HEADER);
		Peptide peptide = null;
		
		if(!ptmsString.equalsIgnoreCase("Unmodified")){
			
			Map<PtmDefinition, Integer> countByPtmDefs = new HashMap<>();
			Map<String,PtmDefinition> ptmDefByAbv = new HashMap<>();

			//Parse modification 
			//"Acetyl (Protein N-term),2 Oxidation (M)" => 3 Modif
			Integer ptmCount = 0;
			String[] ptmsAsString=ptmsString.split(",");
			for(String nextPtmString : ptmsAsString){
				int indexSpace = nextPtmString.indexOf(" ");
				if(indexSpace>0){
					String beforeSapce =  nextPtmString.substring(0, indexSpace);
					if(beforeSapce.matches("\\d+$")){
						String ptmName = nextPtmString.substring(indexSpace+1);
						PtmDefinition ptmDef = getPtmDef(ptmName, allPtms); 
						Integer count = Integer.valueOf(beforeSapce);
						countByPtmDefs.put(ptmDef,count );
						ptmCount+=count;
						ptmDefByAbv.put(ptmName.substring(0,2).toLowerCase(),ptmDef);
					} else {
						PtmDefinition ptmDef = getPtmDef(nextPtmString, allPtms); 						
						countByPtmDefs.put(ptmDef,1);
						ptmCount+=1;
						ptmDefByAbv.put(nextPtmString.substring(0,2).toLowerCase(),ptmDef);
					}
				} else { // no space in ptm name
					PtmDefinition ptmDef = getPtmDef(nextPtmString, allPtms); 					
					countByPtmDefs.put(ptmDef,1);
					ptmCount+=1;
					ptmDefByAbv.put(nextPtmString.substring(0,2).toLowerCase(),ptmDef);
				} //end no space found
			} // for each ptm
			
			// For location, see modified seq : _(ac)ADKM(ox)DM(ox)SIDDIIK_
			char[] aas = modSeq.substring(1,modSeq.length()-1).toCharArray();
			int seqIndex =0;
			boolean inAbvModif=false;
			int locPtmsIndex =0;
			LocatedPtm[] allLocPtms = new LocatedPtm[ptmCount];
			StringBuilder abvModifBuilder =new StringBuilder();
			
			for(char aa : aas){
				if(aa=='('){
					inAbvModif = true;
					abvModifBuilder = new StringBuilder();
				} else if(aa==')'){
					String abv = abvModifBuilder.toString().toLowerCase();
					PtmDefinition ptmdef = ptmDefByAbv.get(abv); 					
					allLocPtms[locPtmsIndex] = LocatedPtm.apply(ptmdef, seqIndex);
					locPtmsIndex++;
					inAbvModif=false;
				} else if(inAbvModif){
					abvModifBuilder.append(aa);					
				} else {
					seqIndex++;
				}
			}
		 	Option<Peptide> pepOpt = pepProvider.getPeptide(seq, allLocPtms);
		 	peptide = (pepOpt.isDefined()) ? pepOpt.get() :  new Peptide(seq,allLocPtms,Peptide.calcMass(seq));
		 	
		} else {
			//No Ptms defined for this peptide
			Option<Peptide> pepOpt = pepProvider.getPeptide(seq, null);
			peptide = (pepOpt.isDefined()) ? pepOpt.get() :  new Peptide(seq,null,Peptide.calcMass(seq));
		}
							
		return peptide;
			
	}
		
	private PtmDefinition getPtmDef(String ptmStr,PtmDefinition[] allPtms ){
		PtmDefinition ptmDef = null; 
		for(PtmDefinition definition : allPtms){
			if(definition.toReadableString().equalsIgnoreCase(ptmStr)){
				ptmDef = definition;
				break;
			}
		}
		return ptmDef;
	}

	private Ms2Query getOrCreateMSQuery(Map<Integer, Ms2Query> queryByInitialId, Spectrum spectrum, Map<String, String> valByHeader) {
		Integer initialId = Integer.valueOf(valByHeader.get(MSMS_ID_HEADER));
		Ms2Query query =   null;
		if(queryByInitialId.containsKey(initialId)){
			query = queryByInitialId.get(initialId);
		} else {			
			
			Double moz = Double.parseDouble(valByHeader.get(MOZ_HEADER));
			Integer charge = Integer.valueOf(valByHeader.get(CHARGE_HEADER));			
			query =  new Ms2Query(Ms2Query.generateNewId(), initialId, 
				moz, charge, spectrum.title(), spectrum.id(), null);
		}
		return query;
	}


	private Spectrum getOrCreateSpectrum(Map<Integer, Spectrum> spectrumByScan, Map<String, String> valByHeader) {
		Integer scanNbr = Integer.valueOf(valByHeader.get(SCAN_NBR_HEADER));
		Spectrum readSp = null;
		if(spectrumByScan.containsKey(scanNbr)){
			readSp = spectrumByScan.get(scanNbr);
		} else {
			String title="Scan "+valByHeader.get(SCAN_NBR_HEADER)+" (rt="+valByHeader.get(RT_HEADER)+") ";
			Double moz = Double.parseDouble(valByHeader.get(MOZ_HEADER));
			Integer charge = Integer.valueOf(valByHeader.get(CHARGE_HEADER));
			Float rt = Float.parseFloat(valByHeader.get(RT_HEADER));
			
			String mozListAsString = valByHeader.get(MASSES_HEADER);
			String[] fragMasses = mozListAsString.split(";");
			double[] mozList = new double[fragMasses.length];
			for(int i=0; i<fragMasses.length; i++){
				mozList[i] = Double.parseDouble(fragMasses[i]);
			}
			
			String intensityListAsString = valByHeader.get(INTENSITIES_HEADER);
			String[] fragIntensities = intensityListAsString.split(";");
			float[] intensitiesList = new float[fragIntensities.length];
			for(int i=0; i<fragIntensities.length; i++){
				intensitiesList[i] = Float.parseFloat(fragIntensities[i]);
			}
			
			readSp = new Spectrum(Spectrum.generateNewId(), title,
			moz, Float.NaN /*Prec Intenity*/, charge, //Precursor data 
			false, //isSummed, 
			0, 0, //Cycle
			scanNbr, 0, //Scan
			rt, rt, //Time 
			Option.apply(mozList), 
			Option.apply(intensitiesList),
			mozList.length,
			-1L, // TODO GET From RS : instrumentConfigId
			-1L, //TODO GET From RS : peaklistId
			null);	
			spectrumByScan.put(scanNbr, readSp);
		}
		return readSp;
	}
	
}