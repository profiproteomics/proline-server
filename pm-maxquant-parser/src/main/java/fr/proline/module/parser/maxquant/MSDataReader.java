package fr.proline.module.parser.maxquant;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.opencsv.CSVReader;

import fr.profi.util.StringUtils;
import fr.proline.core.om.model.msi.InstrumentConfig;
import fr.proline.core.om.model.msi.LocatedPtm;
import fr.proline.core.om.model.msi.Ms2Query;
import fr.proline.core.om.model.msi.PeaklistSoftware;
import fr.proline.core.om.model.msi.Peptide;
import fr.proline.core.om.model.msi.PeptideMatch;
import fr.proline.core.om.model.msi.PeptideMatchScoreType;
import fr.proline.core.om.model.msi.ProteinMatch;
import fr.proline.core.om.model.msi.PtmDefinition;
import fr.proline.core.om.model.msi.SequenceMatch;
import fr.proline.core.om.model.msi.Spectrum;
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext;
import fr.proline.core.om.provider.msi.IPTMProvider;
import fr.proline.core.om.provider.msi.IPeptideProvider;
import fr.proline.module.parser.maxquant.model.ResultSetsDataMapper;
import scala.Option;

public class MSDataReader {

	protected static Logger logger = LoggerFactory.getLogger(MSDataReader.class);

	protected final static String MQ_MSDATA_FILENAME = "combined/txt/msms.txt";

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
	protected final static String SCORE_HEADER = "Score";
	//	protected final static String FRAGMENTATION_HEADER = "Fragmentation";
	//	protected final static String MSn_ANALYZER_HEADER = "Mass analyzer";
	//	protected final static String SCAN_INDEX_HEADER = "Scan index";
	//	protected final static String ENZYME_HEADER = "Enzyme";

	
	protected static String[] headerOfInterest = {SCORE_HEADER,MSMS_ID_HEADER, RS_NAME_HEADER, SCAN_NBR_HEADER, SEQ_HEADER,MOD_SEQ_HEADER, MASS_HEADER, MISSED_CLEAVAGES_HEADER, CHARGE_HEADER,
			NBR_MATCHES_HEADER, TYPE_HEADER, PTMS_HEADER, PROTEINS_HEADER, MASSES_HEADER, INTENSITIES_HEADER, MOZ_HEADER,RT_HEADER };

	private ProviderDecoratedExecutionContext m_parserContext;
	private File m_msmsFile;
	private ResultSetsDataMapper m_rsMapper;
	private InstrumentConfig m_instrumentConfig;
	private PeaklistSoftware m_peaklistSoftware;
	  
	private HashMap<String, Peptide> pepByUniqueKey;
		 
	public MSDataReader(URL folderURL, ProviderDecoratedExecutionContext parserContext,  InstrumentConfig instrumentConfig, PeaklistSoftware peaklistSoftware) {
		m_parserContext = parserContext;
		
		try {
			m_msmsFile = new File(new File(folderURL.toURI()),MQ_MSDATA_FILENAME);
	
		} catch (URISyntaxException uriE) {
			throw new RuntimeException(" Error reading parameters file : "+uriE.getMessage());
		}
		if(!m_msmsFile.exists())
			throw new RuntimeException("No msms file defined. Can't load MQ result");
		
		m_rsMapper = new ResultSetsDataMapper();
		m_peaklistSoftware = peaklistSoftware;
		m_instrumentConfig = instrumentConfig;
		pepByUniqueKey = new HashMap<String, Peptide>();
	}
	
	public MSDataReader(String mqFolder, ProviderDecoratedExecutionContext parserContext,  InstrumentConfig instrumentConfig, PeaklistSoftware peaklistSoftware) {
		m_parserContext = parserContext;
		m_msmsFile = new File(mqFolder,MQ_MSDATA_FILENAME);
		if(!m_msmsFile.exists())
			throw new RuntimeException("No msms file defined. Can't load MQ result");
		m_rsMapper = new ResultSetsDataMapper();
		m_peaklistSoftware = peaklistSoftware;
		m_instrumentConfig = instrumentConfig;
		pepByUniqueKey = new HashMap<String, Peptide>();
	}

	public ResultSetsDataMapper parseMSData2ResulSets(Map<String, Long> rsIdByName, PtmDefinition[] allPtms, StringBuffer warningMsg) {
				
		CSVReader reader = null;
		m_rsMapper.resetMaps();
		
		try {
			
			//Read Headers
			reader = new CSVReader(new FileReader(m_msmsFile),'\t');
			String[] headers = reader.readNext();
			Map<Integer,String> headerByIndex = new HashMap<Integer, String>();			
			for(int i=0; i<headers.length; i++){
				if(Arrays.asList(headerOfInterest).contains(headers[i]) )
					headerByIndex.put(i, headers[i]);
			}
			
			//Read Data			
			IPeptideProvider pepProvider = m_parserContext.getProvider(IPeptideProvider.class);
			IPTMProvider ptmProvider = m_parserContext.getProvider(IPTMProvider.class);
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
				Spectrum sp = getOrCreateSpectrum(spectrumByScan, valByHeader, warningMsg);
				m_rsMapper.addSpectrum(rsName, new Long(sp.id()), sp);
				
				//Get or Create Ms2Query
				Ms2Query query = getOrCreateMSQuery(queryByInitialId,sp, valByHeader);
				
				//Get or Create Peptide			
				Peptide peptide  = getOrCreatePeptide(allPtms, pepProvider,ptmProvider, valByHeader, warningMsg);
				if(peptide ==null){ //Skip this line !
					nextRow = reader.readNext();
					continue;
				}				
				m_rsMapper.addPeptides(rsName, peptide);
				
				//Create PeptideMatch 
				PeptideMatch pepMatch = createPepMatch(peptide,query,rsIdByName, valByHeader);
				m_rsMapper.addPeptideMatches(rsName, pepMatch);
				
				List<ProteinMatch> protMatches = getOrCreateProteinMatches(rsIdByName, valByHeader, pepMatch);
				m_rsMapper.addProteinMatchesToPepMatch(rsName, pepMatch.id(), protMatches);				
				
				nextRow = reader.readNext();
				
			} //End go through rows
			
			//Calculate pepMatch Rank for all RS 
			calculatePepMatchRank(rsIdByName);
			
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

	private void calculatePepMatchRank(Map<String, Long> rsIdByName) {
		Iterator<String> rsNameIt = rsIdByName.keySet().iterator();
		while(rsNameIt.hasNext()){
			String rsName = rsNameIt.next();
			List<PeptideMatch> allPepMatches = m_rsMapper.getPeptideMatchesForRs(rsName);
			if(allPepMatches == null)
				break;
			
			//Map Query -> List PSM
			Map<Long,List<PeptideMatch>> pepMatchByQuery = new HashMap<>();
			for(PeptideMatch pm : allPepMatches){
				Long msQId = pm.msQueryId();
				if(!pepMatchByQuery.containsKey(msQId))
					pepMatchByQuery.put(msQId, new ArrayList<PeptideMatch>());
				pepMatchByQuery.get(msQId).add(pm);				
			}
			
			//Calculate Rank by Query
			for(List<PeptideMatch> pms : pepMatchByQuery.values()){
				pms.sort(new Comparator<PeptideMatch>() {

					@Override
					public int compare(PeptideMatch o1, PeptideMatch o2) {
						if(o1 == null ||o2 == null)
							throw new NullPointerException("Invalid null PeptideMatch specified");
						
						return Float.compare(o1.score(), o2.score()) ;						
					}
				});
				int rank = pms.size();
				for(int i =0; i<pms.size(); i++){
					pms.get(i).rank_$eq(rank);
					rank -= 1;
				}
			}//for each MsQuery
			
		} //end for each RS		
	}

	private List<ProteinMatch> getOrCreateProteinMatches(Map<String, Long> rsIdByName, Map<String, String> valByHeader,PeptideMatch pepMatch) {
		String allProtNameAndDesc = valByHeader.get(PROTEINS_HEADER);
		String rsName = valByHeader.get(RS_NAME_HEADER);
		String[] protNameAndDecrArr = allProtNameAndDesc.split(";");
		long pepId = pepMatch.peptideId();
		long pepMatchId = pepMatch.id();

		List<ProteinMatch> allProtMatches = new ArrayList<ProteinMatch>();
		Map<Long,List<ProteinMatch>> proMatches = m_rsMapper.getProteinMatchesForRs(rsName);
		if(proMatches!=null) {
			proMatches.values().forEach(allProtMatches::addAll);
		}

		List<ProteinMatch> pMatches = new ArrayList<ProteinMatch>();
		for(String nextProt : protNameAndDecrArr){
			String pName =  nextProt;
			String pDesc =  "";
			int pipeIndex = nextProt.indexOf('|');
			if(pipeIndex>0){
				pName =  nextProt.substring(0,pipeIndex);
				pDesc =  nextProt.substring(pipeIndex+1);
			}
			
		
			ProteinMatch currentProtMatch = null;
			for(ProteinMatch pm : allProtMatches){
				if(pm.accession().equals(pName)){ //same accession ProteinMatch already created !
					currentProtMatch = pm;
					break;
				}
			}
			
			if(currentProtMatch==null) { //ProtMatch Not found
				//Create associated SeqMatch
				//FIXME : Arbitrary seqPosition defined !
				SequenceMatch seqM = new SequenceMatch(1, 1+pepMatch.peptide().sequence().length(), //start - end 
													'?', '?',  //residue bef/after
													false, //isDecoy
													rsIdByName.get(rsName),												
													pepId, Option.apply(pepMatch.peptide()),
													pepMatchId, Option.apply(pepMatch),
													Option.empty());
				SequenceMatch[] allSeqMatches = new SequenceMatch[1];
				allSeqMatches[0] = seqM;
				currentProtMatch = new ProteinMatch(pName,
												pDesc,
												false, //isDecoy,
												false, //isLastBioSequence,
												ProteinMatch.generateNewId(),
												0, //taxon id											
												rsIdByName.get(rsName),
												0, //proteinId 
												Option.empty(), //protein
												null, //set all or no seqDbs ?
												null, // geneName
												0, //score
												"maxquant:score", //score type
												0f, //cov	erage
												0, // FIXME: assign the right number for peptideMatchesCount
												allSeqMatches, //sequenceMatches,
												Option.empty() // ProteinMatchProperties											
												);
			} else {
				//ProtMatch exist. Search for SeqMatch for current peptide.
				SequenceMatch[] allSeqMatches = currentProtMatch.sequenceMatches();
				boolean foudSeq = false;
				for(SequenceMatch nextSeqM : allSeqMatches){
					if(nextSeqM.peptideId() == pepId){				
						if(nextSeqM.bestPeptideMatch().get().score()<pepMatch.score()) {					
							//	SeqMatch exist for this Peptide, just change bestPeptideMatch
							nextSeqM.bestPeptideMatchId_$eq(pepMatchId);
							nextSeqM.bestPeptideMatch_$eq(Option.apply(pepMatch));
						}
						foudSeq = true;
						break;
					}
				}
				
				SequenceMatch[] finalSeqMatches = null;
				if(!foudSeq){
					//add new SeqMatch 
					SequenceMatch seqM = new SequenceMatch(1, 1+pepMatch.peptide().sequence().length(), //start - end 
						'?', '?',  //residue bef/after
						false, //isDecoy
						rsIdByName.get(rsName),												
						pepId, Option.apply(pepMatch.peptide()),
						pepMatchId, Option.apply(pepMatch),
						Option.empty());
				
					finalSeqMatches = Arrays.copyOf(allSeqMatches,allSeqMatches.length+1);
					finalSeqMatches[allSeqMatches.length] = seqM;
									
				} else 					
					finalSeqMatches = allSeqMatches;
				
				currentProtMatch.sequenceMatches_$eq(finalSeqMatches);
			} // end Protein Match found
						
			
			pMatches.add(currentProtMatch);	
		} // end go through Protein Matches
		
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
			1, // Rank will be calculated once all Pep matches are read
			score,
			PeptideMatchScoreType.MAXQUANT_SCORE(),
			charge,
			deltaMass,
			Boolean.FALSE, //isDecoy !! TODO
			peptide,
			missCle,
			fragmMatchCount,
			query,
			true, //isValidated
			rsId,
			0, //cd Prettyrank
			0,//sd Prettyrank
			null, Option.empty(), 0l,//children
			Option.empty(), //PeptideMatchProperties
			Option.empty() //PeptideMatchResultSummaryProperties
			);
		
		return pm;
	}

	private Peptide getOrCreatePeptide(PtmDefinition[] allPtms, IPeptideProvider pepProvider, IPTMProvider ptmProvider, Map<String, String> valByHeader, StringBuffer warningMsg) {
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
						List<PtmDefinition> foundPtmDefs = PTMUtils.parsePTMString(ptmProvider, ptmName);
						PtmDefinition ptmDef = null;
						if(foundPtmDefs!=null && !foundPtmDefs.isEmpty())
							ptmDef = foundPtmDefs.get(0); 
						else{
							warningMsg.append("No PTM found for ").append(ptmName).append("\n");
						}
						Integer count = Integer.valueOf(beforeSapce);
						countByPtmDefs.put(ptmDef,count );
						ptmCount+=count;
						ptmDefByAbv.put(ptmName.substring(0,2).toLowerCase(),ptmDef);
						
					} else { //No count of PTM: directly PTM readable string
						List<PtmDefinition> foundPtmDefs = PTMUtils.parsePTMString(ptmProvider, nextPtmString);
						PtmDefinition ptmDef = null;
						if(foundPtmDefs!=null && !foundPtmDefs.isEmpty())
							ptmDef = foundPtmDefs.get(0); 					
						else{
							warningMsg.append("No PTM found for ").append(nextPtmString).append("\n");
						}
						countByPtmDefs.put(ptmDef,1);
						ptmCount+=1;
						ptmDefByAbv.put(nextPtmString.substring(0,2).toLowerCase(),ptmDef);
					}
				} else { // no space in ptm name
					List<PtmDefinition> foundPtmDefs = PTMUtils.parsePTMString(ptmProvider, nextPtmString);
					PtmDefinition ptmDef = null;
					if(foundPtmDefs!=null && !foundPtmDefs.isEmpty())
						ptmDef = foundPtmDefs.get(0); 		
					else{
						warningMsg.append("No PTM found for ").append(nextPtmString).append("\n");
					}				
					countByPtmDefs.put(ptmDef,1);
					ptmCount+=1;
					ptmDefByAbv.put(nextPtmString.substring(0,2).toLowerCase(),ptmDef);
				} //end no space found
			} // for each ptm
			
			
			// --- For location, see modified seq : _(ac)ADKM(ox)DM(ox)SIDDIIK_
						
			char[] aas = modSeq.substring(1,modSeq.length()-1).toCharArray();
			int seqIndex =0;
			boolean inAbvModif=false;
			int locPtmsIndex =0;
			LocatedPtm[] allLocPtms = new LocatedPtm[ptmCount];
			StringBuilder abvModifBuilder =new StringBuilder();
			
			int locationCount = 0;
			for(char aa : aas){
				if(aa=='('){
					inAbvModif = true;
					abvModifBuilder = new StringBuilder();
				} else if(aa==')'){
					String abv = abvModifBuilder.toString().toLowerCase();
					PtmDefinition ptmdef = ptmDefByAbv.get(abv); 	
					if(ptmdef == null){
						logger.warn( "UNABLE to GET PTM DEF for " +abv+" :: "+seq+" ; modification-> "+ptmsString);
						warningMsg.append( "UNABLE to get PTM definition for " ).append(abv).append("in peptide ").append(seq);
						warningMsg.append(" - ").append(ptmsString).append("\n");
					} else {
						allLocPtms[locPtmsIndex] = LocatedPtm.apply(ptmdef, seqIndex);
						locPtmsIndex++;
						locationCount++;
					}
					inAbvModif=false;
				} else if(inAbvModif){
					abvModifBuilder.append(aa);					
				} else {
					seqIndex++;
				}
			}
			
			if(locationCount!=ptmCount){
				warningMsg.append("Unable to get location for all PTMs for peptide " ).append(seq).append("- ").append(ptmsString).append(" peptide IGNORED ");
				logger.warn("!!!! Unable to get location for all PTMs for peptide "+seq+" ; modification-> "+ptmsString+". peptide IGNORED ");
				return null;
			} 
			
			String uniquePepkey = seq+"%"+Peptide.makePtmString(allLocPtms);	
			
			//See if peptide allready  defined			
			if(pepByUniqueKey.containsKey(uniquePepkey))
				peptide = pepByUniqueKey.get(uniquePepkey);
			else {
				//Read from datastore if exist
				Option<Peptide> pepOpt = pepProvider.getPeptide(seq, allLocPtms);				
				peptide = (pepOpt.isDefined()) ? pepOpt.get() :  new Peptide(seq,allLocPtms,Peptide.calcMass(seq));
				pepByUniqueKey.put(uniquePepkey, peptide);	
			}
		 	
		} else {
			//No Ptms defined for this peptide
			LocatedPtm[] emptyLocPtms = new LocatedPtm[0];
			String uniquePepkey = seq+"%";
			if(pepByUniqueKey.containsKey(uniquePepkey))
				peptide = pepByUniqueKey.get(uniquePepkey);
			else {
				Option<Peptide> pepOpt = pepProvider.getPeptide(seq, emptyLocPtms);
				peptide = (pepOpt.isDefined()) ? pepOpt.get() :  new Peptide(seq,emptyLocPtms,Peptide.calcMass(seq));
				pepByUniqueKey.put(uniquePepkey, peptide);
			}
		}
							
		return peptide;
			
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
				moz, charge, spectrum.title(), spectrum.id(), Option.empty());
		}
		return query;
	}


	private Spectrum getOrCreateSpectrum(Map<Integer, Spectrum> spectrumByScan, Map<String, String> valByHeader, StringBuffer warningMsg) {
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
			double[] mozList = new double[0];
			if(StringUtils.isNotEmpty(mozListAsString)){
				String[] fragMasses = mozListAsString.split(";");
				mozList = new double[fragMasses.length];
				for(int i=0; i<fragMasses.length; i++){
					mozList[i] = Double.parseDouble(fragMasses[i]);
				}
			} else {
				warningMsg.append(" No masses for scan ").append(title);
			}
			
			String intensityListAsString = valByHeader.get(INTENSITIES_HEADER);
			float[] intensitiesList = new float[0];
			if(StringUtils.isNotEmpty(mozListAsString)){
				String[] fragIntensities = intensityListAsString.split(";");
				intensitiesList = new float[fragIntensities.length];
				for(int i=0; i<fragIntensities.length; i++){
					intensitiesList[i] = Float.parseFloat(fragIntensities[i]);
				}
			}else {
				warningMsg.append(" No intensities for scan ").append(title);
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
			m_instrumentConfig.id(), 
			m_peaklistSoftware.id(), 
			Option.empty());	
			spectrumByScan.put(scanNbr, readSp);
		}
		return readSp;
	}
	
}