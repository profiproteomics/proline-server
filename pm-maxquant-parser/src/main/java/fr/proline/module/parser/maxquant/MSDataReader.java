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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import fr.proline.core.om.model.msi.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.opencsv.CSVReader;

import fr.profi.util.StringUtils;
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext;
import fr.proline.core.om.provider.msi.IPTMProvider;
import fr.proline.core.om.provider.msi.IPeptideProvider;
import fr.proline.module.parser.maxquant.model.ResultSetsDataMapper;
import scala.Option;

public class MSDataReader {

    public static final int MAX_ACCESSION_LENGTH = 100;
    protected static Logger logger = LoggerFactory.getLogger(MSDataReader.class);

	private final static String MQ_MSDATA_FILENAME = "combined/txt/msms.txt";

	//MSMS file header
	private final static String RS_NAME_HEADER = "Raw file";
	private final static String SCAN_NBR_HEADER = "Scan number";
	private final static String SEQ_HEADER = "Sequence";
	private final static String MASS_HEADER = "Mass";
	private final static String MISSED_CLEAVAGES_HEADER = "Missed cleavages";
	private final static String CHARGE_HEADER = "Charge";
	private final static String NBR_MATCHES_HEADER = "Number of Matches";
	private final static String TYPE_HEADER = "Type";
	private final static String PTMS_HEADER = "Modifications";
	private final static String PROTEINS_HEADER = "Proteins";
	private final static String MASSES_HEADER = "Masses";
	private final static String INTENSITIES_HEADER = "Intensities";
	private final static String MOZ_HEADER = "m/z";
	private final static String RT_HEADER = "Retention time";
	private final static String MSMS_ID_HEADER = "id";
	private final static String MOD_SEQ_HEADER = "Modified sequence";
	private final static String SCORE_HEADER = "Score";
	//	protected final static String FRAGMENTATION_HEADER = "Fragmentation";
	//	protected final static String MSn_ANALYZER_HEADER = "Mass analyzer";
	//	protected final static String SCAN_INDEX_HEADER = "Scan index";
	//	protected final static String ENZYME_HEADER = "Enzyme";

	
	private static String[] headerOfInterest = {SCORE_HEADER,MSMS_ID_HEADER, RS_NAME_HEADER, SCAN_NBR_HEADER, SEQ_HEADER,MOD_SEQ_HEADER, MASS_HEADER, MISSED_CLEAVAGES_HEADER, CHARGE_HEADER,
			NBR_MATCHES_HEADER, TYPE_HEADER, PTMS_HEADER, PROTEINS_HEADER, MASSES_HEADER, INTENSITIES_HEADER, MOZ_HEADER,RT_HEADER };

	private ProviderDecoratedExecutionContext m_parserContext;
	private File m_msmsFile;
	private ResultSetsDataMapper m_rsMapper;
	private Long  m_fragmentationRuleSetId;
	private PeaklistSoftware m_peaklistSoftware;
	  
	private HashMap<String, Peptide> m_pepByUniqueKey;

	private static File URLtoFile(URL folderURL) {
		try {
			return new File(new File(folderURL.toURI()),MQ_MSDATA_FILENAME);
		} catch (URISyntaxException uriE) {
			throw new RuntimeException("Error reading parameters file : "+uriE.getMessage());
		}
	}

	public MSDataReader(URL msmsFileURL, ProviderDecoratedExecutionContext parserContext,  Long fragRuleSet , PeaklistSoftware peaklistSoftware) {
		this(URLtoFile(msmsFileURL), parserContext, fragRuleSet, peaklistSoftware);
	}
	
	public MSDataReader(String mqFolder, ProviderDecoratedExecutionContext parserContext, Long fragRuleSet , PeaklistSoftware peaklistSoftware) {
		this(new File(mqFolder,MQ_MSDATA_FILENAME), parserContext, fragRuleSet, peaklistSoftware);
	}

	private MSDataReader(File msmsFile, ProviderDecoratedExecutionContext parserContext,  Long fragRuleSet , PeaklistSoftware peaklistSoftware) {
		m_parserContext = parserContext;
		m_msmsFile = msmsFile;
		m_fragmentationRuleSetId = fragRuleSet;
		if(!m_msmsFile.exists())
			throw new RuntimeException("No msms file defined. Can't load MQ result");
		m_rsMapper = new ResultSetsDataMapper();
		m_peaklistSoftware = peaklistSoftware;
		m_pepByUniqueKey = new HashMap<String, Peptide>();
	}

	public ResultSetsDataMapper parseMSData2ResulSets(Map<String, Long> rsIdByName, PtmDefinition[] allPtms, String accessionRegexp, StringBuffer warningMsg) {
				
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
				
				List<ProteinMatch> protMatches = getOrCreateProteinMatches(rsIdByName, valByHeader, pepMatch, accessionRegexp);
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

	
	public Map<String, Peptide> getPeptidesByMQModifiedSequence() {
		return m_pepByUniqueKey;
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

	private List<ProteinMatch> getOrCreateProteinMatches(Map<String, Long> rsIdByName, Map<String, String> valByHeader,PeptideMatch pepMatch, String accessionRegexp) {
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

			Pattern pattern = Pattern.compile(accessionRegexp);
			Matcher m = pattern.matcher(nextProt);
			if (m.find()) {
				pName = m.group(1);
				if (m.groupCount() > 1) {
					pDesc = m.group(2);
				} else if (m.end(1) < nextProt.length()){
					pDesc = nextProt.substring(m.end(1) + 1);
				}
			}

            // cut pName after 100 char to avoid db storage error
            if (pName.length() > MAX_ACCESSION_LENGTH)
                pName = pName.substring(0, (MAX_ACCESSION_LENGTH-1-3))+"...";

			ProteinMatch currentProtMatch = null;
			for(ProteinMatch pm : allProtMatches){
				if(pm.accession().equals(pName)){ //same accession ProteinMatch already created !
					currentProtMatch = pm;
					break;
				}
			}
			
			if(currentProtMatch==null) { //ProtMatch Not found
				//Create associated SeqMatch
				Option<SequenceMatchProperties> noOpt =Option.empty();
				//FIXME : Arbitrary seqPosition defined !
				SequenceMatch seqM = new SequenceMatch(1, 1+pepMatch.peptide().sequence().length(), //start - end 
													'?', '?',  //residue bef/after
													false, //isDecoy
													rsIdByName.get(rsName),												
													pepId, (Option<Peptide>) Option.apply(pepMatch.peptide()),
													pepMatchId, (Option<PeptideMatch>)Option.apply(pepMatch),
													noOpt);
				SequenceMatch[] allSeqMatches = new SequenceMatch[1];
				allSeqMatches[0] = seqM;
				Option<Protein> noProtOp =  Option.empty();
				Option<ProteinMatchProperties> noPrpOp = Option.empty();
				currentProtMatch = new ProteinMatch(pName,
												pDesc,
												false, //isDecoy,
												false, //isLastBioSequence,
												ProteinMatch.generateNewId(),
												0, //taxon id											
												rsIdByName.get(rsName),
												0, //proteinId 
												noProtOp, //protein
												null, //set all or no seqDbs ?
												null, // geneName
												0, //score
												"andromeda:score", //score type
												0,
												allSeqMatches, //sequenceMatches,
												noPrpOp // ProteinMatchProperties
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
							nextSeqM.bestPeptideMatch_$eq((Option<PeptideMatch>) Option.apply(pepMatch));
						}
						foudSeq = true;
						break;
					}
				}
				
				SequenceMatch[] finalSeqMatches = null;
				if(!foudSeq){
					//add new SeqMatch
					Option<SequenceMatchProperties> noPrpOp = Option.empty();
					SequenceMatch seqM = new SequenceMatch(1, 1+pepMatch.peptide().sequence().length(), //start - end 
						'?', '?',  //residue bef/after
						false, //isDecoy
						rsIdByName.get(rsName),												
						pepId, (Option<Peptide>)Option.apply(pepMatch.peptide()),
						pepMatchId, (Option<PeptideMatch>) Option.apply(pepMatch),
						noPrpOp);
				
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
		Option<PeptideMatch[]> noChildsOp = Option.empty();
		Option<PeptideMatchProperties> noPropOp = Option.empty();
		Option<PeptideMatchResultSummaryProperties> noValidPropOp = Option.empty();
		PeptideMatch pm = new PeptideMatch(PeptideMatch.generateNewId(),
			1, // Rank will be calculated once all Pep matches are read
			score,
			PeptideMatchScoreType.ANDROMEDA_SCORE(),
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
			null, noChildsOp, 0l,//children
		 	noPropOp, //PeptideMatchProperties
			noValidPropOp //PeptideMatchResultSummaryProperties
			);
		
		return pm;
	}

	private Peptide getOrCreatePeptide(PtmDefinition[] allPtms, IPeptideProvider pepProvider, IPTMProvider ptmProvider, Map<String, String> valByHeader, StringBuffer warningMsg) {
		String ptmsString = valByHeader.get(PTMS_HEADER);
		String seq = valByHeader.get(SEQ_HEADER);
		String modSeq = valByHeader.get(MOD_SEQ_HEADER);
		Peptide peptide = null;
		
		if(!ptmsString.equalsIgnoreCase("Unmodified")){
			
			//See if peptide already  defined			
			if(m_pepByUniqueKey.containsKey(modSeq))
				peptide = m_pepByUniqueKey.get(modSeq);
			else {
				//Read from datastore if exist
				LocatedPtm[] allLocPtms = getLocatedPtmsFromModifiedSequence(modSeq, ptmsString, ptmProvider, warningMsg);
				if (allLocPtms == null) {
					return null;
				}
				Option<Peptide> pepOpt = pepProvider.getPeptide(seq, allLocPtms);				
				peptide = (pepOpt.isDefined()) ? pepOpt.get() :  new Peptide(seq,allLocPtms,Peptide.calcMass(seq));
				m_pepByUniqueKey.put(modSeq, peptide);	
			}
		 	
		} else {
			//No Ptms defined for this peptide
			LocatedPtm[] emptyLocPtms = new LocatedPtm[0];
			if(m_pepByUniqueKey.containsKey(modSeq))
				peptide = m_pepByUniqueKey.get(modSeq);
			else {
				Option<Peptide> pepOpt = pepProvider.getPeptide(seq, emptyLocPtms);
				peptide = (pepOpt.isDefined()) ? pepOpt.get() :  new Peptide(seq,emptyLocPtms,Peptide.calcMass(seq));
				m_pepByUniqueKey.put(modSeq, peptide);
			}
		}
							
		return peptide;
			
	}

	private LocatedPtm[] getLocatedPtmsFromModifiedSequence(String modSeq, String ptmsString, IPTMProvider ptmProvider, StringBuffer warningMsg) {
		
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
		int seqIndex = 0;
		boolean inAbvModif = false;
		int locPtmsIndex = 0;
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
					logger.warn( "UNABLE to GET PTM DEF for " +abv+" :: "+modSeq+" ; modification-> "+ptmsString);
					warningMsg.append( "UNABLE to get PTM definition for " ).append(abv).append("in peptide ").append(modSeq);
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
			warningMsg.append("Unable to get location for all PTMs for peptide " ).append(modSeq).append("- ").append(ptmsString).append(" peptide IGNORED ");
			logger.warn("!!!! Unable to get location for all PTMs for peptide "+modSeq+" ; modification-> "+ptmsString+". peptide IGNORED ");
			return null;
		} 
		
		return allLocPtms;
	}

	private Ms2Query getOrCreateMSQuery(Map<Integer, Ms2Query> queryByInitialId, Spectrum spectrum, Map<String, String> valByHeader) {
		Integer initialId = Integer.valueOf(valByHeader.get(MSMS_ID_HEADER));
		Ms2Query query =   null;
		if(queryByInitialId.containsKey(initialId)){
			query = queryByInitialId.get(initialId);
		} else {			
			
			Double moz = Double.parseDouble(valByHeader.get(MOZ_HEADER));
			Integer charge = Integer.valueOf(valByHeader.get(CHARGE_HEADER));
			Option<MsQueryProperties> noMsQProp = Option.empty();
			query =  new Ms2Query(Ms2Query.generateNewId(), initialId, 
				moz, charge, spectrum.title(), spectrum.id(), 0L, noMsQProp);
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

			Option<Object> rtVal= Option.apply(rt);
			SpectrumProperties spectrumProp = new SpectrumProperties( rtVal);

			Option<Object> frs = Option.empty();
			if(m_fragmentationRuleSetId> 0)
				frs = Option.apply(m_fragmentationRuleSetId);
			readSp = new Spectrum(Spectrum.generateNewId(), title,
			moz, Float.NaN /*Prec Intenity*/, charge, //Precursor data 
			false, //isSummed, 
			0, 0, //Cycle
			scanNbr, 0, //Scan
			rt, rt, //Time
			(Option<double[]>) Option.apply(mozList),
			(Option<float[]>) Option.apply(intensitiesList),
			mozList.length,
			frs,
			m_peaklistSoftware.id(), 
			(Option<SpectrumProperties>) Option.apply(spectrumProp));
			spectrumByScan.put(scanNbr, readSp);
		}
		return readSp;
	}
	
}