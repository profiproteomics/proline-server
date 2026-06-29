/*
 * Copyright (C)  2026.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the CeCILL FREE SOFTWARE LICENSE AGREEMENT
 * ; either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * CeCILL License V2.1 for more details.
 *
 * You should have received a copy of the CeCILL License
 * along with this program;
 * If not, see <http://www.cecill.info/licences/Licence_CeCILL_V2.1-en.html>.
 *
 */

package fr.proline.module.paser.diann.builder;

import fr.proline.core.om.model.msi.*;
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext;
import fr.proline.core.om.provider.msi.IPeptideProvider;
import fr.proline.module.paser.diann.model.DiaNNPepModification;
import fr.proline.module.paser.diann.model.DiaNNResult;
import fr.proline.module.paser.diann.model.Fragment;
import fr.proline.module.paser.diann.model.Precursor;
import fr.proline.module.paser.diann.model.QuantPrecursor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.Option;
import scala.Tuple2;
import scala.collection.Seq;
import scala.collection.mutable.ArrayBuffer;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class MSDataBuilder {
  private static final Logger logger = LoggerFactory.getLogger(MSDataBuilder.class);

  IPeptideProvider m_pepProvider;

  //Cache for value to be keep during different resultset creation and to be returned by methods
  private final HashMap<String, Peptide> m_pepByUniqueKey;
  private final HashMap<String, Set<String>> m_precIdByPepUniqueKey;
  private HashMap<Long, Spectrum> m_spectraById;

  public MSDataBuilder(ProviderDecoratedExecutionContext parserContext) {
    m_pepProvider = parserContext.getProvider(IPeptideProvider.class);
    m_pepByUniqueKey = new HashMap<>();
    m_precIdByPepUniqueKey = new HashMap<>();
  }


  public ResultSet createResultSet(MSISearch msiSearch, Long rsId, String run, DiaNNResult diaNNResult) {
    List<Precursor> precursors =  diaNNResult.getPrecursorForRun(run);
    Map<String,ProteinMatch> currentRSProtMatchesByAcc = new HashMap<>();
    HashMap<String, Peptide> undefineRSPepByUniqueKey = new HashMap<>();
    HashMap<String, Peptide> currentRSPepByUniqueKey = new HashMap<>();
    HashMap<String, List<PeptideMatch>>  currentRSPepMatchesByPepId = new HashMap<>();
    HashMap<String, List<SequenceMatch>>  currentRSSeqMatchesByPepId = new HashMap<>();
    m_spectraById = new HashMap<>();

    PtmDefinition[] fixedPtms = msiSearch.searchSettings().fixedPtmDefs();
    PtmDefinition[] varPtms = msiSearch.searchSettings().variablePtmDefs();
    logger.debug(" ** Parse {} precursors for Run {} ", precursors.size(), run);
    List<PtmDefinition> allPtms = new ArrayList<>(Arrays.asList(fixedPtms));
    allPtms.addAll(Arrays.asList(varPtms));

    for(Precursor precursor : precursors) {

      PeptideMatch pepMatch = createPeptideMatch(precursor, run, msiSearch, allPtms);
      pepMatch.resultSetId_$eq(rsId);
      Peptide pep = pepMatch.peptide();
      String pepUniquekey = pep.uniqueKey();
      m_precIdByPepUniqueKey.computeIfAbsent(pepUniquekey, k -> new HashSet<>()).add(precursor.getPrecursorId());

      //Save links in map for further updates
      List<PeptideMatch> associatedPepMatches = currentRSPepMatchesByPepId.getOrDefault(pepUniquekey, new ArrayList<>());
      associatedPepMatches.add(pepMatch);
      currentRSPepMatchesByPepId.put(pepUniquekey,associatedPepMatches);
      currentRSPepByUniqueKey.put(pepUniquekey, pep);

      // --- Create ProtMatch data
      Option<SequenceMatchProperties> noOpt =Option.empty();
      SequenceMatch seqM = new SequenceMatch(
             1, 1+pepMatch.peptide().sequence().length(), //start - end
             '?', '?',  //residue bef/after
             false, //isDecoy
             rsId,
             pep.id(), Option.apply(pep),
             pepMatch.id(), Option.apply(pepMatch),
             noOpt );

      if(pep.id()<0) {
        undefineRSPepByUniqueKey.put(pepUniquekey, pep); // should search in datastore

        //Save mapping for update ids after searching pep in datastore
        List<SequenceMatch> associatedSeqMatches = currentRSSeqMatchesByPepId.getOrDefault(pepUniquekey, new ArrayList<>());
        associatedSeqMatches.add(seqM);
        currentRSSeqMatchesByPepId.put(pepUniquekey,associatedSeqMatches);
      }

      for(String protein : precursor.getProteinIds()) {
        Option<Protein> noProtOp = Option.empty();
        Option<ProteinMatchProperties> noPrpOp = Option.empty();

        ProteinMatch protMatch;
        if (currentRSProtMatchesByAcc.containsKey(protein)) {
          protMatch = currentRSProtMatchesByAcc.get(protein);
          List<SequenceMatch> allSeqMatches = Arrays.stream(protMatch.sequenceMatches()).collect(Collectors.toList());
          long nbFoundSeqmatches = allSeqMatches.stream().filter(sm -> (sm.start() == seqM.start() && sm.start() == seqM.start() && sm.peptideId() == seqM.peptideId())).count();
          if(nbFoundSeqmatches ==0) {
            allSeqMatches.add(seqM);
            SequenceMatch[] finalSeqMatches = allSeqMatches.toArray(new SequenceMatch[0]);
            protMatch.sequenceMatches_$eq(finalSeqMatches);
          }
        } else {
          SequenceMatch[] allSeqMatches = new SequenceMatch[1];
          allSeqMatches[0] = seqM;
          protMatch = new ProteinMatch(protein,
                  "",
                  false, //isDecoy,
                  false, //isLastBioSequence,
                  ProteinMatch.generateNewId(),
                  0, //taxon id
                  rsId,
                  0, //proteinId
                  noProtOp, //protein
                  null, //set all or no seqDbs ?
                  null, // geneName
                  0, //score
                  "mascot:ions score", //score type VDS TODO
                  0,
                  allSeqMatches, //sequenceMatches,
                  noPrpOp // ProteinMatchProperties
          );
          currentRSProtMatchesByAcc.put(protein, protMatch);
        } // End ProtMatch not found create one
      }//End for each protein of precursor
    } //End for each precursor

    logger.debug(" ** Search {} peptides from datastore  ", undefineRSPepByUniqueKey.size());
    //Get all Peptides from datastore at once
    ArrayBuffer<Tuple2<String, LocatedPtm[]>> pepInfoBuffer = new ArrayBuffer<>();
    for(Peptide nextPep : undefineRSPepByUniqueKey.values()){
      pepInfoBuffer.$plus$eq(new Tuple2<>(nextPep.sequence(), nextPep.ptms()));
    }

    Seq<Tuple2<String, LocatedPtm[]>> pepToSearch = pepInfoBuffer.toSeq();
    Option<Peptide>[] foundPeps = m_pepProvider.getPeptidesAsOptionsBySeqAndPtms(pepToSearch);
    int nbFoundPep = 0;
    for( Option<Peptide> nextFound : foundPeps ){
      if(nextFound.isDefined()){
        nbFoundPep++;
        Peptide fpep = nextFound.get();
        m_pepByUniqueKey.put(fpep.uniqueKey(), fpep); // save in cache
        currentRSPepByUniqueKey.put(fpep.uniqueKey(), fpep); //udpate pep list to be used by RS

        List<PeptideMatch> associatedPepMatches = currentRSPepMatchesByPepId.get(fpep.uniqueKey());
        Map<Long, PeptideMatch> newAssociatedPepMatchesbyId = new HashMap<>();
        for(PeptideMatch nextPepMatch : associatedPepMatches)
        {
          PeptideMatch newPepMatch = new PeptideMatch(nextPepMatch.id(), 1, nextPepMatch.score(), nextPepMatch.scoreType(), nextPepMatch.charge(),
                  nextPepMatch.deltaMoz(), nextPepMatch.isDecoy(), fpep, nextPepMatch.missedCleavage(), nextPepMatch.fragmentMatchesCount(), nextPepMatch.msQuery(),
                  nextPepMatch.isValidated(), nextPepMatch.resultSetId(), nextPepMatch.cdPrettyRank(), nextPepMatch.sdPrettyRank(), null, null, 0,
                  nextPepMatch.properties(), nextPepMatch.validationProperties());
          newAssociatedPepMatchesbyId.put(newPepMatch.id(), newPepMatch);
        }
        currentRSPepMatchesByPepId.put(fpep.uniqueKey(), newAssociatedPepMatchesbyId.values().stream().toList());

        List<SequenceMatch> associatedSeqMatches = currentRSSeqMatchesByPepId.getOrDefault(fpep.uniqueKey(), new ArrayList<>());
        for(SequenceMatch nextSeqMatch : associatedSeqMatches){
          nextSeqMatch.peptideId_$eq(fpep.id());
          nextSeqMatch.peptide_$eq(Option.apply(fpep));
          nextSeqMatch.bestPeptideMatch_$eq(Option.apply(newAssociatedPepMatchesbyId.get(nextSeqMatch.bestPeptideMatchId())));
        }
      }
    }
    logger.debug(" *** Found {}/{} Peptides in Datastore : ", nbFoundPep, pepToSearch.length());
    List<PeptideMatch> rsPepMatches =  currentRSPepMatchesByPepId.values().stream().flatMap(List::stream).toList();
    logger.debug(" *** Created {} peptide matches and {} protein matches ", currentRSProtMatchesByAcc.size(), currentRSProtMatchesByAcc.size());
    return  createOMResultSet(rsId,  currentRSPepByUniqueKey.values().stream().toList(), rsPepMatches, currentRSProtMatchesByAcc.values().stream().toList(), run, msiSearch);
  }

  public HashMap<Long, Spectrum> getSpectraById(){
    return m_spectraById;
  }

  public HashMap<String, Set<String>> getPrecIdByPepUniqueKey(){
    return m_precIdByPepUniqueKey;
  }

  private PeptideMatch createPeptideMatch(Precursor precursor, String run, MSISearch msiSearch,  List<PtmDefinition> allPtms ){

    // --- Create PepMatch data
    double[] fragMoz = new double[0];
    float[] fragIntensities = new float[0];
    Option<Object> frs = Option.empty(); //TODO get from parseOption
    Option<SpectrumProperties>  spectrumProps = Option.empty(); // no rtinseconds
    long pklId = msiSearch.peakList().id();

    // Parse Fragment Info
    QuantPrecursor qPrec = precursor.getQuantitationForRun(run);
    int fragCount =0;
    int missCleaved = 0;
    float ab = 0f;
    float rtStart = 0;
    float rtStop = 0;
    float score = 0;
    if(qPrec != null) {
      ab = qPrec.getAbundance().floatValue();
      rtStart = qPrec.getRtStart();
      rtStop = qPrec.getRtStop();
      score = Double.valueOf(Math.pow(10, -qPrec.getQValue())).floatValue();
      if (qPrec.getMissCleaved() != null) {
        missCleaved = qPrec.getMissCleaved();
      }
      if(qPrec.getFragments() != null && !qPrec.getFragments().isEmpty()) {
        int fragSize = qPrec.getFragments().size();
        fragMoz = new double[fragSize];
        fragIntensities = new float[fragSize];
        for (int i = 0; i < fragSize; i++) {
          Fragment fragment = qPrec.getFragments().get(i);
          fragMoz[i] = fragment.getMoz();
          fragIntensities[i] = fragment.getQuantAb();
        }
        fragCount = fragSize;
      }
    }
    Spectrum spectrum = new Spectrum(
            Spectrum.generateNewId(),
            "diaNN result "+run+" "+precursor.getLibIndex(),
            precursor.getMoz(),
            ab,
            precursor.getCharge(),
            false, 0,0,0,0,rtStart, rtStop,
            Option.apply(fragMoz), Option.apply(fragIntensities), fragMoz.length, frs, pklId,
            spectrumProps);

    Ms2Query query =  new Ms2Query(Ms2Query.generateNewId(), precursor.getLibIndex(),
            precursor.getMoz(), precursor.getCharge(),
            spectrum.title(),spectrum.id(), msiSearch.id(), Option.empty());

    m_spectraById.put(spectrum.id(), spectrum);

    //Peptide pep = createPeptide(precursor, allPtms);
    List<Object>  pepKeyToPtms = getPeptideKeyAndPtms(precursor, allPtms);
    //-- Verify if already parsed
    Peptide pep;
    if(m_pepByUniqueKey.containsKey((String) pepKeyToPtms.get(0))) {
      pep =  m_pepByUniqueKey.get((String) pepKeyToPtms.get(0));
    } else {
      pep = new Peptide(precursor.getSequence(), ((List<LocatedPtm>) pepKeyToPtms.get(1)).toArray(new LocatedPtm[0]));
      m_pepByUniqueKey.put((String) pepKeyToPtms.get(0), pep);
    }

    Option<PeptideMatchResultSummaryProperties> summaryProperties = Option.empty();
    Option<PeptideMatchProperties>  pepMProperties = Option.empty();

    return new PeptideMatch(PeptideMatch.generateNewId(), 1, score, PeptideMatchScoreType.MASCOT_IONS_SCORE(),
            precursor.getCharge(), 0.0f, false, pep, missCleaved,  fragCount, query,
            true, 0, 1,1, null, null, 0, pepMProperties,summaryProperties);
  }

  private List<Object> getPeptideKeyAndPtms(Precursor precursor, List<PtmDefinition> allPtms){

    List<Object> keyAndPtms = new ArrayList<>();
    String key = precursor.getSequence();
    List<DiaNNPepModification> modifs = DiaNNPepModification.parseModification(precursor.getModifiedSequence());

    if(modifs.isEmpty()) {
      keyAndPtms.add(key + "%");
      keyAndPtms.add(new ArrayList<>());
    } else {
      //create Ptm list
      List<LocatedPtm> ptms = new ArrayList<>();
      for (DiaNNPepModification diaNNModif : modifs) {
        if (diaNNModif.isUnimodModif()) {
          if (diaNNModif.getUnimodId() == 0)
            continue;
          List<PtmDefinition> foundPtms = allPtms.stream().filter(def -> def.unimodId() == diaNNModif.getUnimodId()).toList();
          ptms.add(LocatedPtm.apply(foundPtms.get(0), diaNNModif.getModifLocation()));
        }
      }

      String ptmStr = Peptide.makePtmString(ptms.toArray(new LocatedPtm[0]));
      keyAndPtms.add(key + "%"+ptmStr);
      keyAndPtms.add(ptms);
    }

    return  keyAndPtms;
  }

  private ResultSet createOMResultSet(Long rsId,List<Peptide> peptides,  List<PeptideMatch> pepMatches, List<ProteinMatch> protMatches, String rsName, MSISearch msiSearch){
    return new ResultSet(peptides.toArray(new Peptide[0]),
            pepMatches.toArray(new PeptideMatch[0]),
            protMatches.toArray(new ProteinMatch[0]),
            false, //isDecoy
            true, //isSearchResult
            false,//isValidatedContent
            0, //mergedResultSummaryId
            rsId,
            rsName,//RS Name
            "DiaNN result", //RS Description
            false, //isQuantified
            msiSearch.id(),
            Option.apply(msiSearch),
            new MSISearch[0],
            0,
            Option.empty(), //DecoyRS
            Option.empty());
  }
}
