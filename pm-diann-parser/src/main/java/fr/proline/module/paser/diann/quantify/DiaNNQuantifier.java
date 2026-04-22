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

package fr.proline.module.paser.diann.quantify;

import fr.proline.context.IExecutionContext;
import fr.proline.core.om.model.msi.PeptideInstance;
import fr.proline.core.om.model.msi.ProteinSet;
import fr.proline.core.om.model.msi.ResultSet;
import fr.proline.core.om.model.msi.ResultSummary;
import fr.proline.core.om.model.msq.*;
import fr.proline.core.orm.msi.ObjectTreeSchema;
import fr.proline.core.orm.msi.repository.ObjectTreeSchemaRepository;
import fr.proline.core.orm.uds.MasterQuantitationChannel;
import fr.proline.core.service.msq.quantify.AbstractDiannQuantifier;
import fr.proline.module.paser.diann.model.DiaNNResult;
import fr.proline.module.paser.diann.model.Precursor;
import fr.proline.module.paser.diann.model.QuantPrecursor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.Option;
import scala.collection.JavaConverters;
import scala.collection.mutable.LongMap;

import javax.persistence.EntityManager;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class DiaNNQuantifier extends AbstractDiannQuantifier {
  private static final Logger logger = LoggerFactory.getLogger(DiaNNQuantifier.class);

  DiaNNResult m_diannResult;
  IExecutionContext m_executionContext;
  Map<String, ResultSet> m_allResultSetsByRun;
  ResultSummary m_mergedResultSummary;
  MasterQuantChannel m_masterQuantChannel;
  MasterQuantitationChannel m_ormMasterQuantChannel;
  Map<String, Set<String>> m_precIdByPepKey;

  public DiaNNQuantifier(MasterQuantChannel masterQuantChannel, Map<String, ResultSet> rsByName  , ResultSummary mergedRsm, DiaNNResult diannResult, Map<String, Set<String>> precIdByPepKey , IExecutionContext execContext) {
    super(execContext, execContext.getUDSDbConnectionContext().getEntityManager().find(MasterQuantitationChannel.class, masterQuantChannel.id()), new ExperimentalDesign(new BiologicalSample[0], new GroupSetup[0],new MasterQuantChannel[] {masterQuantChannel} ));
    this.m_allResultSetsByRun = rsByName;
    this.m_mergedResultSummary = mergedRsm;
    this.m_diannResult = diannResult;
    this.m_executionContext = execContext;
    this.m_masterQuantChannel = masterQuantChannel;
    this.m_precIdByPepKey = precIdByPepKey;
  }


  public void quantifyMasterChannel() {
    // Compute and store quant entities (MQ Peptides, MQ ProteinSets)
    List<MasterQuantPeptide> mQuantPeps = computeMasterQuantPeptides();
    List<MasterQuantProteinSet>  masterQuantProteinSets = computeMasterQuantProteinSets(mQuantPeps);

    EntityManager msiEM = m_executionContext.getMSIDbConnectionContext().getEntityManager();
    fr.proline.core.orm.msi.ResultSummary ormMergeRSM = msiEM.find(fr.proline.core.orm.msi.ResultSummary.class, m_mergedResultSummary.id());

    storeMasterQuantPeptidesAndProteinSets(ormMergeRSM, mQuantPeps.toArray(new MasterQuantPeptide[0]), masterQuantProteinSets.toArray(new MasterQuantProteinSet[0]));
  }


  private List<MasterQuantProteinSet> computeMasterQuantProteinSets(List<MasterQuantPeptide> mQuantPeps ) {
    List<MasterQuantProteinSet> mqProtSets =  new ArrayList<>();
    Map<Long, MasterQuantPeptide> mqPepByPepInstId = new HashMap<>();
    for(MasterQuantPeptide pep : mQuantPeps) {
      mqPepByPepInstId.put(pep.peptideInstance().get().id(), pep);
    }

    for(ProteinSet protSet : this.m_mergedResultSummary.proteinSets()){
      scala.collection.mutable.HashMap<Object,Object> pepIdSelection = new scala.collection.mutable.HashMap<>();

      Map<Long, Float> abPerQChannel = new HashMap<>();
      Map<Long, Integer> pepMatchPerQChannel = new HashMap<>();
      Map<Long, Integer> pepPerQChannel = new HashMap<>();
      LongMap<QuantProteinSet> qProtSetByQChId = new LongMap<>();
      List<MasterQuantPeptide> protMqPeptides = new ArrayList<>();
      for(PeptideInstance pi : protSet.peptideSet().getPeptideInstances()){
        MasterQuantPeptide mqPep = mqPepByPepInstId.get(pi.id());
        protMqPeptides.add(mqPep);
        pepIdSelection.put(mqPep.id(), 2);
        Map<Object, QuantPeptide> pepByQch = JavaConverters.mapAsJavaMap(mqPep.quantPeptideMap());
        for (Object key : pepByQch.keySet()){
          QuantPeptide qp = pepByQch.get(key);
          Long qChId = (Long) key;

          Float ab = abPerQChannel.getOrDefault(qChId, 0.0f);
          ab += qp.abundance();
          abPerQChannel.put(qChId,ab);
          Integer nbPM = pepMatchPerQChannel.getOrDefault(qChId,0);
          nbPM += qp.peptideMatchesCount();
          pepMatchPerQChannel.put(qChId,nbPM);
          Integer nbPep = pepPerQChannel.getOrDefault(qChId,0);
          nbPep += 1;
          pepPerQChannel.put(qChId,nbPep);
        }
      }

      //for each qch
      for(Long qChId : abPerQChannel.keySet()) {
        QuantProteinSet qProtSet = new QuantProteinSet(abPerQChannel.get(qChId),abPerQChannel.get(qChId),pepMatchPerQChannel.get(qChId),
                Option.apply(pepPerQChannel.get(qChId)),  qChId, Option.apply(protSet.id()), Option.empty(), 2);

        qProtSetByQChId.put(qChId, qProtSet);
      }

      MasterQuantProteinSetProperties prop = new MasterQuantProteinSetProperties(false, null, pepIdSelection, null );
      MasterQuantProteinSet mqProtSet = new MasterQuantProteinSet(protSet, qProtSetByQChId,protMqPeptides.toArray(new MasterQuantPeptide[0]), 2, Option.apply(prop));
      mqProtSets.add(mqProtSet);
    }

    return mqProtSets;
  }

  private List<MasterQuantPeptide> computeMasterQuantPeptides() {
    List<MasterQuantPeptide> masterQuantPeptides = new ArrayList<>();

    Map<String, QuantChannel> qChByRun = new HashMap<>();
    Arrays.stream(this.m_masterQuantChannel.quantChannels()).forEach(qch -> qChByRun.put(qch.name(), qch));

    for(PeptideInstance pi : this.m_mergedResultSummary.peptideInstances()){
      List<MasterQuantPeptideIon> masterQuantPeptidesIon = new ArrayList<>();

      String pepKey = pi.peptide().uniqueKey();
      long mQPepId = MasterQuantPeptide.generateNewId();
      Set<String> precIds = m_precIdByPepKey.get(pepKey);
      Map<Long, List<QuantPeptideIon>> allIonsByQc = new HashMap<>(precIds.size());
      for(String precId : precIds){ //For each "ions" associated to this peptide instance in each runs
        Precursor prec = m_diannResult.getPrecursorForId(precId);
        LongMap<QuantPeptideIon> currentIonsByQc = new LongMap<>();
//        double bestQPrecScore = 0L;
        for(String run : prec.getQuantifiedRuns()){
          QuantPrecursor qPrec = prec.getQuantitationForRun(run);
//          if(bestQPrecScore < qPrec.getQValue()){
//            bestQPrecScore = qPrec.getQValue();
//          }
          long qChId =  qChByRun.get(run).id();
          //for each QuantChannel
          QuantPeptideIon qpepion = new QuantPeptideIon(qPrec.getAbundance().floatValue(),qPrec.getAbundance().floatValue(),
                  prec.getMoz(), qPrec.getRt(),(qPrec.getRtStop()- qPrec.getRtStart()), qPrec.getPredictedRT(),
                  prec.getLibIndex(), 1, Option.empty(), Option.apply(qPrec.getQValue().floatValue()),Option.empty(),
                  qChId, Option.apply(pi.peptideId()), Option.apply(pi.id()), Option.empty(),
                  Option.empty(),Option.empty(),Option.empty(), 2, Option.apply(Boolean.TRUE));
          allIonsByQc.computeIfAbsent(qChId, k -> new ArrayList<>()).add(qpepion);
          currentIonsByQc.put(qChId, qpepion);
        }//for each qCh where ion is quantified



        MasterQuantPeptideIon mqPepion = new MasterQuantPeptideIon(MasterQuantPeptideIon.generateNewId(), prec.getMoz(), prec.getCharge(),
                  0, currentIonsByQc.size(), Option.empty(),2, mQPepId, m_mergedResultSummary.id(),
                  Option.apply(pi.id()), Option.apply(pi.bestPeptideMatchId()), Option.empty(), Option.empty(), currentIonsByQc, Option.empty(), new MasterQuantReporterIon[0]);

        masterQuantPeptidesIon.add(mqPepion);

      } // for each ion of current pepInstance

      //Create quantPepide for each run
      LongMap<QuantPeptide> allQP = new LongMap<>();
      for(Long qId : allIonsByQc.keySet()){
        List<QuantPeptideIon> pepIons = allIonsByQc.get(qId);
        float ab = (float)pepIons.stream().mapToDouble(QuantPeptideIon::abundance).sum();
        QuantPeptide qpep = new QuantPeptide(ab, ab, 0,pepIons.size(), 2, qId, Option.apply(pi.peptideId()), Option.apply(pi.id()));
        allQP.put(qId, qpep);
      }

      MasterQuantPeptide mqPep = new MasterQuantPeptide(mQPepId, Option.apply(pi), allQP, masterQuantPeptidesIon.toArray(new MasterQuantPeptideIon[0]),2, m_mergedResultSummary.id(), Option.empty());
      masterQuantPeptides.add(mqPep);

    } // End for each peptide instance

    return masterQuantPeptides;
  }

  @Override
  public ObjectTreeSchema quantPeptidesObjectTreeSchema() {
    return ObjectTreeSchemaRepository.loadOrCreateObjectTreeSchema(msiEm(), ObjectTreeSchema.SchemaName.LABEL_FREE_QUANT_PEPTIDES.toString());
  }

  @Override
  public ObjectTreeSchema quantPeptideIonsObjectTreeSchema() {
    return ObjectTreeSchemaRepository.loadOrCreateObjectTreeSchema(msiEm(), ObjectTreeSchema.SchemaName.LABEL_FREE_QUANT_PEPTIDE_IONS.toString());
  }
}
