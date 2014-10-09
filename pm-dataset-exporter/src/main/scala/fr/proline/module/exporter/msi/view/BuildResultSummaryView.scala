package fr.proline.module.exporter.msi.view

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import fr.proline.core.om.model.msi.ResultSummary
import fr.proline.module.exporter.api.view.IDataView
import fr.proline.module.exporter.api.view.IViewTypeEnumeration
import scala.collection.mutable.ArrayBuffer
import fr.proline.core.om.model.msi.ProteinMatch

case class IdentDataSet(
  projectName: String,
  resultSummary: ResultSummary
  // TODO: represent the result set hierarchy here (merge) ???
) {

  // Count the number of protein sets and proteins matches related to a given peptide match
  val protSetIdSetByPepMatchId = new HashMap[Long,HashSet[Long]]()
  val protMatchIdSetByPepMatchId = new HashMap[Long,HashSet[Long]]()
  
  for(
    protSet <- resultSummary.proteinSets;
    protMatchId <- protSet.getProteinMatchIds;
    pepInst <- protSet.peptideSet.getPeptideInstances;
    pepMatchId <- pepInst.getPeptideMatchIds
  ) {
    protSetIdSetByPepMatchId.getOrElseUpdate(pepMatchId, new HashSet[Long] ) += protSet.id
    protMatchIdSetByPepMatchId.getOrElseUpdate(pepMatchId, new HashSet[Long] ) += protMatchId      
    
  }
  
  lazy val pepMatchById = resultSummary.resultSet.get.peptideMatchById
  
  //   Create list of all ProtMatches for pepMatches (validated or not) 
  lazy val allProtMatchSetByPepId =  { 
    val resultBuilder = new HashMap[Long, HashSet[ProteinMatch]]
    for( protMatch <- resultSummary.resultSet.get.proteinMatches ) {
      val protMatchPeptMatches = new ArrayBuffer[Long]()
      if (protMatch.sequenceMatches != null) {
        protMatch.sequenceMatches.foreach(seqMatch => {
          val pepId = seqMatch.getPeptideId
          resultBuilder.getOrElse(seqMatch.getPeptideId, new HashSet[ProteinMatch]) +=protMatch          
        }) //end go throudh protMatch sequenceMatches 
      }      
    } //End go through ProtMarches
    resultBuilder
  }
}

object BuildResultSummaryView {
  
  private def _builders: Map[IViewTypeEnumeration#Value, IdentDataSet => IDataView ] = Map( 
    ResultSummaryViewTypes.MSI_SEARCH_EXTENDED -> { ds: IdentDataSet => new MsiSearchExtendedView(ds) },
    ResultSummaryViewTypes.IMPORT_AND_VALIDATION_PROPS -> { ds: IdentDataSet => new ResultParseAndFiltersView(ds.resultSummary) },
    ResultSummaryViewTypes.STATISTICS -> { ds: IdentDataSet => new StatisticsView(ds.resultSummary) },
    ResultSummaryViewTypes.PEP_SET_TO_PROT_MATCH -> { ds: IdentDataSet => new PepSetToProtMatchView(ds) },
    ResultSummaryViewTypes.PROT_SET_TO_PROT_MATCH -> { ds: IdentDataSet => new ProtSetToProtMatchView(ds) },
    ResultSummaryViewTypes.PROT_SET_TO_TYPICAL_PROT_MATCH -> { ds: IdentDataSet => new ProtSetToTypicalProtMatchView(ds) },
    ResultSummaryViewTypes.PROT_SET_TO_BEST_PEPTIDE_MATCH -> { ds: IdentDataSet => new ProtSetToBestPepMatchView(ds) },
    ResultSummaryViewTypes.ALL_PEPTIDE_MATCHES -> { ds: IdentDataSet => new AllPeptideMatchesView(ds) },
    ResultSummaryViewTypes.ALL_PROT_SET_PEPTIDE_MATCHES -> { ds: IdentDataSet => new AllProtSetPSMView(ds) }
  )

  def apply( identDS: IdentDataSet, viewType: IViewTypeEnumeration#Value ): IDataView = {
    _builders(viewType)(identDS)
  }
  
}