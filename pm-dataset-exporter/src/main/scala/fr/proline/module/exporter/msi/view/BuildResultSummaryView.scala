package fr.proline.module.exporter.msi.view

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import com.typesafe.scalalogging.LazyLogging
import fr.proline.core.om.model.msi.SpectrumMatch
import fr.proline.core.om.model.msi.ProteinMatch
import fr.proline.core.om.model.msi.ResultSet
import fr.proline.core.om.model.msi.ResultSummary
import fr.proline.core.om.model.msi.Spectrum
import fr.proline.module.exporter.api.view.IDataView
import fr.proline.module.exporter.api.view.IFixedDatasetView
import fr.proline.module.exporter.api.view.IViewTypeEnumeration
import fr.proline.core.om.model.msq.MasterQuantPeptide

case class MsiIdentDataSet (
  projectName: String,
  resultSummary: ResultSummary,
  // TODO: represent the result set hierarchy here (merge) ???
  childsResultSummarys: Array[ResultSummary],
  childsResultSets: Array[ResultSet]
) extends LazyLogging {

  // Count the number of protein sets and proteins matches related to a given peptide match
  val validProtSetIdSetByPepMatchId = new HashMap[Long,HashSet[Long]]()
  val validProtMatchIdSetByPepMatchId = new HashMap[Long,HashSet[Long]]()
    
  // Init Maps 
  resultSummary.proteinSets.filter(_.isValidated).foreach( protSet =>{
	  
	  protSet.peptideSet.getPeptideMatchIds.foreach(validProtSetIdSetByPepMatchId.getOrElseUpdate(_, new HashSet[Long] ) += protSet.id )
    
	  val protMatchIdByPepSet =  protSet.getAllProteinMatchesIdByPeptideSet
	  protMatchIdByPepSet.foreach( entry =>{
		  val pepSet = entry._1
		  for(prMId <- entry._2; 
			  pepMatchId<-pepSet.getPeptideMatchIds) {
			  	validProtMatchIdSetByPepMatchId.getOrElseUpdate(pepMatchId, new HashSet[Long] ) += prMId
  			}
	  })
  })

    
  lazy val pepMatchById = resultSummary.resultSet.get.getPeptideMatchById
  
  //   Create list of all ProtMatches for pepMatches (validated or not) 
  lazy val allProtMatchSetByPepId =  { 
    val resultBuilder = new HashMap[Long, HashSet[ProteinMatch]]
    for( protMatch <- resultSummary.resultSet.get.proteinMatches ) {
      if (protMatch.sequenceMatches != null) {
        protMatch.sequenceMatches.foreach(seqMatch => {
          val pepId = seqMatch.getPeptideId
          resultBuilder.getOrElseUpdate(seqMatch.getPeptideId, new HashSet[ProteinMatch]) +=protMatch          
        }) //end go throudh protMatch sequenceMatches 
      }      
    } //End go through ProtMarches
    resultBuilder
  }
}

object BuildResultSummaryView {
  
  private def _builders: Map[IViewTypeEnumeration#Value, MsiIdentDataSet => IDataView ] = Map( 
    ResultSummaryViewTypes.MSI_SEARCH_EXTENDED -> { ds: MsiIdentDataSet => new MsiSearchExtendedView(ds) },
    ResultSummaryViewTypes.IMPORT_AND_VALIDATION_PROPS -> { ds: MsiIdentDataSet => new ResultParseAndFiltersView(ds.resultSummary) },
    ResultSummaryViewTypes.STATISTICS -> { ds: MsiIdentDataSet => new StatisticsView(ds.resultSummary) },
    ResultSummaryViewTypes.PEP_SET_TO_PROT_MATCH -> { ds: MsiIdentDataSet => new PepSetToProtMatchView(ds) },
    ResultSummaryViewTypes.PROT_SET_TO_PROT_MATCH -> { ds: MsiIdentDataSet => new ProtSetToProtMatchView(ds) },
    ResultSummaryViewTypes.PROT_SET_TO_TYPICAL_PROT_MATCH -> { ds: MsiIdentDataSet => new ProtSetToTypicalProtMatchView(ds) },
    ResultSummaryViewTypes.PROT_SET_TO_BEST_PEPTIDE_MATCH -> { ds: MsiIdentDataSet => new ProtSetToBestPepMatchView(ds) },
    ResultSummaryViewTypes.PROT_SET_TO_ALL_PEPTIDE_MATCHES -> { ds: MsiIdentDataSet => new ProtSetToAllPepMatchesView(ds) },
    ResultSummaryViewTypes.TYPICAL_PROT_MATCH_TO_ALL_PEP_MATCHES -> { ds: MsiIdentDataSet => new TypicalProtMatchToAllPepMatchesView(ds) }    
  )

  def apply( identDS: MsiIdentDataSet, viewType: IViewTypeEnumeration#Value ): IDataView = {
    _builders(viewType)(identDS)
  }
  
  
}

case class IdentWithSpectrumDataSet ( 
  resultSummary: ResultSummary,
  sharedPepMatchIds : Array[Long],
  spectrumByPepMatchId : Map[Long, Spectrum],
  spectrumMatchesByPeptMatchId : HashMap[Long, SpectrumMatch],
  masterQuantPepByPepId :  Option[Map[Long, MasterQuantPeptide]]
)

object BuildRSMSpectraView {
  
  private def _builders: Map[IViewTypeEnumeration#Value, IdentWithSpectrumDataSet => IDataView ] = Map( 
    RSMSpectraViewTypes.SPECTRA_LIST -> { ds: IdentWithSpectrumDataSet => new SpectraListView(ds) }        
  )

  def apply( identDS: IdentWithSpectrumDataSet, viewType: IViewTypeEnumeration#Value ): IDataView = {
    _builders(viewType)(identDS)
  }
  
  
}