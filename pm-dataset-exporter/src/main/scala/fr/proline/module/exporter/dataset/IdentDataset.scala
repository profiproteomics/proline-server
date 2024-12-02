package fr.proline.module.exporter.dataset

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.LongMap
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.collection._
import fr.proline.core.om.model.msi._

import scala.collection.mutable.ArrayBuffer

// TODO: add this status to OM ?
object ProteinMatchStatus extends Enumeration {
  val SAMESET = Value("Sameset")
  val SUBSET = Value("Subset")
  val REPRESENTATIVE = Value("Representative")
}

class IdentDataset(
                    val projectName: String,
                    val resultSummary: LazyResultSummary,
                    protected val loadLeaveResultSummaries: () => Array[LazyResultSummary],
                    protected val loadLeaveResultSets: () => Array[LazyResultSet], // TODO: remove me => we don't need this, it should be identical to loadChildResultSummaries
                    protected val loadBioSequences: () => Array[BioSequence],
                    protected val loadSpectraDescriptors: (Array[Long]) => Array[Spectrum],
                    val loadPepMatches: (Array[Long]) => Array[PeptideMatch],
                    val loadPeptides: (Array[Long]) => Array[Peptide],
                    val loadPtmDataset: () => Option[PtmDataSet],
                    val datasetID: Long
) extends LazyLogging {

  // Count the number of protein sets and proteins matches related to a given peptide match
  val validProtSetsByPeptideId = new LongMap[HashSet[ProteinSet]]()
  val validSamesetProtMatchIdSetByPepMatchId = new LongMap[HashSet[Long]]()
  val validProtMatchIdSetByPepMatchId = new LongMap[HashSet[Long]]()

  // Init Maps
  resultSummary.proteinSets.withFilter(_.isValidated).foreach { protSet =>

    val samesetProtMatchIdSet = protSet.getSameSetProteinMatchIds.toSet

    protSet.peptideSet.getPeptideMatchIds.foreach { pepMatchId =>
      validSamesetProtMatchIdSetByPepMatchId.getOrElseUpdate(pepMatchId, new HashSet[Long]) ++= samesetProtMatchIdSet
      validProtMatchIdSetByPepMatchId.getOrElseUpdate(pepMatchId, new HashSet[Long])
    }
    //#19643 use peptide Ids instead if psmId (may need information from leaf psm (in case of quant peptide ion for example)
    protSet.peptideSet.getPeptideIds.foreach {pepId =>
      validProtSetsByPeptideId.getOrElseUpdate(pepId, new HashSet[ProteinSet]) += protSet
    }

    val protMatchIdByPepSet = protSet.getAllProteinMatchesIdByPeptideSet
    for(
      (pepSet,protMatchIds) <- protSet.getAllProteinMatchesIdByPeptideSet;
      pepMatchId <- pepSet.getPeptideMatchIds
    ) {
      validProtMatchIdSetByPepMatchId(pepMatchId) ++= protMatchIds
    }
  }

  lazy val peptideMatchById: LongMap[PeptideMatch] = resultSummary.lazyResultSet.peptideMatches.mapByLong(_.id)

  // Create a map of all ProtMatches for pepMatches (validated or not)
  lazy val allProtMatchSetByPepId: LongMap[HashSet[ProteinMatch]] = {
    val protMatchSetByPepId = new LongMap[HashSet[ProteinMatch]]

    for (protMatch <- resultSummary.lazyResultSet.proteinMatches) {

      if (protMatch.sequenceMatches != null) {
        protMatch.sequenceMatches.foreach { seqMatch =>
          protMatchSetByPepId.getOrElseUpdate(seqMatch.getPeptideId, new HashSet[ProteinMatch]) += protMatch
        }
      }

    } // End of go through protein matches

    protMatchSetByPepId
  }

  lazy val childResultSummaries: Array[LazyResultSummary] = loadLeaveResultSummaries()
  lazy val allResultSummaries: Array[LazyResultSummary] = Array(resultSummary) ++ childResultSummaries
  lazy val leavesResultSets : Array[LazyResultSet] = loadLeaveResultSets()

  lazy val spectrumDescriptorByMsQueryId: Map[Long, Spectrum] = {
    val msQueries = new ArrayBuffer[Ms2Query]()
    val spectraIds =new ArrayBuffer[Long]()
    for(
      peptideMatch <- resultSummary.lazyResultSet.peptideMatches
      if peptideMatch.msQuery != null && peptideMatch.msQuery.isInstanceOf[Ms2Query]
    ){
      val ms2Query = peptideMatch.getMs2Query()
      spectraIds += ms2Query.spectrumId
      msQueries += ms2Query
    }

    val spectraDescriptorById: LongMap[Spectrum] = loadSpectraDescriptors(spectraIds.toArray).mapByLong(_.id)
    msQueries.map(q=>{
      q.id -> spectraDescriptorById(q.spectrumId)
    }).toMap
  }

  lazy val bioSequenceById: LongMap[BioSequence] = loadBioSequences().mapByLong(_.id)
  
  private val protMatchStatusMapByLazyRsm = new HashMap[LazyResultSummary,LongMap[ProteinMatchStatus.Value]]
  
  def getProteinMatchStatus( lazyRsm: LazyResultSummary, protMatchId: Long ): Option[ProteinMatchStatus.Value] = {
    val protMatchStatusMapOpt = protMatchStatusMapByLazyRsm.get(lazyRsm)
    
    val protMatchStatusMap = if( protMatchStatusMapOpt.isDefined) protMatchStatusMapOpt.get
    else {
      val protMatchStatusById = new LongMap[ProteinMatchStatus.Value](lazyRsm.peptideSets.length)
      
      for(protSet <- lazyRsm.proteinSets) {
        
        // TODO: add this methods to ProteinSet class ?
        val reprMatchId = protSet.getRepresentativeProteinMatchId()
        if( reprMatchId != 0 ) {
          protMatchStatusById += reprMatchId -> ProteinMatchStatus.REPRESENTATIVE
        }
        
        for( samesetId <- protSet.getSameSetProteinMatchIds; if samesetId != reprMatchId ) {
          protMatchStatusById += samesetId -> ProteinMatchStatus.SAMESET
        }
        
        for( subsetId <- protSet.getSubSetProteinMatchIds ) {
          protMatchStatusById += subsetId -> ProteinMatchStatus.SUBSET
        }
      }
      
      // Cache the built status mapping
      protMatchStatusMapByLazyRsm.put(lazyRsm, protMatchStatusById)
    
      protMatchStatusById
    }
    
    protMatchStatusMap.get(protMatchId)
  }
  
  private val protMatchAcMappingByLazyRsm = new HashMap[LazyResultSummary,Map[String,ProteinMatch]]
  
  def getIdentifiedProteinMatchByAc( lazyRsm: LazyResultSummary, protMatchAc: String ): Option[ProteinMatch] = {
    val protMatchAcMappingOpt = protMatchAcMappingByLazyRsm.get(lazyRsm)
    
    val protMatchAcMapping = if(protMatchAcMappingOpt.isDefined) protMatchAcMappingOpt.get
    else {
      val newProtMatchAcMapping = lazyRsm.lazyResultSet.proteinMatches.map( pm => pm.accession -> pm ).toMap
      
      // Cache the built AC mapping
      protMatchAcMappingByLazyRsm.put(lazyRsm, newProtMatchAcMapping)
      
      newProtMatchAcMapping
    }
    
    protMatchAcMapping.get(protMatchAc)
  }
  
}