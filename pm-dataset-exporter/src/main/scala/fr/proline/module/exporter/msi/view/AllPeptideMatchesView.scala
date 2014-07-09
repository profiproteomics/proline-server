package fr.proline.module.exporter.msi.view

import java.io.File
import java.io.OutputStream
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import fr.proline.context.IExecutionContext
import fr.proline.core.om.provider.msi.impl.SQLResultSummaryProvider
import fr.proline.core.om.model.msi._
import fr.proline.module.exporter.api.template.IViewTemplate
import fr.proline.module.exporter.api.view._

case class AllPepMatchesBuildingContext(
  pepMatch: PeptideMatch,
  protMatch: ProteinMatch,
  seqMatch: SequenceMatch,
  protSetIdSetByPepMatchId: HashMap[Long,HashSet[Long]],
  protMatchIdSetByPepMatchId: HashMap[Long,HashSet[Long]],
  protSetBuildingCtx: Option[ProtSetToToTypicalProtMatchBuildingContext] = None
) extends IRecordBuildingContext

// TODO: try to merge this view with ProtSetToBestPepMatchView
// Maybe ProtSetToBestPepMatchView could inherit from an AbstractAllPepMatchesView ???
class AllPepMatchesView( override val rsm: ResultSummary ) extends AbstractProtSetToTypicalProtMatchView {
  
  override var viewName = "all_peptide_matches"
  override val fields = ProtSetToBestPepMatchViewFields
  private val emptyProtMatchRecord = {
    ProtSetToToTypicalProtMatchViewFields.values.map(_.toString -> null).toMap
  }
  
  override def formatRecord(
    buildingContext: IRecordBuildingContext,
    recordFormatter: Map[String,Any] => Unit
  ): Unit = {
    
    // Cast the building context
    val allPepMatchesBuildingCtx = buildingContext.asInstanceOf[AllPepMatchesBuildingContext]
    val protSetIdSetByPepMatchId = allPepMatchesBuildingCtx.protSetIdSetByPepMatchId
    val protMatchIdSetByPepMatchId = allPepMatchesBuildingCtx.protMatchIdSetByPepMatchId
    
    // Build a protein match record if the protein set building context is defined
    val protSetBuildingCtxOpt = allPepMatchesBuildingCtx.protSetBuildingCtx
    val protMatchRecord = if( protSetBuildingCtxOpt.isEmpty ) {
      val protMatch = allPepMatchesBuildingCtx.protMatch
      Map(
        fields.ACCESSION -> protMatch.accession,
        fields.DESCRIPTION -> protMatch.description,
        //fields.GENE_NAME -> protMatch.geneName,
        //fields.TAXON_ID -> protMatch.taxonId,
        fields.COVERAGE -> protMatch.coverage,
        fields.PEPTIDE_MATCHES_COUNT -> protMatch.peptideMatchesCount,
        fields.MW -> Option(protMatch.protein).map( _.map( _.mass ).getOrElse(0.0) ).getOrElse(0.0)
      ).map( r => r._1.toString -> r._2 )
    }
    else this.buildRecord(protSetBuildingCtxOpt.get)
    
    // Retrieve some values
    val seqMatch = allPepMatchesBuildingCtx.seqMatch
    val pepMatch = allPepMatchesBuildingCtx.pepMatch
    
    // BEGIN OF CODE DUPLICATED WITH ProtSetToBestPepMatchView //
    val peptide = pepMatch.peptide
    val initialQueryId = Option(pepMatch.msQuery).map(_.initialId).getOrElse(null)
    val experimentalMoz = Option(pepMatch.msQuery).map(_.moz).getOrElse(null)
    
    val resBefore = if( seqMatch.residueBefore == '\0' ) '-' else seqMatch.residueBefore
    val resAfter = if( seqMatch.residueAfter == '\0' ) '-' else seqMatch.residueAfter
    
    // Build the full record
    val record = protMatchRecord ++ Map(
      fields.PEPTIDE_ID -> peptide.id,
      fields.SEQUENCE -> peptide.sequence,
      fields.MODIFICATIONS -> peptide.readablePtmString,
      fields.MISSED_CLEAVAGES -> pepMatch.missedCleavage,
      fields.RANK -> pepMatch.rank,
      fields.CD_PRETTY_RANK -> pepMatch.cdPrettyRank,
      fields.PEPMATCH_SCORE -> "%.1f".format(pepMatch.score).toDouble,
      //fields.IS_PSM_VALIDATED -> pepMatch.isValidated,
      fields.CALCULATED_MASS -> peptide.calculatedMass, 
      fields.CHARGE -> Option(pepMatch.msQuery).map(_.charge).getOrElse(null),
      fields.EXPERIMENTAL_MOZ -> experimentalMoz,
      fields.DELTA_MOZ -> pepMatch.deltaMoz, // FIXME: to convert in PPM we need the experimentalMoz and thus the msQuery
      fields.PEPTIDE_LENGTH -> peptide.sequence.length,
      fields.INITIAL_QUERY_ID -> initialQueryId,
      fields.FRAGMENT_MATCHES_COUNT -> pepMatch.fragmentMatchesCount,
      fields.SPECTRUM_TITLE -> Option(pepMatch.getMs2Query).map( _.spectrumTitle ).getOrElse(""),
      fields.PROTEIN_SETS_COUNT -> protSetIdSetByPepMatchId.get(pepMatch.id).map(_.size).getOrElse(0),
      fields.PROTEIN_MATCHES_COUNT -> protMatchIdSetByPepMatchId.get(pepMatch.id).map(_.size).getOrElse(0),
      fields.START -> seqMatch.start,
      fields.END -> seqMatch.end,
      fields.RESIDUE_BEFORE -> resBefore,
      fields.RESIDUE_AFTER -> resAfter
    ).map( r => r._1.toString -> r._2)
    
    // END OF CODE DUPLICATED WITH ProtSetToBestPepMatchView //
    
    // Give the record to the formatter
    recordFormatter( record )
  }

  override def onEachRecord( recordFormatter: Map[String,Any] => Unit ) {
    
    val rs = rsm.resultSet.get
    val protMatchById = rs.proteinMatchById
    val pepMatchById = rs.peptideMatchById    
    val pepSetById = Map() ++ rsm.peptideSets.map( ps => ps.id -> ps )
    
    // BEGIN OF CODE DUPLICATED WITH AbstractProtSetToTypicalProtMatchView
    // Count the number of protein sets and proteins matches related to a given peptide match
    val protSetIdSetByPepMatchId = new HashMap[Long,HashSet[Long]]()
    val protMatchIdSetByPepMatchId = new HashMap[Long,HashSet[Long]]()
    
    for(
      protSet <- rsm.proteinSets;
      protMatchId <- protSet.getProteinMatchIds;
      pepInst <- protSet.peptideSet.getPeptideInstances;
      pepMatchId <- pepInst.getPeptideMatchIds
    ) {
      protSetIdSetByPepMatchId.getOrElseUpdate(pepMatchId, new HashSet[Long] ) += protSet.id
      protMatchIdSetByPepMatchId.getOrElseUpdate(pepMatchId, new HashSet[Long] ) += protMatchId      
    }
    // END OF CODE DUPLICATED WITH AbstractProtSetToTypicalProtMatchView
    
    // Keep track of peptide matches which are exported in the next loop
    val exportedPepMatchIds = new collection.mutable.HashSet[Long]
    
    // Iterate over RSM protein sets
    for( protSet <- rsm.proteinSets ) {
      // Note that we export only protein matches which are loaded with the RSM
      // The result will depend of provider which have been used
      
      // Typical Protein Match is put first
      val typicalProtMatchId = protSet.getTypicalProteinMatchId
      
      val typicalProtMatch = if( typicalProtMatchId != 0 ) { 
        protMatchById(typicalProtMatchId)
      } else {
        protMatchById( protSet.getSameSetProteinMatchIds.head )
      }
      
      val protSetBuildingContext = new ProtSetToToTypicalProtMatchBuildingContext(
        protSet,
        typicalProtMatch,
        pepMatchById,
        pepSetById,
        protSetIdSetByPepMatchId, // TODO: remove me from here
        protMatchIdSetByPepMatchId // TODO: remove me from here
      )
      
      typicalProtMatch.sequenceMatches.foreach { seqMatch =>
        
        val pepMatchOpt = pepMatchById.get( seqMatch.bestPeptideMatchId )
        
        for( pepMatch <- pepMatchOpt ) {
          exportedPepMatchIds += pepMatch.id
          
          val buildingContext = new AllPepMatchesBuildingContext(
            pepMatch = pepMatch,
            protMatch = typicalProtMatch,
            seqMatch = seqMatch,
            protSetIdSetByPepMatchId,
            protMatchIdSetByPepMatchId,
            protSetBuildingCtx = Some(protSetBuildingContext)
          )
          
          // Format this peptide match with protein set information
          this.formatRecord( buildingContext, recordFormatter )
        }
      }
    }
    
    // Map protein matches and sequences matches by peptide
    val protAndSeqMatchByPepId = ( for(
      protMatch <- rs.proteinMatches;
      seqMatch <- protMatch.sequenceMatches
    ) yield seqMatch.getPeptideId -> Pair(protMatch,seqMatch) ).toMap
    
    // Iterate over all result set peptide matches
    for( pepMatch <- rs.peptideMatches ) {
      
      // Export only peptide matches which have not been already exported
      if( exportedPepMatchIds.contains(pepMatch.id) == false ) {
        
        val protAndSeqMatch = protAndSeqMatchByPepId(pepMatch.peptide.id)
        
        val buildingContext = new AllPepMatchesBuildingContext(
          pepMatch = pepMatch,
          protMatch = protAndSeqMatch._1,
          seqMatch = protAndSeqMatch._2,
          protSetIdSetByPepMatchId = protSetIdSetByPepMatchId,
          protMatchIdSetByPepMatchId = protMatchIdSetByPepMatchId
        )
        
        // Format this unassigned peptide match
        this.formatRecord( buildingContext, recordFormatter )
      }
    }
    
  }

}