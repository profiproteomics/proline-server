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

object TypicalProtMatchToAllPepMatchesViewFields extends IProtSetToToTypicalProtMatchViewFields with IPeptideMatchViewFields 

// TODO: try to merge this view with ProtSetToBestPepMatchView
// Maybe ProtSetToBestPepMatchView could inherit from an AbstractAllPepMatchesView ???
class TypicalProtMatchToAllPepMatchesView( override val identDS: IdentDataSet ) extends AbstractProtSetToTypicalProtMatchView {
   
  override val fields = TypicalProtMatchToAllPepMatchesViewFields
  
  override var viewName = "all_peptide_matches"

  protected def buildPepMatchRecord(
    protMatchRecord: Map[String,Any],
    pepMatch: PeptideMatch,
    seqMatch: SequenceMatch
  ): Map[String,Any] = {

    val peptide = pepMatch.peptide
    val initialQueryId = Option(pepMatch.msQuery).map(_.initialId).getOrElse(null)
    val experimentalMoz = Option(pepMatch.msQuery).map(_.moz).getOrElse(null)
    
    val resBefore = if( seqMatch.residueBefore == '\0' ) '-' else seqMatch.residueBefore
    val resAfter = if( seqMatch.residueAfter == '\0' ) '-' else seqMatch.residueAfter
    
    // Build the full record
    protMatchRecord ++ Map(
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
      fields.PROTEIN_SETS_COUNT -> identDS.validProtSetIdSetByPepMatchId.get(pepMatch.id).map(_.size).getOrElse(0),
      fields.PROTEIN_MATCHES_COUNT -> identDS.validProtMatchIdSetByPepMatchId.get(pepMatch.id).map(_.size).getOrElse(0),
      fields.START -> seqMatch.start,
      fields.END -> seqMatch.end,
      fields.RESIDUE_BEFORE -> resBefore,
      fields.RESIDUE_AFTER -> resAfter
    ).map( r => r._1.toString -> r._2)
    
  }
  

    // TODO: override buildRecord instead ???
  override def formatRecord(
    buildingContext: IRecordBuildingContext,
    recordFormatter: Map[String,Any] => Unit
  ): Unit = {
    
    // Cast the building context
    val allPepMatchesBuildingCtx = buildingContext.asInstanceOf[PepMatchBuildingContext]
    
    // Build a protein match record if the protein set building context is defined
    val protSetBuildingCtxOpt = allPepMatchesBuildingCtx.protMatchBuildingCtx
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
    
    // Build the peptide match record
    val pepMatchRecord = this.buildPepMatchRecord(
      protMatchRecord,
      allPepMatchesBuildingCtx.pepMatch,
      allPepMatchesBuildingCtx.seqMatch
    )
    
    // Give the record to the formatter
    recordFormatter( pepMatchRecord )
  }
  
  override def onEachRecord( recordFormatter: Map[String,Any] => Unit ) {
    
    val rsm = identDS.resultSummary
    val rs = rsm.resultSet.get
    val protMatchById = rs.getProteinMatchById
    val pepMatchById = rs.getPeptideMatchById
    
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
      
      val protMatchBuildingCtx = new ProtMatchBuildingContext(
        protSet,
        protSet.peptideSet,
        typicalProtMatch
      )
      
      typicalProtMatch.sequenceMatches.foreach { seqMatch =>
        
        val pepMatchOpt = pepMatchById.get( seqMatch.bestPeptideMatchId )
        
        for( pepMatch <- pepMatchOpt ) {
          exportedPepMatchIds += pepMatch.id
          
          val buildingContext = new PepMatchBuildingContext(
            pepMatch = pepMatch,
            protMatch = typicalProtMatch,
            seqMatch = seqMatch,
            protMatchBuildingCtx = Some(protMatchBuildingCtx)
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
        
        val buildingContext = new PepMatchBuildingContext(
          pepMatch = pepMatch,
          protMatch = protAndSeqMatch._1,
          seqMatch = protAndSeqMatch._2
        )
        
        // Format this unassigned peptide match
        this.formatRecord( buildingContext, recordFormatter )
      }
    }
    
  }

}