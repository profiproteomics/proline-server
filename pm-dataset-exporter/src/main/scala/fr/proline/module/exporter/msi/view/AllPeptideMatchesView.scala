package fr.proline.module.exporter.msi.view

import java.io.File
import java.io.OutputStream
import fr.proline.context.IExecutionContext
import fr.proline.core.om.provider.msi.impl.SQLResultSummaryProvider
import fr.proline.core.om.model.msi._
import fr.proline.module.exporter.api.template.IViewTemplate
import fr.proline.module.exporter.api.view._

case class AllPepMatchesBuildingContext(
  pepMatch: PeptideMatch,
  protMatch: ProteinMatch,
  seqMatch: SequenceMatch,
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
    
    // Build a protein match record if the protein set building context is defined
    val protSetBuildingCtxOpt = allPepMatchesBuildingCtx.protSetBuildingCtx
    val protMatchRecord = if( protSetBuildingCtxOpt.isEmpty ) {
      val protMatch = allPepMatchesBuildingCtx.protMatch
      Map(
        fields.ACCESSION -> protMatch.accession,
        fields.DESCRIPTION -> protMatch.description,
        fields.GENE_NAME -> protMatch.geneName,
        fields.TAXON_ID -> protMatch.taxonId,
        fields.COVERAGE -> protMatch.coverage,
        fields.PEPTIDE_MATCHES_COUNT -> protMatch.peptideMatchesCount,
        fields.MW -> Option(protMatch.protein).map( _.map( _.mass ).getOrElse(0.0) ).getOrElse(0.0)
      ).map( r => r._1.toString -> r._2 )
    }
    else this.buildRecord(protSetBuildingCtxOpt.get)

    // Retrieve some values
    val seqMatch = allPepMatchesBuildingCtx.seqMatch
    val pepMatch = allPepMatchesBuildingCtx.pepMatch
    val peptide = pepMatch.peptide
    val experimentalMoz = Option(pepMatch.msQuery).map(_.moz).getOrElse(null)
    
    val pepSeq = peptide.sequence
    val resBefore = if( seqMatch.residueBefore == '\0' ) '-' else seqMatch.residueBefore
    val resAfter = if( seqMatch.residueAfter == '\0' ) '-' else seqMatch.residueAfter
    List(resBefore,peptide.sequence,resAfter).mkString(".")
    
    // Build the full record
    val record = protMatchRecord ++ Map(
      fields.START -> seqMatch.start,
      fields.END -> seqMatch.end,
      fields.SEQUENCE -> pepSeq,
      fields.MODIFICATIONS -> peptide.readablePtmString,
      fields.MISSED_CLEAVAGES -> pepMatch.missedCleavage,
      fields.RANK -> pepMatch.rank,
      fields.PEPMATCH_SCORE -> pepMatch.score,
      fields.CALCULATED_MASS -> peptide.calculatedMass, 
      fields.CHARGE -> Option(pepMatch.msQuery).map(_.charge).getOrElse(null),
      fields.EXPERIMENTAL_MOZ -> experimentalMoz,
      fields.DELTA_MOZ -> pepMatch.deltaMoz,
      fields.FRAGMENT_MATCHES_COUNT -> pepMatch.fragmentMatchesCount,
      fields.SPECTRUM_TITLE -> Option(pepMatch.getMs2Query).map( _.spectrumTitle ).getOrElse("")
    ).map( r => r._1.toString -> r._2)
    
    // Give the record to the formatter
    recordFormatter( record )
  }

  override def onEachRecord( recordFormatter: Map[String,Any] => Unit ) {
    
    val rs = rsm.resultSet.get
    val protMatchById = rs.proteinMatchById
    val pepMatchById = rs.peptideMatchById    
    val pepSetById = Map() ++ rsm.peptideSets.map( ps => ps.id -> ps )
    
    // Keep track of peptide matches which are exported in the next loop
    val exportedPepSet = new collection.mutable.HashSet[Peptide]
    
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
      
      val protSetBuildingContext = new ProtSetToToTypicalProtMatchBuildingContext(protSet,typicalProtMatch,pepMatchById,pepSetById)
      typicalProtMatch.sequenceMatches.foreach { seqMatch =>
        
        val pepMatchOpt = pepMatchById.get( seqMatch.bestPeptideMatchId )
        
        for( pepMatch <- pepMatchOpt ) {
          exportedPepSet += pepMatch.peptide
          
          val buildingContext = new AllPepMatchesBuildingContext(
            pepMatch = pepMatch,
            protMatch = typicalProtMatch,
            seqMatch = seqMatch,            
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
      if( exportedPepSet.contains(pepMatch.peptide) == false ) {
        
        val protAndSeqMatch = protAndSeqMatchByPepId(pepMatch.peptide.id)
        
        val buildingContext = new AllPepMatchesBuildingContext(
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