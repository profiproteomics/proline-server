package fr.proline.module.exporter.dataset.view

import fr.proline.module.exporter.api.view.IRecordBuildingContext
import fr.proline.module.exporter.commons.config.ExportConfigConstant._

abstract class AbstractProtSetToPepMatchView extends AbstractProtSetToTypicalProtMatchView {
  
  protected val pepMatchFieldSet = Set(
    FIELD_PSM_PEPTIDE_ID,
    FIELD_PSM_SEQUENCE,
    FIELD_PSM_MODIFICATIONS,
    FIELD_PSM_ID,
    FIELD_PSM_SCORE,
    FIELD_PSM_CALCULATED_MASS,
    FIELD_PSM_CHARGE,
    FIELD_PSM_EXPERIMENTAL_MOZ,
    FIELD_PSM_DELTA_MOZ,
    FIELD_PSM_RT,
    FIELD_PSM_PEPTIDE_LENGTH,
    FIELD_PSM_INITIAL_QUERY_ID,
    FIELD_PSM_MISSED_CLEAVAGES,
    FIELD_PSM_RANK,
    FIELD_PSM_CD_PRETTY_RANK,
    FIELD_PSM_FRAGMENT_MATCHES_COUNT,
    FIELD_PSM_SPECTRUM_TITLE,
    FIELD_PSM_NB_PROTEIN_SETS,
    FIELD_PSM_NB_SAMESET_PROTEIN_MATCHES,
    FIELD_PSM_NB_PROTEIN_MATCHES,
    FIELD_PSM_NB_DATABANK_PROTEIN_MATCHES,
    FIELD_PSM_START,
    FIELD_PSM_END,
    FIELD_PSM_RESIDUE_BEFORE,
    FIELD_PSM_RESIDUE_AFTER,
    FIELD_PSM_PTM_SCORE,
    FIELD_PSM_PTM_SITES_CONFIDENCE
  )
  
  protected val pepMatchFieldsConfigs = sheetConfig.fields.filter( f => pepMatchFieldSet.contains(f.id) )

  override def buildRecord(buildingContext: IRecordBuildingContext): Map[String, Any] = {

    val pepMatchBuildingCtx = buildingContext.asInstanceOf[PepMatchBuildingContext]
    val protMatchBuildingCtx = pepMatchBuildingCtx.protMatchBuildingCtx.get
    val protMatchRecord = super.buildRecord(protMatchBuildingCtx)
    
    val pepMatch = pepMatchBuildingCtx.pepMatch
    val seqMatch = pepMatchBuildingCtx.seqMatch
    val peptide = pepMatch.peptide
    val msQueryOpt = Option(pepMatch.getMs2Query)
    
    // Retrieve retention time mapping
    val spectrumDescriptorByMsQueryId = identDS.spectrumDescriptorByMsQueryId
    
    val retentionTimeOpt = if (msQueryOpt.isEmpty) None
    else {
      spectrumDescriptorByMsQueryId.get(pepMatch.msQuery.id).map { spectrumMetdata =>
        dcf4.format(spectrumMetdata.firstTime)
      }
    }
    
    val ptmSitePropsOpt = pepMatch.properties.flatMap( _.getPtmSiteProperties )
    val ptmScoreOpt = ptmSitePropsOpt.flatMap(_.getMascotDeltaScore)
    
    val ptmSitesOpt = if (ptmSitePropsOpt.isEmpty) None
    else {
      val ptmSiteProperties = ptmSitePropsOpt.get

      if (ptmSiteProperties.getMascotProbabilityBySite.isEmpty) None
      else {
        val sites = ptmSiteProperties.getMascotProbabilityBySite.get.map { case (k, v) =>
          k + " = " + dcf2.format(v)
        }
        Some( sites.mkString(",") )
      }
    }
    
    val recordBuilder = Map.newBuilder[String,Any]
    recordBuilder ++= protMatchRecord

    for (fieldConfig <- pepMatchFieldsConfigs) {
      val fieldValue: Any = fieldConfig.id match {
        case FIELD_PSM_PEPTIDE_ID => peptide.id
        case FIELD_PSM_SEQUENCE => peptide.sequence
        case FIELD_PSM_MODIFICATIONS => peptide.readablePtmString
        case FIELD_PSM_ID => pepMatch.id
        case FIELD_PSM_SCORE => decimalFormat.format(pepMatch.score)
        case FIELD_PSM_CALCULATED_MASS => dcf6.format(peptide.calculatedMass)
        case FIELD_PSM_CHARGE => msQueryOpt.map(_.charge).orNull
        case FIELD_PSM_EXPERIMENTAL_MOZ => dcf6.format(msQueryOpt.map(_.moz).orNull)
        case FIELD_PSM_DELTA_MOZ => dcf6.format(pepMatch.deltaMoz)
        case FIELD_PSM_RT => retentionTimeOpt.orNull
        case FIELD_PSM_PEPTIDE_LENGTH => peptide.sequence.length
        case FIELD_PSM_INITIAL_QUERY_ID => msQueryOpt.map(_.initialId).orNull
        case FIELD_PSM_MISSED_CLEAVAGES => pepMatch.missedCleavage
        case FIELD_PSM_RANK => pepMatch.rank
        case FIELD_PSM_CD_PRETTY_RANK => pepMatch.cdPrettyRank
        case FIELD_PSM_FRAGMENT_MATCHES_COUNT => pepMatch.fragmentMatchesCount
        case FIELD_PSM_SPECTRUM_TITLE => msQueryOpt.map(_.spectrumTitle).orNull
        case FIELD_PSM_NB_PROTEIN_SETS => identDS.validProtSetIdSetByPepMatchId.get(pepMatch.id).map(_.size).getOrElse(0)
        case FIELD_PSM_NB_SAMESET_PROTEIN_MATCHES => identDS.validSamesetProtMatchIdSetByPepMatchId.get(pepMatch.id).map(_.size).getOrElse(0)
        case FIELD_PSM_NB_PROTEIN_MATCHES => identDS.validProtMatchIdSetByPepMatchId.get(pepMatch.id).map(_.size).getOrElse(0)
        case FIELD_PSM_NB_DATABANK_PROTEIN_MATCHES => identDS.allProtMatchSetByPepId.get(peptide.id).map(_.size).getOrElse(0)
        case FIELD_PSM_START => seqMatch.start
        case FIELD_PSM_END => seqMatch.end
        case FIELD_PSM_RESIDUE_BEFORE => if (seqMatch.residueBefore == '\0') '-' else seqMatch.residueBefore
        case FIELD_PSM_RESIDUE_AFTER => if (seqMatch.residueAfter == '\0') '-' else seqMatch.residueAfter
        case FIELD_PSM_PTM_SCORE => ptmScoreOpt.map( dcf2.format(_) ).orNull
        case FIELD_PSM_PTM_SITES_CONFIDENCE => ptmSitesOpt.orNull
      }
      
      recordBuilder += fieldConfig.title -> fieldValue
    }
    
    recordBuilder.result()
  }

}

