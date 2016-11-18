package fr.proline.module.exporter.dataset.view

import fr.proline.core.om.model.msi._
import fr.proline.module.exporter.api.view.IRecordBuildingContext
import fr.proline.module.exporter.commons.config._
import fr.proline.module.exporter.commons.config.ExportConfigConstant._
import fr.proline.module.exporter.dataset._

abstract class AbstractProtSetToTypicalProtMatchView extends AbstractIdentDatasetView {
  
  protected val protSetFieldSet = Set(
    FIELD_PROTEIN_SETS_ID,
    FIELD_PROTEIN_SETS_ACCESSION,
    FIELD_PROTEIN_SETS_DESCRIPTION,
    FIELD_PROTEIN_SETS_SCORE,
    FIELD_PROTEIN_SETS_IS_VALIDATED,
    FIELD_PROTEIN_SETS_SELECTION_LEVEL,
    FIELD_PROTEIN_SETS_NB_SAMESET_PROTEIN_MATCHES,
    FIELD_PROTEIN_SETS_NB_SUBSET_PROTEIN_MATCHES,
    FIELD_PROTEIN_SETS_COVERAGE,
    FIELD_PROTEIN_SETS_MW,
    FIELD_PROTEIN_SETS_NB_SEQUENCES,
    FIELD_PROTEIN_SETS_NB_SPECIFIC_SEQUENCES,
    FIELD_PROTEIN_SETS_NB_PEPTIDES,
    FIELD_PROTEIN_SETS_NB_SPECIFIC_PEPTIDES,
    FIELD_PROTEIN_SETS_NB_PEPTIDE_MATCHES,
    FIELD_PROTEIN_SETS_NB_SPECIFIC_PEPTIDE_MATCHES
  )
  
  protected val protSetFieldsConfigs = sheetConfig.fields.filter( f => protSetFieldSet.contains(f.id) )

  def buildRecord( buildingContext: IRecordBuildingContext ): Map[String,Any] = {
    
    val buildingCtx = buildingContext.asInstanceOf[ProtMatchBuildingContext]
    val protSet = buildingCtx.protSet
    val pepSet = protSet.peptideSet
    val protMatch = buildingCtx.protMatch // here it is the typical protein match
    
    // MW
    val bioSequenceById = identDS.bioSequenceById
    val proteinId = protMatch.getProteinId
    val massOpt = if (proteinId > 0) bioSequenceById.get(proteinId).map(_.mass) else None
    val mass = massOpt.getOrElse(0.0)
    
    // Coverage
    val proteinMatchCoverageById = Option(protSet.proteinMatchCoverageById).getOrElse(Map())
    val protMatchCoverage = proteinMatchCoverageById(protMatch.id)
    
    val recordBuilder = Map.newBuilder[String,Any]

    for (fieldConfig <- protSetFieldsConfigs) {
      val fieldValue: Any = fieldConfig.id match {
        case FIELD_PROTEIN_SETS_ID => protSet.id
        case FIELD_PROTEIN_SETS_ACCESSION => protMatch.accession
        case FIELD_PROTEIN_SETS_DESCRIPTION => protMatch.description
        case FIELD_PROTEIN_SETS_SCORE => decimalFormat.format(pepSet.score) // FIXME: this column does not exist in the MSIdb
        case FIELD_PROTEIN_SETS_IS_VALIDATED => protSet.isValidated
        case FIELD_PROTEIN_SETS_SELECTION_LEVEL => protSet.selectionLevel
        case FIELD_PROTEIN_SETS_NB_SAMESET_PROTEIN_MATCHES => protSet.getSameSetProteinMatchIds.length
        case FIELD_PROTEIN_SETS_NB_SUBSET_PROTEIN_MATCHES => protSet.getSubSetProteinMatchIds.length
        case FIELD_PROTEIN_SETS_COVERAGE => dcf2.format(protMatchCoverage)
        case FIELD_PROTEIN_SETS_MW => decimalFormat.format(mass)
        case FIELD_PROTEIN_SETS_NB_SEQUENCES => buildingCtx.allSeqs.distinct.length
        case FIELD_PROTEIN_SETS_NB_SPECIFIC_SEQUENCES => if(protSet.isValidated) buildingCtx.specificSeqs.distinct.length else -1
        case FIELD_PROTEIN_SETS_NB_PEPTIDES => buildingCtx.peptideCount
        case FIELD_PROTEIN_SETS_NB_SPECIFIC_PEPTIDES => if(protSet.isValidated) buildingCtx.specificPeps.length else -1 
        case FIELD_PROTEIN_SETS_NB_PEPTIDE_MATCHES => protSet.peptideSet.peptideMatchesCount
        case FIELD_PROTEIN_SETS_NB_SPECIFIC_PEPTIDE_MATCHES => if(protSet.isValidated) buildingCtx.specificPepMatchIds.length else -1
      }
      
      recordBuilder += fieldConfig.title -> fieldValue
    }
    
    recordBuilder.result()
  }

  /*def buildRecord(buildingContext: IRecordBuildingContext): Map[String, Any] = {

    val dcf1 = new DecimalFormat("0.#")
    dcf1.setDecimalFormatSymbols(decimalFormat.getDecimalFormatSymbols())
    val dcf2 = new DecimalFormat("0.##")
    dcf2.setDecimalFormatSymbols(decimalFormat.getDecimalFormatSymbols())
    val dcf4 = new DecimalFormat("0.####")
    dcf4.setDecimalFormatSymbols(decimalFormat.getDecimalFormatSymbols())
    val dcf6 = new DecimalFormat("0.######")
    dcf6.setDecimalFormatSymbols(decimalFormat.getDecimalFormatSymbols())

    val isProtSetCtx = (buildingContext.isInstanceOf[ProtMatchBuildingContext])
    val isPepSetCtx = (buildingContext.isInstanceOf[PepMatchBuildingContext])
    val isProtSetQuantiCtx = (buildingContext.isInstanceOf[ProtMatchQuantiBuildingContext])
    val isProtSetCtxDefinedForPep = (isPepSetCtx && buildingContext.asInstanceOf[PepMatchBuildingContext].protMatchBuildingCtx.isDefined)
    val isPepSetQuantiCtx = (buildingContext.isInstanceOf[PepMatchQuantiBuildingContext])
    val isPepIonSetQuantiCtx = (buildingContext.isInstanceOf[PepIonMatchQuantiBuildingContext])

    val protSetBuildingCtx = if (isProtSetCtx) buildingContext.asInstanceOf[ProtMatchBuildingContext] else (if (isProtSetCtxDefinedForPep) buildingContext.asInstanceOf[PepMatchBuildingContext].protMatchBuildingCtx.get else null)
    val allPepMatchesBuildingCtx = if (isProtSetCtx) null else buildingContext.asInstanceOf[PepMatchBuildingContext]
    val protSetQuantiBuildingCtx = if (isProtSetQuantiCtx) buildingContext.asInstanceOf[ProtMatchQuantiBuildingContext] else null
    val pepSetQuantiBuildingCtx = if (isPepSetQuantiCtx) buildingContext.asInstanceOf[PepMatchQuantiBuildingContext] else null
    val pepIonSetQuantiBuildingCtx = if (isPepIonSetQuantiCtx) buildingContext.asInstanceOf[PepIonMatchQuantiBuildingContext] else null

    var protSet: ProteinSet = null
    var protMatch: ProteinMatch = null
    var peptideSet: PeptideSet = null
    if (protSetBuildingCtx != null) {
      protSet = protSetBuildingCtx.protSet
      protMatch = protSetBuildingCtx.protMatch
      peptideSet = protSetBuildingCtx.peptideSet
    }

    var protSetId = -1l
    var protSetScore: Any = ""
    var protSetValid = "false"
    if (protSetBuildingCtx != null) {
      protSetId = protSetBuildingCtx.protSet.id
      protSetScore = ExportConfigManager.format(dcf1, protSetBuildingCtx.protSet.peptideSet.score)
      protSetValid = protSetBuildingCtx.protSet.isValidated.toString
    }

    var pepMatch: PeptideMatch = null
    var seqMatch: SequenceMatch = null
    if (allPepMatchesBuildingCtx != null) {
      pepMatch = allPepMatchesBuildingCtx.pepMatch
      seqMatch = allPepMatchesBuildingCtx.seqMatch
    }

    val peptide = if (pepMatch == null) null else pepMatch.peptide
    val initialQueryId = if (pepMatch == null) null else Option(pepMatch.msQuery).map(_.initialId).getOrElse(null)
    val experimentalMoz = if (pepMatch == null) null else ExportConfigManager.format(dcf6, (Option(pepMatch.msQuery).map(_.moz).getOrElse(null)))

    val resBefore = if (pepMatch == null) null else if (seqMatch.residueBefore == '\0') '-' else seqMatch.residueBefore
    val resAfter = if (pepMatch == null) null else if (seqMatch.residueAfter == '\0') '-' else seqMatch.residueAfter

    val (ptmScore, ptmSites) = if (pepMatch == null ) ("", "")
    else {
      val pepMatchPropsOpt = pepMatch.properties
      
      // If defined pepMatchPropsOpt and ptmSiteProperties
      if (pepMatchPropsOpt.isDefined && pepMatchPropsOpt.get.getPtmSiteProperties.isDefined) {
        val ptmSiteProperties = pepMatchPropsOpt.get.getPtmSiteProperties.get
        
        val score = ptmSiteProperties.getMascotDeltaScore.getOrElse(0.0f)
        var sitesEx = "";
        if (ptmSiteProperties.getMascotProbabilityBySite.isDefined){
          val sites = ptmSiteProperties.getMascotProbabilityBySite.get.map { case(k,v) => 
            k+" = "+ExportConfigManager.format(dcf2, v)
          }
          sitesEx = sites.mkString(",") 
        }
          
        ( ExportConfigManager.format(dcf2,score).toString, sitesEx )
      } else ("", "")
    }
    
    val dbProtMatchesCount = {
      if (identDS.allProtMatchSetByPepId == null || pepMatch == null) null
      else if (identDS.allProtMatchSetByPepId.get(pepMatch.peptideId).isDefined) {
        identDS.allProtMatchSetByPepId.get(pepMatch.peptideId).get.size
      } else
        0
    }
    
    // MW
    val bioSequenceById = identDS.bioSequenceById    
    val proteinId = protMatch.getProteinId
    val massOpt = if (proteinId > 0) bioSequenceById.get(proteinId).map(_.mass) else None
    val mass = massOpt.getOrElse(0.0)

    //Option(protMatch.protein).flatMap(_.map(_.mass)).getOrElse(0))
    
    // retentionTime
    val spectrumMetadataByMsQueryId = identDS.spectraDescriptorById
    
    val retentionTimeOpt = if (pepMatch == null || pepMatch.msQuery == null) None
    else {
      spectrumMetadataByMsQueryId.get(pepMatch.msQuery.id).map { spectrumMetdata =>
        ExportConfigManager.format(dcf4, spectrumMetdata.firstTime)
      }
    }
    val retentionTime = retentionTimeOpt.getOrElse("-")

    // score
    var score: Double = protSet.peptideSet.score
    // coverage
    var coverage: Float = if (protSet.proteinMatchCoverageById.contains(protMatch.id)) protSet.proteinMatchCoverageById.get(protMatch.id).get else 0
    // Add some statistics
    var stats: List[String] = null
    var nbS = 0
    if (isQuantDs && protSetQuantiBuildingCtx != null && protSetQuantiBuildingCtx.profile != null) {
      stats = this.stringifyRatiosStats(protSetQuantiBuildingCtx.profile.getRatios)
      nbS = stats.size
    }
    if (isQuantDs && pepSetQuantiBuildingCtx != null && pepSetQuantiBuildingCtx.masterQuantPeptide != null && pepSetQuantiBuildingCtx.masterQuantPeptide.getRatios(groupSetupNumber) != null) {
      stats = this.stringifyRatiosStats(pepSetQuantiBuildingCtx.masterQuantPeptide.getRatios(groupSetupNumber))
      nbS = stats.size
    }

    // masterquantPeptide
    var elutionTime: Any = ""
    if (isPepSetQuantiCtx && pepSetQuantiBuildingCtx != null && pepSetQuantiBuildingCtx.masterQuantPeptide != null) {
      val bestQPep = pepSetQuantiBuildingCtx.masterQuantPeptide.getBestQuantPeptide
      elutionTime = if (bestQPep.elutionTime.isNaN()) "" else ExportConfigManager.format(dcf4, bestQPep.elutionTime) 
    }
    
    // psmId 
    var pepMatchId: Any = "";
    if (pepMatch != null){
      pepMatchId = pepMatch.id;
    }
    var exportMap: ListMap[String, Any] = ListMap()
    val listFields: Array[CustomFieldConfig] = sheetConfig.fields
    for (f <- listFields) {
      f.id match {
        case FIELD_PROTEIN_SETS_ID => {
          exportMap += (fields.addField(f.title) -> protSet.id)
        }
        case FIELD_PROTEIN_SETS_ACCESSION => {
          exportMap += (fields.addField(f.title) -> protMatch.accession)
        }
        case FIELD_PROTEIN_SETS_DESCRIPTION => {
          exportMap += (fields.addField(f.title) -> protMatch.description)
        }
        case FIELD_PROTEIN_SETS_SCORE => {
          exportMap += (fields.addField(f.title) -> ExportConfigManager.format(decimalFormat, score))
        }
        case FIELD_PROTEIN_SETS_IS_VALIDATED => {
          exportMap += (fields.addField(f.title) -> protSet.isValidated.toString)
        }
        case FIELD_PROTEIN_SETS_SELECTION_LEVEL => {
          exportMap += (fields.addField(f.title) -> protSet.selectionLevel)
        }
        case FIELD_PROTEIN_SETS_NB_SAMESET_PROTEIN_MATCHES => {
          exportMap += (fields.addField(f.title) -> protSet.getSameSetProteinMatchIds.length)
        }
        case FIELD_PROTEIN_SETS_NB_SUBSET_PROTEIN_MATCHES => {
          exportMap += (fields.addField(f.title) -> protSet.getSubSetProteinMatchIds.length)
        }
        case FIELD_PROTEIN_SETS_COVERAGE => {
          exportMap += (fields.addField(f.title) -> ExportConfigManager.format(dcf2, coverage))
        }
        case FIELD_PROTEIN_SETS_MW => {
          exportMap += (fields.addField(f.title) -> ExportConfigManager.format(decimalFormat, mass))
        }
        case FIELD_PROTEIN_SETS_NB_SEQUENCES => {
          exportMap += (fields.addField(f.title) -> protSetBuildingCtx.allSeqs.distinct.length)
        }
        case FIELD_PROTEIN_SETS_NB_SPECIFIC_SEQUENCES => {
          exportMap += (fields.addField(f.title) -> protSetBuildingCtx.specificSeqs.distinct.length)
        }
        case FIELD_PROTEIN_SETS_NB_PEPTIDES => {
          exportMap += (fields.addField(f.title) -> protSetBuildingCtx.peptideCount)
        }
        case FIELD_PROTEIN_SETS_NB_SPECIFIC_PEPTIDES => {
          exportMap += (fields.addField(f.title) -> protSetBuildingCtx.specificPeps.length)
        }
        case FIELD_PROTEIN_SETS_NB_PEPTIDE_MATCHES => {
          exportMap += (fields.addField(f.title) -> protSet.peptideSet.peptideMatchesCount)
        }
        case FIELD_PROTEIN_SETS_NB_SPECIFIC_PEPTIDE_MATCHES => {
          exportMap += (fields.addField(f.title) -> protSetBuildingCtx.specificPepMatchIds.length)
        }
        case FIELD_PROTEIN_MATCH_IS_TYPICAL_PROTEIN => {
          exportMap += (fields.addField(f.title) -> (protSet.getRepresentativeProteinMatchId == protMatch.id))
        }
        case FIELD_PROTEIN_MATCH_IS_SAMESET => {
          exportMap += (fields.addField(f.title) -> !peptideSet.isSubset)
        }
        case FIELD_PROTEIN_MATCH_PEPTIDE_SET_SCORE => {
          exportMap += (fields.addField(f.title) -> ExportConfigManager.format(dcf1, peptideSet.score))
        }
        case FIELD_PSM_PEPTIDE_ID => {
          exportMap += (fields.addField(f.title) -> peptide.id)
        }
        case FIELD_PSM_SEQUENCE => {
          exportMap += (fields.addField(f.title) -> peptide.sequence)
        }
        case FIELD_PSM_MODIFICATIONS => {
          exportMap += (fields.addField(f.title) -> peptide.readablePtmString)
        }
        case FIELD_PSM_ID => {
          exportMap += (fields.addField(f.title) -> pepMatchId)
        }
        case FIELD_PSM_SCORE => {
          exportMap += (fields.addField(f.title) -> ExportConfigManager.format(decimalFormat, pepMatch.score))
        }
        case FIELD_PSM_CALCULATED_MASS => {
          exportMap += (fields.addField(f.title) -> ExportConfigManager.format(dcf6, peptide.calculatedMass))
        }
        case FIELD_PSM_CHARGE => {
          exportMap += (fields.addField(f.title) -> Option(pepMatch.msQuery).map(_.charge).getOrElse(null))
        }
        case FIELD_PSM_EXPERIMENTAL_MOZ => {
          exportMap += (fields.addField(f.title) -> experimentalMoz)
        }
        case FIELD_PSM_DELTA_MOZ => {
          exportMap += (fields.addField(f.title) -> ExportConfigManager.format(dcf6, pepMatch.deltaMoz))
        }
        case FIELD_PSM_RT => {
          exportMap += (fields.addField(f.title) -> retentionTime)
        }
        case FIELD_PSM_PEPTIDE_LENGTH => {
          exportMap += (fields.addField(f.title) -> peptide.sequence.length)
        }
        case FIELD_PSM_INITIAL_QUERY_ID => {
          exportMap += (fields.addField(f.title) -> initialQueryId)
        }
        case FIELD_PSM_MISSED_CLEAVAGES => {
          exportMap += (fields.addField(f.title) -> pepMatch.missedCleavage)
        }
        case FIELD_PSM_RANK => {
          exportMap += (fields.addField(f.title) -> pepMatch.rank)
        }
        case FIELD_PSM_CD_PRETTY_RANK => {
          exportMap += (fields.addField(f.title) -> pepMatch.cdPrettyRank)
        }
        case FIELD_PSM_FRAGMENT_MATCHES_COUNT => {
          exportMap += (fields.addField(f.title) -> pepMatch.fragmentMatchesCount)
        }
        case FIELD_PSM_SPECTRUM_TITLE => {
          exportMap += (fields.addField(f.title) -> Option(pepMatch.getMs2Query).map(_.spectrumTitle).getOrElse(""))
        }
        case FIELD_PSM_NB_PROTEIN_SETS => {
          exportMap += (fields.addField(f.title) -> identDS.validProtSetIdSetByPepMatchId.get(pepMatch.id).map(_.size).getOrElse(0))
        }
        case FIELD_PSM_NB_PROTEIN_MATCHES => {
          exportMap += (fields.addField(f.title) -> identDS.validProtMatchIdSetByPepMatchId.get(pepMatch.id).map(_.size).getOrElse(0))
        }
        case FIELD_PSM_NB_DATABANK_PROTEIN_MATCHES => {
          exportMap += (fields.addField(f.title) -> dbProtMatchesCount)
        }
        case FIELD_PSM_START => {
          exportMap += (fields.addField(f.title) -> seqMatch.start)
        }
        case FIELD_PSM_END => {
          exportMap += (fields.addField(f.title) -> seqMatch.end)
        }
        case FIELD_PSM_RESIDUE_BEFORE => {
          exportMap += (fields.addField(f.title) -> resBefore)
        }
        case FIELD_PSM_RESIDUE_AFTER => {
          exportMap += (fields.addField(f.title) -> resAfter)
        }
        case FIELD_PSM_PTM_SCORE => {
          exportMap += (fields.addField(f.title) -> ptmScore)
        }
        case FIELD_PSM_PTM_SITES_CONFIDENCE => {
          exportMap += (fields.addField(f.title) -> ptmSites)
        }
        case FIELD_PROTEIN_SETS_QUANTI_STATUS => {
          if (isQuantDs && protSetQuantiBuildingCtx.masterQuantProteinSet != null) {
            quantDs.qcIds.foreach(qcId => {
              //Get QuantProteinSet for current ProteinSet in current QuantChannel
              val protQuantOpt = protSetQuantiBuildingCtx.masterQuantProteinSet.quantProteinSetMap.get(qcId)
              val protMatchId = protQuantOpt.flatMap( _.proteinMatchId ).getOrElse(-1L)
              val identRsm = quantDs.identRsmByQcId(qcId)
              
              val qcStatus = quantDs.getProteinMatchStatus(identRsm, protMatchId).map(_.toString).getOrElse("")
              exportMap += (fields.addField(f.title + titleSep + quantDs.qcNameById(qcId)) -> qcStatus)
            })
          }
        }
        case FIELD_PROTEIN_SETS_QUANTI_PEPTIDE_NUMBER => {
          if (isQuantDs && protSetQuantiBuildingCtx.masterQuantProteinSet != null) {
            quantDs.qcIds.foreach(qcId => {
              //Get QuantProteinSet for current ProteinSet in current QuantChannel
              val protQuantOpt = protSetQuantiBuildingCtx.masterQuantProteinSet.quantProteinSetMap.get(qcId)
              val protMatchId = protQuantOpt.flatMap( _.proteinMatchId ).getOrElse(-1L)              
              val identRsm = quantDs.identRsmByQcId(qcId)
              
              val protMatchOpt = identRsm.lazyResultSet.proteinMatchById.get(protMatchId)
              val protMatchPepCount = protMatchOpt.map(_.peptidesCount).getOrElse(0)
              
              exportMap += (fields.addField(f.title + titleSep + quantDs.qcNameById(qcId)) -> protMatchPepCount)
            })
          }
        }
        case FIELD_PROTEIN_SETS_QUANTI_RAW_ABUNDANCE => {
          if (isQuantDs ) {
            quantDs.qcIds.foreach(qcId => {
              var qcRawAbun: Any = null
              if (protSetQuantiBuildingCtx != null && protSetQuantiBuildingCtx.masterQuantProteinSet != null) {
                qcRawAbun = if (protSetQuantiBuildingCtx.masterQuantProteinSet.quantProteinSetMap.contains(qcId)) {
                  protSetQuantiBuildingCtx.masterQuantProteinSet.quantProteinSetMap(qcId).rawAbundance
                } else {
                  0
                }
              }
              if (pepSetQuantiBuildingCtx != null && pepSetQuantiBuildingCtx.masterQuantPeptide != null && pepSetQuantiBuildingCtx.masterQuantPeptide.quantPeptideMap != null) {
            	qcRawAbun = if (pepSetQuantiBuildingCtx.masterQuantPeptide.quantPeptideMap.contains(qcId)) {
                  pepSetQuantiBuildingCtx.masterQuantPeptide.quantPeptideMap(qcId).rawAbundance
                } else {
                  0
                }
              }
              if (pepIonSetQuantiBuildingCtx != null && pepIonSetQuantiBuildingCtx.masterQuantPeptideIon != null && pepIonSetQuantiBuildingCtx.masterQuantPeptideIon.quantPeptideIonMap != null) {
            	qcRawAbun = if (pepIonSetQuantiBuildingCtx.masterQuantPeptideIon.quantPeptideIonMap.contains(qcId)) {
                  pepIonSetQuantiBuildingCtx.masterQuantPeptideIon.quantPeptideIonMap(qcId).rawAbundance
                } else {
                  0
                }
              }
              exportMap += (fields.addField(f.title + titleSep + quantDs.qcNameById(qcId)) -> ExportConfigManager.format(decimalFormat,qcRawAbun))
            })
          }
        }
        case FIELD_PROTEIN_SETS_QUANTI_ABUNDANCE => {
          if (isQuantDs) {
            quantDs.qcIds.foreach(qcId => {
              var qcAbun: Any = null
              if (protSetQuantiBuildingCtx != null &&  protSetQuantiBuildingCtx.masterQuantProteinSet != null) {
                qcAbun = if (protSetQuantiBuildingCtx.masterQuantProteinSet.quantProteinSetMap.contains(qcId)) {
                  protSetQuantiBuildingCtx.masterQuantProteinSet.quantProteinSetMap(qcId).abundance
                } else {
                  0
                }
              }
              if (pepSetQuantiBuildingCtx != null && pepSetQuantiBuildingCtx.masterQuantPeptide != null && pepSetQuantiBuildingCtx.masterQuantPeptide.quantPeptideMap != null) {
            	qcAbun = if (pepSetQuantiBuildingCtx.masterQuantPeptide.quantPeptideMap.contains(qcId)) {
                  pepSetQuantiBuildingCtx.masterQuantPeptide.quantPeptideMap(qcId).abundance
                } else {
                  0
                }
              }
              if (pepIonSetQuantiBuildingCtx != null && pepIonSetQuantiBuildingCtx.masterQuantPeptideIon != null && pepIonSetQuantiBuildingCtx.masterQuantPeptideIon.quantPeptideIonMap != null) {
            	qcAbun = if (pepIonSetQuantiBuildingCtx.masterQuantPeptideIon.quantPeptideIonMap.contains(qcId)) {
                  pepIonSetQuantiBuildingCtx.masterQuantPeptideIon.quantPeptideIonMap(qcId).abundance
                } else {
                  0
                }
              }
              exportMap += (fields.addField(f.title + titleSep + quantDs.qcNameById(qcId)) -> ExportConfigManager.format(decimalFormat,qcAbun))
            })
          }
        }
        case FIELD_PROTEIN_SETS_QUANTI_PSM_COUNT => {
          if (isQuantDs) {
            quantDs.qcIds.foreach(qcId => {
              var qcPSMCount: Any = null
              if (protSetQuantiBuildingCtx != null  && protSetQuantiBuildingCtx.masterQuantProteinSet != null) {
                qcPSMCount = if (protSetQuantiBuildingCtx.masterQuantProteinSet.quantProteinSetMap.contains(qcId)) {
                  protSetQuantiBuildingCtx.masterQuantProteinSet.quantProteinSetMap(qcId).peptideMatchesCount
                } else {
                  0
                }
              }
              if (pepSetQuantiBuildingCtx != null && pepSetQuantiBuildingCtx.masterQuantPeptide != null && pepSetQuantiBuildingCtx.masterQuantPeptide.quantPeptideMap != null) {
            	qcPSMCount = if (pepSetQuantiBuildingCtx.masterQuantPeptide.quantPeptideMap.contains(qcId)) {
                  pepSetQuantiBuildingCtx.masterQuantPeptide.quantPeptideMap(qcId).peptideMatchesCount
                } else {
                  0
                }
              }
              if (pepIonSetQuantiBuildingCtx != null && pepIonSetQuantiBuildingCtx.masterQuantPeptideIon != null && pepIonSetQuantiBuildingCtx.masterQuantPeptideIon.quantPeptideIonMap != null) {
            	qcPSMCount = if (pepIonSetQuantiBuildingCtx.masterQuantPeptideIon.quantPeptideIonMap.contains(qcId)) {
                  pepIonSetQuantiBuildingCtx.masterQuantPeptideIon.quantPeptideIonMap(qcId).peptideMatchesCount
                } else {
                  0
                }
              }
              exportMap += (fields.addField(f.title + titleSep + quantDs.qcNameById(qcId)) -> ExportConfigManager.format(decimalFormat,qcPSMCount))
            })
          }
        }
        case FIELD_PROTEIN_SETS_XIC_PROFILIZER_RATIO => {
          if (isQuantDs && quantDs.ratioDefs != null) {
            var i = 0;
            for (r <- quantDs.ratioDefs) {
              if (nbS > i + 0) {
                exportMap += (fields.addField(mkGroupTitle(f.title, r)) -> stats(i + 0))
              }
              i = i + 1
            }
          }
        }
        case FIELD_PROTEIN_SETS_XIC_PROFILIZER_TTEST_PVALUE => {
          if (isQuantDs && quantDs.ratioDefs != null) {
            var i = 0;
            for (r <- quantDs.ratioDefs) {
              if (nbS > i + 1) {
                exportMap += (fields.addField(mkGroupTitle(f.title, r)) -> stats(i + 1))
              }
              i = i + 1
            }
          }
        }
        case FIELD_PROTEIN_SETS_XIC_PROFILIZER_ZTEST_PVALUE => {
          if (isQuantDs && quantDs.ratioDefs != null) {
            var i = 0;
            for (r <- quantDs.ratioDefs) {
              if (nbS > i + 2) {
                exportMap += (fields.addField(mkGroupTitle(f.title, r)) -> stats(i + 2))
              }
              i = i + 1
            }
          }
        }
        case FIELD_PROTEIN_SETS_XIC_PROFILIZER_ZSCORE => {
          if (isQuantDs && quantDs.ratioDefs != null) {
            var i = 0;
            for (r <- quantDs.ratioDefs) {
              if (nbS > i + 3) {
                exportMap += (fields.addField(mkGroupTitle(f.title, r)) -> stats(i + 3))
              }
              i = i + 1
            }
          }
        }
        case FIELD_PSM_QUANTI_MASTER_QUANT_PEPTIDE_ID => {
          if (isPepSetQuantiCtx && pepSetQuantiBuildingCtx != null && pepSetQuantiBuildingCtx.masterQuantPeptide != null) {
            exportMap += (fields.addField(f.title) -> pepSetQuantiBuildingCtx.masterQuantPeptide.id)
          }
        }
        case FIELD_PSM_QUANTI_ELUTION_TIME => {
          if (isPepSetQuantiCtx && pepSetQuantiBuildingCtx != null && pepSetQuantiBuildingCtx.masterQuantPeptide != null) {
            exportMap += (fields.addField(f.title) -> elutionTime)
          }
        }
        case FIELD_PSM_QUANTI_SELECTION_LEVEL => {
          if (isPepSetQuantiCtx && pepSetQuantiBuildingCtx != null && pepSetQuantiBuildingCtx.masterQuantPeptide != null) {
            exportMap += (fields.addField(f.title) -> pepSetQuantiBuildingCtx.masterQuantPeptide.selectionLevel)
          }
        }
        case FIELD_MASTER_QUANT_PEPTIDE_ION_ID => {
          if (isPepIonSetQuantiCtx && pepIonSetQuantiBuildingCtx != null && pepIonSetQuantiBuildingCtx.masterQuantPeptideIon != null) {
            exportMap += (fields.addField(f.title) -> pepIonSetQuantiBuildingCtx.masterQuantPeptideIon.id)
          }
        }
        case FIELD_MASTER_QUANT_PEPTIDE_ION_CHARGE => {
          if (isPepIonSetQuantiCtx && pepIonSetQuantiBuildingCtx != null && pepIonSetQuantiBuildingCtx.masterQuantPeptideIon != null) {
            exportMap += (fields.addField(f.title) -> pepIonSetQuantiBuildingCtx.masterQuantPeptideIon.charge)
          }
        }
        case FIELD_MASTER_QUANT_PEPTIDE_ION_ELUTION_TIME => {
          if (isPepIonSetQuantiCtx && pepIonSetQuantiBuildingCtx != null && pepIonSetQuantiBuildingCtx.masterQuantPeptideIon != null) {
            exportMap += (fields.addField(f.title) -> ExportConfigManager.format(dcf4, pepIonSetQuantiBuildingCtx.masterQuantPeptideIon.elutionTime))
          }
        }
        case FIELD_MASTER_QUANT_PEPTIDE_ION_FEATURE_ID => {
          if (isPepIonSetQuantiCtx && pepIonSetQuantiBuildingCtx != null && pepIonSetQuantiBuildingCtx.masterQuantPeptideIon != null) {
            exportMap += (fields.addField(f.title) -> pepIonSetQuantiBuildingCtx.masterQuantPeptideIon.lcmsMasterFeatureId.get)
          }
        }
        case other => {
          // should not happen
        }
      }
    }

    //exportMap.map( r => r._1.toString -> r._2)
    exportMap
    
  }*/

  
}