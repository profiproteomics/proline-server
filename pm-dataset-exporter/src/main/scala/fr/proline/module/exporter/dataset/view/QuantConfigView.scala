package fr.proline.module.exporter.dataset.view

import java.text.DecimalFormat
import java.text.SimpleDateFormat
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.LongMap
import fr.profi.util.collection._
import fr.proline.core.algo.lcms._
import fr.proline.core.algo.msq.config._
import fr.proline.core.algo.msq.config.profilizer._
import fr.proline.core.om.model.msi._
import fr.proline.core.om.model.msq.IQuantMethod
import fr.proline.module.exporter.api.view._
import fr.proline.module.exporter.commons.config.ExportConfigConstant._
import fr.proline.module.exporter.commons.config.ExportConfigSheet
import fr.proline.module.exporter.commons.config.view.CustomViewFields
import fr.proline.module.exporter.dataset._

class QuantConfigView(
  val quantDs: QuantDataset,
  //val sheetConfig: ExportConfigSheet,
  val dateFormat: SimpleDateFormat,
  val decimalFormat: DecimalFormat
) extends IFormLikeView {
  
  private val quantConfigAndMethodOpt = quantDs.quantConfigAndMethod
  private val profilizerConfigOpt = quantDs.profilizerConfig

  var viewName = "quant_config"

  // TODO: update the IFormLikeView trait to use List[Tuple2] instead of Map 
  def getFieldValueMap() = _fieldValuePairs.toMap
  def getFieldsNames() = _fieldValuePairs.map(_._1)

  private val _fieldValuePairs = {
    
    val quantConfigParams = if (quantConfigAndMethodOpt.isEmpty) Array.empty[(String,Any)]
    else {
      val (quantConfig, quantMethod ) = quantConfigAndMethodOpt.get
      
      quantConfig match {
        case lfqConfig: LabelFreeQuantConfig => {
          
          // TODO: replace the boolean values in lfqConfig by this enum
          val signalProcStrategy = if(lfqConfig.detectPeakels || lfqConfig.detectFeatures) "Detect LC-MS peaks"
          else if (lfqConfig.startFromValidatedPeptides) "Perform XIC of validated peptides"
          else "Perform XIC of MS/MS events"
          
          val deisotopingMode = if (lfqConfig.detectPeakels || !lfqConfig.detectFeatures) "Identification based"
          else "Unsupervised"
            
          val extractionParams = lfqConfig.extractionParams
          //val clusteringParams = lfqConfig.clusteringParams
          val alnMethod = AlnMethod.withName(lfqConfig.alnMethodName)
          val alnParams = lfqConfig.alnParams
          val alnFtMappingParams = alnParams.ftMappingParams
          val alnSmoothingMethod = AlnSmoothing.withName(alnParams.smoothingMethodName)
          val alnSmoothingParams = alnParams.smoothingParams
          val ftMappingParams = lfqConfig.ftMappingParams
            
          Array(
            "### QUANTITATION PARAMETERS ###" -> null,
            "Signal processing strategy" -> signalProcStrategy,
            "Deisotoping mode" -> deisotopingMode,
            "Use previous peakel detection" -> lfqConfig.useLastPeakelDetection,
            "Signal extraction tolerance" -> s"${extractionParams.mozTol} ${extractionParams.mozTolUnit}",
            //"" -> lfqConfig.minPeakelDuration
            "Alignment method" -> lfqConfig.alnMethodName,
            if (alnMethod == AlnMethod.ITERATIVE) "Max. number of alignment iterations" -> alnParams.maxIterations else null,
            "Alignment m/z tolerance" -> s"${alnFtMappingParams.mozTol} ${alnFtMappingParams.mozTolUnit}",
            "Alignment time tolerance (sec)" -> alnFtMappingParams.timeTol,
            "Alignment smoothing method" -> alnParams.smoothingMethodName,
            if (alnSmoothingMethod == AlnSmoothing.TIME_WINDOW) "Alignment time interval (sec)" -> alnSmoothingParams.minWindowLandmarks else null,
            "Alignment window size" -> alnSmoothingParams.windowSize,
            "Alignment window overlap (%)" -> alnSmoothingParams.windowOverlap,
            "Match between runs m/z tolerance" -> s"${ftMappingParams.mozTol} ${ftMappingParams.mozTolUnit}",
            "Match between runs time tolerance (sec)" -> ftMappingParams.timeTol,
            lfqConfig.normalizationMethod.map( "Intensity normalization method" -> _ ).orNull
          )
          
        }
        case _ => Array.empty[(String,Any)]
      }
    }
    
    val profilizerParams = profilizerConfigOpt.map { profilizerConfig =>
      
      val mainParams = Array(
        "### POST-PROCESSING PARAMETERS ###" -> null,
        "Use only specific peptides" -> profilizerConfig.useOnlySpecificPeptides,
        "Discard miss cleaved peptides" -> profilizerConfig.discardMissedCleavedPeptides,
        "Discard oxidized peptides" -> profilizerConfig.discardOxidizedPeptides,
        "Apply profile clustering" -> profilizerConfig.applyProfileClustering,
        if (!profilizerConfig.applyProfileClustering) null
        else profilizerConfig.profileClusteringMethod.map("Profile clustering method" -> _).orNull,
        "Abundance summarizer method" -> profilizerConfig.abundanceSummarizerMethod
      )
      
      val peptideStatConfig = _stringifyProfilizerStatParams(
        "### POST-PROCESSING PARAMETERS AT PEPTIDE LEVEL ###",
        " on peptides",
        profilizerConfig.peptideStatConfig
      )
      val proteinStatConfig = _stringifyProfilizerStatParams(
        "### POST-PROCESSING PARAMETERS AT PROTEIN LEVEL ###",
        " on proteins",
        profilizerConfig.proteinStatConfig
      )
      
      mainParams ++ peptideStatConfig ++ proteinStatConfig
      
    }.getOrElse( Array() )
    
    (quantConfigParams ++ profilizerParams).filter(_ != null) // remove null entries
  }
  
  private def _stringifyProfilizerStatParams(label: String, suffix: String, statConfig: ProfilizerStatConfig): Array[(String,Any)] = {
    val params = Array(
      "Apply normalization" -> statConfig.applyNormalization,
      "Apply missing values inference" -> statConfig.applyMissValInference,
      if (statConfig.applyMissValInference) "Missing values inference method" -> statConfig.missValInferenceMethod else null,
      if (statConfig.missValInferenceMethod != "PERCENTILE") null
      else statConfig.missValInferenceConfig.map( "Missing values noise percentile" -> _.noisePercentile ).orNull,
      "Apply variance correction" -> statConfig.applyVarianceCorrection,
      "Apply T-Test" -> statConfig.applyTTest,
      "T-Test p-value threshold" -> decimalFormat.format(statConfig.statTestsAlpha),
      "Apply Z-Test" -> statConfig.applyZTest
    ).withFilter( _ != null).map( kv => (kv._1 + suffix, kv._2) )
    
    Array(label -> null) ++ params
  }

}