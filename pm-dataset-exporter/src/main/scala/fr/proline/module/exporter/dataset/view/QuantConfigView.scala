package fr.proline.module.exporter.dataset.view

import java.text.DecimalFormat
import java.text.SimpleDateFormat

import fr.profi.util.serialization.ProfiJson
import fr.proline.core.algo.msq.config.profilizer._
import fr.proline.module.exporter.api.view._
import fr.proline.module.exporter.dataset._

import scala.collection.mutable.ArrayBuffer

class QuantConfigView(
  val quantDs: QuantDataset,
  //val sheetConfig: ExportConfigSheet,
  val dateFormat: SimpleDateFormat,
  val decimalFormat: DecimalFormat
) extends IFormLikeView {

  var viewName = "quant_config"
  private var developerDictionnary = Map("aln" -> "alignment", "config" -> "", "params" -> "", "tol" -> "tolerance", "ft" -> "feature")

  private val quantConfigAndMethodOpt = quantDs.quantConfigAndMethod
  private val profilizerConfigOpt = quantDs.profilizerConfig
  private val _fieldValuePairs = {

    val quantConfigParams = if (quantConfigAndMethodOpt.isEmpty) {
      Array.empty[(String, Any)]
    } else {
      val quantConfigStr = quantConfigAndMethodOpt.get._1
      val quantConfigAsMap = ProfiJson.deserialize[Map[String,Any]](quantConfigStr)
      _stringifyMap(quantConfigAsMap, developerDictionnary)
    }

    val profilizerParams = profilizerConfigOpt.map { profilizerConfig =>
      _stringifyMap(ProfiJson.deserialize[Map[String,Any]](profilizerConfig), developerDictionnary)
    }.getOrElse( Array() )

    (quantConfigParams ++ profilizerParams).filter(_ != null) // remove null entries
  }


  // TODO: update the IFormLikeView trait to use List[Tuple2] instead of Map
  def getFieldValueMap(): Map[String, Any] = _fieldValuePairs.toMap

  def getFieldsNames(): Array[String] = _fieldValuePairs.map(_._1)
  
  def _stringifyMap(map : Map[String, Any], dictionnary: Map[String, String] = Map()): Array[(String, Any)] = {

    var strings = ArrayBuffer[(String, Any)]()
    var tmpMap = collection.mutable.Map[String, Any]()

    map.foreach { case(key, value) =>
      if (map.contains(key+"_unit")) {
        tmpMap += (key -> ( (if(value !=null) value.toString else "") +" "+map(key+"_unit")))
      } else if (!key.endsWith("_unit") ) {
        tmpMap += (key -> value)
      }
    }

    tmpMap.foreach { case (key, value) =>
      val modifiedKey = key.replace('_',' ').split("\\W+").map(w => dictionnary.getOrElse(w, w)).mkString(" ").trim().replaceAll("\\s{2,}", " ")
      val str = value match {
        case m:Map[_, _] => _stringifyMap(m.asInstanceOf[Map[String, Any]], dictionnary).map{ case(e1, e2) => (modifiedKey+" "+e1, e2) }
        case _ => Array((modifiedKey, if(value !=null) value.toString else ""))
      }
      strings ++= str
    }
    strings.toArray
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