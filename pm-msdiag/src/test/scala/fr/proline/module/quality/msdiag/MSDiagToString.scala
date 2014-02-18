package fr.proline.module.quality.msdiag

import scala.collection.mutable.ArrayBuffer
import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.module.quality.msdiag.msi._
import fr.proline.core.om.model.msi.PeptideMatch

object MSDiagToString extends Logging {
  
  private def getLabels(byCharges: Array[Array[MSDiagChargeMatch]]): Array[String] = {
    val arrayLabels = new Array[String](byCharges(0).size)
    for(i <- byCharges(0).indices) { arrayLabels(i) = byCharges(0)(i).getLabel }
    arrayLabels.toArray
  }
  private def getLabels(byRT: Array[Array[MSDiagRTMatch]]): Array[String] = {
    val arrayLabels = new Array[String](byRT(0).size)
    for(i <- byRT(0).indices) { arrayLabels(i) = byRT(0)(i).getLabel }
    arrayLabels.toArray
  }

  def printCountPerChargeAndScore(byCharges: Array[Array[MSDiagChargeMatch]]) {
    logger.debug("Count per charge and score")
    var line = "Charge\t"
    getLabels(byCharges).foreach(l => {line += l + "\t"})
    println(line)
    for(i <- byCharges.indices) {
      line = ""+byCharges(i)(0).charge+"\t"
      for(j <- byCharges(i).indices) line += byCharges(i)(j).countItems+"\t"
      println(line)
    }
  }
  
  def printMozPerChargeAndScore(byCharges: Array[Array[MSDiagChargeMatch]]) {
    logger.debug("Moz per charge and score")
    var line = "Charge\t"
    getLabels(byCharges).foreach(l => {line += l + "\t"})
    println(line)
    for(i <- byCharges.indices) {
      line = ""+byCharges(i)(0).charge+"\t"
      for(j <- byCharges(i).indices) line += byCharges(i)(j).getLowestExperimentalMoz+"/"+byCharges(i)(j).getHighestExperimentalMoz+"\t"
      println(line)
    }
  }

  def printCountPerScore(byCharges: Array[Array[MSDiagChargeMatch]]) {
    logger.debug("Count per score")
    val arrayLabels = getLabels(byCharges)
    val arrayTotalCount = new Array[Int](arrayLabels.size)
    val arrayTotalDecoyCount = new Array[Int](arrayLabels.size)
    for(i <- byCharges.indices) {
      for(j <- byCharges(i).indices) {
        arrayTotalCount(j) += byCharges(i)(j).countItems
        arrayTotalDecoyCount(j) += byCharges(i)(j).decoyMatches.size
      }
    }
    for(i <- 0 until arrayLabels.size) println(arrayLabels(i) + " : " + arrayTotalCount(i) + "/" + arrayTotalDecoyCount(i))
  }

  def printCountPerRTAndScore(byRT: Array[Array[MSDiagRTMatch]]) {
    logger.debug("Count per RT and score")
    if(byRT != null) {
    	var line = "RT\t"
	    getLabels(byRT).foreach(l => {line += l + "\t"})
	    println(line)
	    for(i <- byRT.indices) {
	      line = ""+byRT(i)(0).rtmn+"\t"
	      for(j <- byRT(i).indices) {
	        line += byRT(i)(j).countItems+"\t"
	      }
	      println(line)
	    }
    } else { logger.debug("No retention times") }
  }
 
  def printPeptideMatch(pm: PeptideMatch, isDecoy: Boolean): String = {
    //"Charge="+pm.msQuery.charge + " ; Score="+pm.score + " ; =Title"+pm.getMs2Query.spectrumTitle + " ; Sequence="+pm.peptide.sequence + " ; Mass="+pm.peptide.calculatedMass+" ; =isDecoy"+isDecoy
    pm.getMs2Query.spectrumTitle+"\t"+pm.rank+"\t"+pm.msQuery.charge+"\t"+pm.score+"\t"+pm.peptide.sequence+"\t"+pm.peptide.calculatedMass+"\t"+isDecoy
  }
}