package fr.proline.module.quality.msdiag

import fr.proline.core.om.model.msi.PeptideMatch
import fr.proline.core.om.model.msi.ResultSet
import fr.proline.module.quality.msdiag.msi._
import fr.proline.context.IExecutionContext
import scala.collection.mutable.ArrayBuffer
import com.typesafe.scalalogging.LazyLogging

/**
 * @author Alexandre Burel (LSMBO IPHC CNRS)
 * MSDiag is an software designed to run a diagnosis of a MS/MS search
 * It has been originally developped for MSDA (https://msda.unistra.fr) and was intended to generate an Excel report from a Mascot search
 * A standard MSDiag report is an Excel file that contains 3 sheets
 * - A spectrum report, with the main result for each spectra (first rank peptide, first protein given).
 *   The unassigned spectra are kept, with less information
 *   The redundant matches (same peptide) are highlighted
 * - A table listing the number of matches per charge and score window (ie. 20 to 40), or unassigned
 *   Values such as moz and intensity are given per charge state (min, max)
 *   The number of decoy matches per score window is given
 *   The total number of redundant matches is given
 * - A table listing the number of matches per retention time and score window (or unassigned)
 *   The matches are grouped by minute (ie. number of unassigned from minute 2.0 to minute 2.9)
 *   This table can be directly transformed into a chromatogram with Excel graphs
 *
 * This Proline module should be able to do the same thing, but from a Proline ResultSet
 * This module extracts all the information needed to generate a MSDiag report and gives functions to get a similar output
 * This module is meant to be called by a GUI that will create the report
 *
 * Add a comparison function between 2 or more msdiags ?
 * 
 * 2014/11/10
 * This module must have a IExecutionContext so it can get data from the MSIdb
 * The input should be either a RSid or a RSMid
 *
 */

class MSDiag(val rsId: Long, val parserContext: IExecutionContext) extends LazyLogging {
  
  // this variable is the only access to the data stored in the databases
  private val rs = new MSDiagResultSetManager(parserContext, rsId)

  // this array is used to split the PSMs upon their score
  // a default window should be defined by the full range (min -> max) divided in 3 or 4 equivalent area (ie. less than 20 ; 20->40 ; 40->60 ; more than 60)
  // the unassigned spectra should be systematically added as first item of the score window
  private var scoreWindow: Array[Float] = Array(20, 40, 60)
  def setScoreWindow(_scoreWindow: Array[Float]) { scoreWindow = _scoreWindow }
  
  // Original MSDiag only considers matches with rank=1
  // It might be interesting to be able to see further
  private var maxRank: Integer = 1
  def setMaxRank(_maxRank: Integer) { maxRank = _maxRank }
  def unsetMaxRank(_maxRank: Integer) { maxRank = 0 }

  private var nbScansPerGroup: Integer = 100
  def setNbScansPerGroup(nb: Integer) { nbScansPerGroup = nb }
  
  def getSettings: Map[String, Any] = Map("Score window" -> scoreWindow.mkString("-"), 
									      "Max rank" -> maxRank,
									      "Scan groups size" -> nbScansPerGroup)
  def setSettings(settings: Map[String, Any]) {
    settings.keys.foreach(setting => {
        setting match {
          case "Score window" => {
            val newScores = new ArrayBuffer[Float]()
            settings.get(setting).get.toString.split("-").foreach(newScores += _.toFloat)
            setScoreWindow(newScores.toArray)
          }
          case "Max rank" => setMaxRank(settings.get(setting).get.toString.toInt)
          case "Scan groups size" => setNbScansPerGroup(settings.get(setting).get.toString.toInt)
          case _ =>
        } 
    })
  }
									      
  private val authorizedMethodRegex = "^getMSDiag_.*"
  /**
   * Access the list of methods available in MSDiag
   * @return the list of methods with a name starting with "getMSDiag"
   */
  def getMethods: Array[String] = this.getClass().getMethods().map(_.getName()).sorted.filter(_ matches authorizedMethodRegex)
  
  /**
   * @param method the name of the method to call
   * @return
   */
  def executeMethod(method: String): Any = {
    if (method.matches(authorizedMethodRegex)) {
      this.getClass().getMethod(method).invoke(this)
    } else {
      throw new Exception("Unknown or unavailable method")
    }
  }
  
  def getAvailableReports: Array[MSDiagOutput] = {
    val msdiags = new ArrayBuffer[MSDiagOutput]
    this.getMethods.foreach(m => {
      this.executeMethod(m) match {
        case msd: MSDiagOutput => msdiags += msd
        case _ => logger.debug("Method "+m+" returned an unexpected MSDiag output type")
      }
    })
    msdiags.toArray
  }

  def getMSDiag_assignementRepartition: MSDiagOutput = AssignementRepartition.get(rs, 0)
  def getMSDiag_matchesPerResultSetAndScore: MSDiagOutput = MatchesPerResultSetAndScore.get(rs, scoreWindow, maxRank, 1)
  def getMSDiag_matchesPerChargeAndScore: MSDiagOutput = MatchesPerChargeAndScore.get(rs, scoreWindow, maxRank, 2)
  def getMSDiag_massesPerChargeAndScore: MSDiagOutput = MassesPerChargeAndScore.get(rs, scoreWindow, maxRank, 3)
  def getMSDiag_matchesPerMinuteAndScore: MSDiagOutput = MatchesPerMinuteAndScore.get(rs, scoreWindow, maxRank, 4)
  
//  def getMSDiagMatchesPerChargeAndScore: MSDiagOutput = MatchesPerChargeAndScore.get(rs, scoreWindow, maxRank)
//  def getMSDiagMatchesPerMinuteAndScore: MSDiagOutput = MatchesPerMinuteAndScore.get(rs, scoreWindow, maxRank)
//  def getMSDiagMatchesPerResultSetAndScore: MSDiagOutput = MatchesPerResultSetAndScore.get(rs, scoreWindow, maxRank)
//  def getMSDiagMassesPerChargeAndScore: MSDiagOutput = MassesPerChargeAndScore.get(rs, scoreWindow, maxRank)
//  def getMSDiagAssignementRepartition: MSDiagOutput = AssignementRepartition.get(rs)
  // not used anymore
//  def getMSDiagMatchesPerScanAndScore: MSDiagOutput = MatchesPerScanAndScore.get(rs, scoreWindow, maxRank, nbScansPerGroup)
//  def getMSDiagMassesPerCharge: MSDiagOutput = MassesPerCharge.get(rs, maxRank)
//  def getMSDiagMassesPerScore: MSDiagOutput = null

}
