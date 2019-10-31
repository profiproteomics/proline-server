package fr.proline.module.parser.mascot

import java.io.PrintWriter
import java.util.Date

import org.junit.Ignore

import com.typesafe.scalalogging.LazyLogging

import fr.proline.core.om.model.msi.PeptideMatch
import fr.profi.util.system.OSInfo

@Ignore
object TestUtils extends LazyLogging {

  private val LATIN_1 = "ISO-8859-1"

  private val SEPARATOR = ", "

  private val LINE_LENGTH = 256

  def savePeptideMatches(peptideMatches: Array[PeptideMatch]) {
    val now = new Date()

    def sorter(left: PeptideMatch, right: PeptideMatch): Boolean = {
      var result: Boolean = false

      val compare = left.peptide.sequence.compareTo(right.peptide.sequence)

      if (compare < 0) {
        result = true
      } else if (compare == 0) {

        if (left.score > right.score) { // Descending score
          result = true
        }

      }

      result
    }

    val sortedPeptideMatches = peptideMatches.sortWith(sorter)

    val osType = OSInfo.getOSType

    val outputFileName = String.format("peptide-matches_%s_%tY%<tm%<td%<tH%<tM%<tS_%<tL.txt", osType.name, now)

    val pw = new PrintWriter(outputFileName, LATIN_1)

    try {
      sortedPeptideMatches.foreach(pepMatch => {
        val line = new StringBuilder(LINE_LENGTH)
        line.append(pepMatch.peptide.sequence).append(SEPARATOR)
        line.append(pepMatch.peptide.ptmString).append(SEPARATOR)
        line.append(pepMatch.rank).append(SEPARATOR)
        line.append(pepMatch.score).append(SEPARATOR)
        line.append(pepMatch.deltaMoz)

        pw.println(line)
      })

    } finally {
      pw.close()

      if (pw.checkError()) {
        logger.error("Error closing " + outputFileName + " file")
      }

    }

  }

}