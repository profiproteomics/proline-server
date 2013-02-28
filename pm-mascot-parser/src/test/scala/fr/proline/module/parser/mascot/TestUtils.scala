package fr.proline.module.parser.mascot

import java.io.PrintWriter
import java.util.Date

import org.junit.Ignore

import com.weiglewilczek.slf4s.Logging

import fr.proline.core.om.model.msi.PeptideMatch
import fr.proline.util.system.OSInfo

@Ignore
object TestUtils extends Logging {

  private val LATIN_1 = "ISO-8859-1"

  private val SEPARATOR = ", "

  private val LINE_LENGTH = 256

  def savePeptideMatches(peptideMatches: Array[PeptideMatch]) = {
    val now = new Date()

    val osType = OSInfo.getOSType

    val outputFileName = String.format("peptide-matches_%s_%tY%<tm%<td%<tH%<tM%<tS_%<tL.txt", osType.name, now)

    val pw = new PrintWriter(outputFileName, LATIN_1)

    try {
      peptideMatches.foreach(pepMatch => {
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