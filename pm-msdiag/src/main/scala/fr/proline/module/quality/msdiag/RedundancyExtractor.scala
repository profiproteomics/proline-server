package fr.proline.module.quality.msdiag

import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.core.om.model.msi.PeptideMatch
import scala.collection.mutable.ArrayBuffer

object RedundancyExtractor extends Logging {

  def getRedundantMatches(peptideMatches: Array[PeptideMatch]): Array[PeptideMatch] = {
    val redundantMatches = new ArrayBuffer[PeptideMatch]
    if (peptideMatches != null && peptideMatches.size > 1) {
      peptideMatches.sortBy(pm => (pm.peptide.sequence, pm.msQuery.charge, pm.peptide.calculatedMass))
      for (i <- 1 until peptideMatches.size) {
        if (isSimilar(peptideMatches(i), peptideMatches(i - 1))) {
          if (i == 1) { redundantMatches += peptideMatches(i - 1) }
          redundantMatches += peptideMatches(i)
        }
      }
    }
    redundantMatches.toArray
  }
  
  private def isSimilar(pm1: PeptideMatch, pm2: PeptideMatch): Boolean = pm1.peptide.sequence == pm2.peptide.sequence &&
    pm1.msQuery.charge == pm2.msQuery.charge &&
    pm1.peptide.calculatedMass == pm2.peptide.calculatedMass
  
}
