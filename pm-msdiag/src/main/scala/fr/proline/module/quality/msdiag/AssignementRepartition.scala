package fr.proline.module.quality.msdiag

import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.module.quality.msdiag.msi.MSDiagOutput
import fr.proline.module.quality.msdiag.msi.MSDiagResultSetManager
import fr.proline.module.quality.msdiag.msi.MSDiagUtils
import fr.proline.module.quality.msdiag.msi.MSDiagOutputTypes

/*
 * This is what it should return 
| Unassigned | Assigned |
|       1903 |     1305 |
 */

object AssignementRepartition extends Logging {

  def get(rs: MSDiagResultSetManager): MSDiagOutput = {
    
    val columnNames = Array[String]("Unassigned", "Assigned")
    val data = Array.ofDim[Any](1, 2)
    data(0)(0) = rs.getUnassignedQueries.size
    data(0)(1) = rs.getAllMsQueries.size - rs.getUnassignedQueries.size

    // return output
    new MSDiagOutput(
      matrix = data,
      outputType = MSDiagOutputTypes.Pie,
      description = "Repartition of assigned and unassigned spectra",
      columnNames = columnNames.toSeq)
  }
}
