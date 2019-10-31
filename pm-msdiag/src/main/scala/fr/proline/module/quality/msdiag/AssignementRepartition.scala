package fr.proline.module.quality.msdiag

//import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.module.quality.msdiag.msi.MSDiagOutput
import fr.proline.module.quality.msdiag.msi.MSDiagResultSetManager
import fr.proline.module.quality.msdiag.msi.MSDiagUtils
import fr.proline.module.quality.msdiag.msi.MSDiagOutputTypes
import com.typesafe.scalalogging.LazyLogging

/*
 * This is what it should return 
| Unassigned | Assigned |
|       1903 |     1305 |
 */

object AssignementRepartition extends LazyLogging {

  def get(rs: MSDiagResultSetManager, preferedOrder: Int = 0): MSDiagOutput = {
    
    val columnNames = Array[String]("Unassigned", "Assigned")
    val columnTypes = Array[String]("Double", "Double")
    val columnCategories = Array[String]("Data", "Data")
    val data = Array.ofDim[Any](1, 2)
    data(0)(0) = rs.getUnassignedQueries.size
    data(0)(1) = rs.getAllMsQueries.size - rs.getUnassignedQueries.size

    // return output
    new MSDiagOutput(
      matrix = data,
      outputType = MSDiagOutputTypes.Pie,
      columnTypes = columnTypes.toSeq,
      columnCategories = columnCategories.toSeq,
      description = "Assigned and unassigned spectra",
      columnNames = columnNames.toSeq,
      preferedOrder = preferedOrder)
  }
}
