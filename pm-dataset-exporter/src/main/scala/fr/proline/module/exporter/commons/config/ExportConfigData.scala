package fr.proline.module.exporter.commons.config

/**
 * represents which data to export
 * for now, 
 * - which protein (all or validated proteins[default])
 * - which quant peptide (from best profile[default] or all profile)
 * to export 
 */
case class ExportConfigData(
   //allProteinSet set to false :  RSM is validated result so by default, validated data exported. User may have incorrect result if this parameter is not changed -and is_validated not exported for warning-
  allProteinSet: Boolean = false,
  bestProfile: Boolean = true // export  bestProfile or iterates over profiles
)

object ExportConfigData {
 
  def getDefaultConfig() = ExportConfigData()
}
