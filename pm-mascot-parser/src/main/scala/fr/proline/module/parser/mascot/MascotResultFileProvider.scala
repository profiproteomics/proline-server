package fr.proline.module.parser.mascot

import java.io.File
import java.util.Locale

import com.weiglewilczek.slf4s.Logging

import fr.proline.core.om.model.msi.IResultFile
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IResultFileProvider

class MascotResultFileProvider extends IResultFileProvider with Logging {

  val fileType: String = "MascotMSParser"

  def getResultFile(fileLocation: File, importProperties: Map[String, Any], parserContext: ProviderDecoratedExecutionContext): IResultFile = {
    // TEST LMN : Forcing ENGLISH Locale before any Mascot work
    val oldLocale = Locale.getDefault

    if (!Locale.ENGLISH.equals(oldLocale)) {
      logger.info("Forcing ENGLISH Locale before Mascot imports (was " + oldLocale + ')')
    }

    Locale.setDefault(Locale.ENGLISH)
    // End TEST LMN

    new MascotResultFile(fileLocation, importProperties, parserContext)
  }

  val resultFileProperties: Map[String, Class[_]] = {
    val propertiedBuilder = Map.newBuilder[String, Class[_]]
    propertiedBuilder += (MascotParseParams.ION_SCORE_CUTOFF.toString -> Double.getClass())
    propertiedBuilder += (MascotParseParams.SUBSET_THRESHOLD.toString -> Double.getClass())
    propertiedBuilder += (MascotParseParams.PROTEIN_CUTOFF_PVALUE.toString -> Double.getClass())
    propertiedBuilder.result
  }

}