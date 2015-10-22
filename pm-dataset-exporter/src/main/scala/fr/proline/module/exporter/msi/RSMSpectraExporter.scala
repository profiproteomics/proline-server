package fr.proline.module.exporter.msi

import fr.proline.module.exporter.api.template.IViewTemplate
import fr.proline.module.exporter.api.view.IDataView
import fr.proline.module.exporter.commons.XDatasetExporter
import fr.proline.module.exporter.commons.config.ExportConfig
import fr.proline.module.exporter.msi.view.BuildRSMSpectraView
import fr.proline.module.exporter.msi.view.IdentWithSpectrumDataSet
import fr.proline.module.exporter.msi.view.RSMSpectraViewTypes

/**
 * @author VD225637
 */
class RSMSpectraExporter(
  val dataView: IDataView,
  val template: IViewTemplate,
  val exportConfig: ExportConfig) extends XDatasetExporter {

  def this(
    ds: IdentWithSpectrumDataSet,
    viewType: RSMSpectraViewTypes.Value,
    template: IViewTemplate,
    exportConfig: ExportConfig) = {
    this(BuildRSMSpectraView(ds, viewType), template, exportConfig)
  }
}