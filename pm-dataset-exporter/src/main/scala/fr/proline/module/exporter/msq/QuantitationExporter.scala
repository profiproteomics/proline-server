package fr.proline.module.exporter.msq


import fr.proline.module.exporter.api.template.IViewTemplate
import fr.proline.module.exporter.api.view.IDataView
import fr.proline.module.exporter.commons.XDatasetExporter
import fr.proline.module.exporter.msq.view.QuantitationViewTypes
import fr.proline.module.exporter.msq.view.BuildQuantitationView
import fr.proline.module.exporter.msq.view.QuantiDataSet

/*
 * Quantitation Exporter, builds the export file depending of format
 */

class QuantitationExporter (
  
	val dataView: IDataView,
	val template: IViewTemplate
	
) extends XDatasetExporter {
  
  def this(
    ds: QuantiDataSet,
    viewType: QuantitationViewTypes.Value,
    template: IViewTemplate
  ) = {
    this(BuildQuantitationView(ds, viewType), template)
  }
  
}