package fr.proline.module.exporter.dataset


import fr.proline.module.exporter.api.template.IViewTemplate
import fr.proline.module.exporter.api.view.IDataView
import fr.proline.module.exporter.commons.XDatasetExporter
import fr.proline.module.exporter.dataset.view.BuildDatasetView
import fr.proline.module.exporter.dataset.view.IdentDataSet
import fr.proline.module.exporter.commons.config.ExportConfig
import fr.proline.module.exporter.commons.config.view.DatasetViewTypes


class DatasetExporter (
		val dataView: IDataView,
		val template: IViewTemplate,
		val exportConfig :ExportConfig
		) extends XDatasetExporter {

	def this(
			ds: IdentDataSet,
			viewType: DatasetViewTypes.Value,
			template: IViewTemplate, 
			exportConfig:ExportConfig
			) = {
		this(BuildDatasetView( ds, viewType, exportConfig), template, exportConfig)
	}
}