package fr.proline.module.exporter.msi.view

import scala.collection.mutable.HashMap

import fr.proline.core.om.model.msi.ResultSummary
import fr.proline.core.om.model.msi.Spectrum
import fr.proline.core.om.model.msi.SpectrumMatch

import fr.proline.core.om.model.msq.MasterQuantPeptide
import fr.proline.module.exporter.api.view.IDataView
import fr.proline.module.exporter.api.view.IViewTypeEnumeration
import fr.proline.module.exporter.commons.config.ExportConfig

case class IdentWithSpectrumDataSet ( 
  resultSummary: ResultSummary,
  sharedPepMatchIds : Array[Long],
  spectrumByPepMatchId : Map[Long, Spectrum],
  spectrumMatchesByPeptMatchId : HashMap[Long, SpectrumMatch],
  masterQuantPepByPepId :  Option[Map[Long, MasterQuantPeptide]]
)

object BuildRSMSpectraView {

  private def _builders(exportConfig: ExportConfig): Map[IViewTypeEnumeration#Value, IdentWithSpectrumDataSet => IDataView ] = Map( 
    RSMSpectraViewType.SPECTRA_LIST -> { ds: IdentWithSpectrumDataSet => new SpectraListView(ds, exportConfig) }        
  )
  def apply( identDS: IdentWithSpectrumDataSet, viewType: IViewTypeEnumeration#Value, exportConfig: ExportConfig ): IDataView = {
    _builders(exportConfig)(viewType)(identDS)
  }
  
  
}