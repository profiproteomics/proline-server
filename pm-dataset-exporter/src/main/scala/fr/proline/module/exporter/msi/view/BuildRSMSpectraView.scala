package fr.proline.module.exporter.msi.view

import fr.proline.core.om.model.msi._
import fr.proline.core.om.model.msq.MasterQuantPeptideIon
import fr.proline.module.exporter.api.view.{IDataView, IViewTypeEnumeration}
import fr.proline.module.exporter.commons.config.ExportConfig

import scala.collection.mutable.HashMap

case class IdentWithSpectrumDataSet (
                                      resultSummary: ResultSummary,
                                      bestPeptideMatchesByPeptideAndCharge: Map[(Peptide, Int), PeptideMatch],
                                      spectrumByPepMatchId : Map[Long, Spectrum],
                                      spectrumMatchesByPeptMatchId : HashMap[Long, SpectrumMatch],
                                      masterQuantPepIonByPepMatchId :  Option[Map[Long, MasterQuantPeptideIon]]
)

object BuildRSMSpectraView {

  private def _builders(exportConfig: ExportConfig, mode: FormatCompatibility.Value): Map[IViewTypeEnumeration#Value, IdentWithSpectrumDataSet => IDataView] = Map(
    RSMSpectraViewType.SPECTRA_LIST -> { ds: IdentWithSpectrumDataSet =>
      if (mode == FormatCompatibility.PEAKVIEW) {
        new PeakViewSpectraListView(ds, exportConfig)
      } else {
        new SpectronautSpectraListView(ds, exportConfig)
      }
    }
  )

  def apply(identDS: IdentWithSpectrumDataSet, viewType: IViewTypeEnumeration#Value, exportConfig: ExportConfig, mode: FormatCompatibility.Value): IDataView = {
    _builders(exportConfig, mode)(viewType)(identDS)
  }
  
  
}