package fr.proline.module.exporter.msq.view

import fr.proline.module.exporter.api.view.IDataView
import fr.proline.module.exporter.api.view.IViewTypeEnumeration
import fr.proline.module.exporter.msi.view._
import fr.proline.core.om.model.msq.QuantResultSummary
import fr.proline.core.om.model.msi.ProteinMatch
import fr.proline.core.om.model.msq.ExperimentalDesign
import fr.proline.core.om.model.msq.RatioDefinition
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

case class ProtSetCells(accession: String, description : String, selectionLevel: Int, proteinSetId : Long)   
      
case class QuantiDataSet (
	masterQuantChannelId: Long,
	quantRSM: QuantResultSummary,
	protMatchById: Map[Long, ProteinMatch],
	protSetCellsById: HashMap[Long,ProtSetCells],
	qcIds: Array[Long],
	expDesign: ExperimentalDesign, 
	ratioDefs: Array[RatioDefinition], 
	nameByQchId:  Map[Long,String]
)

object BuildQuantitationView {
	private def _builders: Map[IViewTypeEnumeration#Value, QuantiDataSet => IDataView ] = Map( 
			QuantitationViewTypes.MASTER_QUANT_PEPTIDE_IONS -> { ds: QuantiDataSet => new MasterQuantPeptideIonsView(ds) },
			QuantitationViewTypes.MASTER_QUANT_PEPTIDE -> { ds: QuantiDataSet => new MasterQuantPeptideView(ds) },
			QuantitationViewTypes.MASTER_QUANT_PROTEIN_SETS -> { ds: QuantiDataSet => new MasterQuantProteinSetsView(ds) },
			QuantitationViewTypes.BASIC_MASTER_QUANT_PROTEIN_SETS -> { ds: QuantiDataSet => new BasicMasterQuantProteinSetsView(ds) }
  )

  def apply( quantiDS: QuantiDataSet, viewType: IViewTypeEnumeration#Value ): IDataView = {
    _builders(viewType)(quantiDS)
  }
}