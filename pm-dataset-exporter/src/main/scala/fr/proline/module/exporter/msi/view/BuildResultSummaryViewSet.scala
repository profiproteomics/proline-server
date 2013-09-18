package fr.proline.module.exporter.msi.view

import fr.proline.core.om.model.msi.ResultSummary
import fr.proline.module.exporter.api.view.IDatasetView
import fr.proline.module.exporter.api.template.ViewWithTemplate
import fr.proline.module.exporter.api.template._

object BuildResultSummaryViewSet {

  def apply(rsm: ResultSummary, viewSetName: String, viewSetTemplate: IViewSetTemplate ): ResultSummaryViewSet = {
    
    val templatedViews = viewSetTemplate.templatedViewTypes.map { templatedViewType =>
      val viewWithTpl = ViewWithTemplate( BuildResultSummaryView(rsm,templatedViewType.viewType), templatedViewType.template )
      if( templatedViewType.viewName.isDefined ) viewWithTpl.datasetView.viewName = templatedViewType.viewName.get
      
      viewWithTpl
    }
    
    new ResultSummaryViewSet(viewSetName,templatedViews)
  }
  
}