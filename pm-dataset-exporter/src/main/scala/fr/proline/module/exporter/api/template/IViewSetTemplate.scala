package fr.proline.module.exporter.api.template

import fr.proline.module.exporter.api.context.IViewSetCreationContext

trait IViewSetTemplate extends IViewTemplate {
  
  def templatedViewTypes: Seq[ViewTypeWithTemplate]
  def viewSetCreationContext: IViewSetCreationContext
  
  lazy val fileExtension = {
    require(
      templatedViewTypes.map(_.template.fileExtension).toSet.size == 1,
      "All templates must have the same file extension"
    )
    
    templatedViewTypes.head.template.fileExtension
  }

}