package fr.proline.module.parser.mascot

import com.weiglewilczek.slf4s.Logging
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.model.msi.PtmDefinition
import fr.proline.core.om.provider.msi.IPTMProvider
import scala.collection.mutable
import scala.collection.mutable.HashMap


class MascotPTMHelper(val parserContext: ProviderDecoratedExecutionContext) extends Logging {

  var varPtmDefsByModName = mutable.Map.empty[String, Array[PtmDefinition]]
  var varPtmIndexesByModName = mutable.Map.empty[String, Int]
  var fixedPtmDefsByModName = mutable.Map.empty[String, Array[PtmDefinition]]
  var fixedPtmIndexesByModName = mutable.Map.empty[String, Int]
  
  def getPtmDefsByModName(mascotModsAsStr: String): (mutable.Map[String, Array[PtmDefinition]], mutable.Map[String, Int]) = {

    val ptmProvider = parserContext.getProvider(classOf[IPTMProvider])
    val ptmDefsByModName = new HashMap[String, Array[PtmDefinition]]()
    val ptmIndexesByModName = new HashMap[String, Int]
    
    if (mascotModsAsStr != null && !mascotModsAsStr.isEmpty) {
      var index = 0
      mascotModsAsStr.split(",").foreach { modAsStr =>
        ptmDefsByModName(modAsStr) = MascotPTMUtils.mascotModToPTMDefinitions(ptmProvider, modAsStr)
        ptmIndexesByModName(modAsStr) = index
        index += 1
      }
    }
    (ptmDefsByModName, ptmIndexesByModName)
  }
    
  def createVarPtmDefs(mascotModsAsStr: String): mutable.Map[String, Array[PtmDefinition]] = {
    val maps = getPtmDefsByModName(mascotModsAsStr)
	 varPtmDefsByModName = maps._1
	 varPtmIndexesByModName = maps._2
	 varPtmDefsByModName
  }

  def createFixedPtmDefs(mascotModsAsStr: String): mutable.Map[String, Array[PtmDefinition]] = {
    val maps = getPtmDefsByModName(mascotModsAsStr)
    fixedPtmDefsByModName = maps._1
    fixedPtmIndexesByModName = maps._2 
    fixedPtmDefsByModName
  }


}