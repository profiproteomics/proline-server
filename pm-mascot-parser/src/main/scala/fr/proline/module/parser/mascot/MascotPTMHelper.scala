package fr.proline.module.parser.mascot

import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import com.typesafe.scalalogging.LazyLogging

import fr.proline.core.om.model.msi.PtmDefinition
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IPTMProvider


class MascotPTMHelper(val parserContext: ProviderDecoratedExecutionContext) extends LazyLogging  {

  var varPtmDefsByModName = mutable.Map.empty[String, Array[PtmDefinition]]
  var varPtmIndexed = List.empty[String]
  var fixedPtmDefsByModName = mutable.Map.empty[String, Array[PtmDefinition]]
  var fixedPtmIndexed = List.empty[String]
  
  def getPtmDefsByModName(mascotModsAsStr: String): (mutable.Map[String, Array[PtmDefinition]], List[String]) = {

    val ptmProvider = parserContext.getProvider(classOf[IPTMProvider])
    val ptmDefsByModName = new HashMap[String, Array[PtmDefinition]]()
    val ptmIndexes = new ListBuffer[String]
    
    if (mascotModsAsStr != null && !mascotModsAsStr.isEmpty) {
      mascotModsAsStr.split(",").foreach { modAsStr =>
        ptmDefsByModName(modAsStr) = MascotPTMUtils.mascotModToPTMDefinitions(ptmProvider, modAsStr)
        ptmIndexes += modAsStr
      }
    }
    (ptmDefsByModName, ptmIndexes.toList)
  }
    
  def createVarPtmDefs(mascotModsAsStr: String): mutable.Map[String, Array[PtmDefinition]] = {
    val maps = getPtmDefsByModName(mascotModsAsStr)
	 varPtmDefsByModName = maps._1
	 varPtmIndexed = maps._2
	 varPtmDefsByModName
  }

  def indexOfVarPtm(ptm: String) : Int = {
		  varPtmIndexed.indexOf(ptm)
  }
  
  def createFixedPtmDefs(mascotModsAsStr: String): mutable.Map[String, Array[PtmDefinition]] = {
    val maps = getPtmDefsByModName(mascotModsAsStr)
    fixedPtmDefsByModName = maps._1
    fixedPtmIndexed = maps._2 
    fixedPtmDefsByModName
  }

  def indexOfFixedPtm(ptm: String) : Int = {
		  fixedPtmIndexed.indexOf(ptm)
  }

}