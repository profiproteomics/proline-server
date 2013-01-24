package fr.proline.module.parser.provider.fake

import fr.proline.core.om.provider.msi.IPTMProvider
import fr.proline.core.om.model.msi.PtmDefinition
import fr.proline.core.om.model.msi.PtmLocation
import fr.proline.repository.DatabaseContext

/**
 * Return only no value (Option.empty) 
 */
object EmptyPTMProvider extends IPTMProvider {
  
  val psDbCtx: DatabaseContext = null
  
  def getPtmDefinitionsAsOptions(ptmDefIds: Seq[Int]): Array[Option[PtmDefinition]] = {
  	val retArray =  new Array[Option[PtmDefinition]](ptmDefIds.length)
  	var index = 0
  	ptmDefIds foreach ( id => {
  	  retArray.update(index, Option.empty[PtmDefinition])
  	  index += 1
  	})
  	
  	retArray
  }
  
  def getPtmDefinitions(ptmDefIds: Seq[Int]): Array[PtmDefinition] = {
    Array.empty[PtmDefinition]
  }
 
  def getPtmDefinition(ptmName: String, ptmResidue: Char, ptmLocation: PtmLocation.Location): Option[PtmDefinition] = {
	  Option.empty[PtmDefinition]
  }
  
  def getPtmId(shortName: String): Option[Int] = {
     Option.empty[Int]
  }
  
}