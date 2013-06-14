package fr.proline.module.parser.provider.fake

import scala.collection.mutable.HashMap
import fr.proline.core.om.model.msi.IonTypes
import fr.proline.core.om.model.msi.PtmDefinition
import fr.proline.core.om.model.msi.PtmEvidence
import fr.proline.core.om.model.msi.PtmNames
import fr.proline.core.om.provider.msi.IPTMProvider
import fr.proline.core.om.model.msi.PtmLocation
//import fr.proline.repository.DatabaseContext

object PTMFakeProvider extends IPTMProvider { 

  val psDbCtx = null
//  val psDbCtx: DatabaseContext = null
  var ptmDefByName:HashMap[String, PtmDefinition] = new HashMap[String, PtmDefinition]()
    
  /**
   *  Get PtmDefinition with specified ptm (name), residue and location,  if already created using this Provider  
   * or create a new fake one with classification = Post-translational, a fake Precursor PtmEvidence and specified properties
   */
  def getPtmDefinition(ptmName: String, ptmResidue: Char, ptmLocation: PtmLocation.Location): Option[PtmDefinition] = { 
    
    val fullName = ptmName+" ("+ptmResidue+")"
    if(ptmDefByName.contains(fullName))
      return ptmDefByName.get(fullName)  
      
    var newPtmDef:PtmDefinition  = null
    val ptmEvidence = new PtmEvidence( 
          ionType = IonTypes.Precursor,
      		composition = "UNVALID",
      		monoMass = Double.MaxValue,
      		averageMass = Double.MaxValue,
      		isRequired = false
      		)
    
    newPtmDef = new PtmDefinition(
                      id = PtmDefinition.generateNewId(),
                      ptmId = PtmNames.generateNewId,
                      location =ptmLocation.toString,
                      residue = ptmResidue,
                      classification = "Post-translational",
                      names = new PtmNames(shortName = ptmName,fullName = fullName),
                      ptmEvidences = Array(ptmEvidence)
                    )
    ptmDefByName += fullName -> newPtmDef 
    
    Some(newPtmDef)
  }
  
  /**
   * Return an empty array
   */
  def getPtmDefinitionsAsOptions(ptmDefIds: Seq[Long]): Array[Option[PtmDefinition]] = {
  	val retArray =  new Array[Option[PtmDefinition]](1)
  	retArray.update(0, Option.empty[PtmDefinition])	
  	
  	retArray
  }
  
  def getPtmDefinitions(ptmDefIds: Seq[Long]): Array[PtmDefinition] = {
    this.getPtmDefinitionsAsOptions(ptmDefIds).filter( _ != None ).map( _.get )
  }
  
  /**
   * Return an Option.empty Int
   */
  def getPtmId(shortName: String): Option[Long] = {
    Option.empty[Long]
  }

}