package fr.proline.module.parser.mascot

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.util.matching.Regex
import com.weiglewilczek.slf4s.Logging

import fr.proline.util.regex.RegexUtils._
import fr.proline.core.om.builder.PtmDefinitionBuilder
import fr.proline.core.om.model.msi.PtmDefinition
import fr.proline.core.om.model.msi.PtmEvidence
import fr.proline.core.om.model.msi.PtmNames
import fr.proline.core.om.model.msi.PtmLocation
import fr.proline.core.om.model.msi.IonTypes
import fr.proline.core.om.provider.msi.IPTMProvider


object MascotPTMUtils extends Logging {
  
  val accessedPtms = new ArrayBuffer[PtmDefinition]
  val ptmDefsByMascotModName = new HashMap[String,Array[PtmDefinition]]
  var mascotServerURL: String = null
  
  val MascotModRegex = """(.+) \((.+)\)""".r
  
  /**
   * Parse mascotMods, Seq[String] containing the list of PTMs as given by Mascot. 
   * For each PTM, PtmDefinitions will be retrieved using specified IPTMProvider or created if it doesn't exist.
   *  
   * Return an PtmDefinition ArrayBuffer corresponding to mascot "PTM-residue/site" 
   * 
   */
  def mascotModsToPTMDefinitions( ptmProvider: IPTMProvider, mascotMods: Seq[String] ): Array[PtmDefinition] = { 
    mascotMods.flatMap( m => this.mascotModToPTMDefinitions( ptmProvider, m ) ).toArray
  }
  
  /**
   * Parse mascotMod, String corresponding to a PTM as defined by Mascot.
   * For each PTM, PtmDefinitions will be retrieved using specified IPTMProvider or created if it doesn't exist.
   *  
   * Return an PtmDefinition ArrayBuffer corresponding to mascot "PTM-residue/site" 
   * 
   */
  def mascotModToPTMDefinitions( ptmProvider: IPTMProvider, mascotMod: String ): Array[PtmDefinition] = {   
    
    // Check if this Mascot modification has been already retrieved
    if( ptmDefsByMascotModName.contains(mascotMod) ) {
      return ptmDefsByMascotModName(mascotMod)
    }
    
    var ptmDefs = new ArrayBuffer[PtmDefinition]()
//    logger.debug(" Get or create PtmDefinition for string read from Mascot : "+ptmsStr)
    
    var modName = mascotMod
    
    if(mascotServerURL != null) //Read from mascot modification file. 
      return _getPTMDefsFromModFile(ptmProvider, mascotMod )
    
    //No mascotServerURL : Get PTM information from mascot "name"    
    var posConstraint = ""
    if( modName.contains("(") ) { //Split name to ptmName + Residue/Site      
      val MascotModRegex( ptmName, ptmConstraint ) = modName
      modName = ptmName
      posConstraint = ptmConstraint
    }
    
    // Get the map (location->residue) of each residue/site this PTM could be applied to
    val posConstraints = _parsePositionConstraint2(posConstraint)
//      logger.debug(" PTM "+ptmName+" on residues "+residues +"; nbr residue/location : "+locationResidueMap.size)
    
    // Create a PtmDefinition for each position constraint (residue and/or location)
    posConstraints.foreach { case (residue,location) =>
      
    	val resChar = if( residue != None ) residue.get else '\0'
  		val foundPtms = accessedPtms.filter { p => p.names == modName && p.residue == resChar && p.location == location }
  		
  		var nextPtmDef = Option.empty[PtmDefinition]
  		
  		if( foundPtms.length == 1) nextPtmDef = Some(foundPtms(0)) // PtmDefinition Already retrieve or created
  		else {
    		nextPtmDef = ptmProvider.getPtmDefinition(modName, resChar, location)
    		
    		// TODO: DBO => I think it should be forbidden to do this because composition has to be known
    		// FIXME: The fallback should be performed by parsing the Unimod section of the dat file
  			if( nextPtmDef == None ) { //Ptm Definition don't exist. Create One 
        	  				      	  
      	  var ptmIdOpt = ptmProvider.getPtmId(modName) // Get PtmName if exist
      	  var ptmId = 0
      	  
      	  if( ptmIdOpt == Some )
      	    ptmId = ptmIdOpt.get
      	  else
      	    ptmId = PtmNames.generateNewId      	  
      	  
      	  // FIXME Create PtmEvidence : type Precursor is required ! + PtmClassification
  			  val ptmEvidence = new PtmEvidence(
  			      ionType = IonTypes.Precursor,
  			      composition = "UNVALID",
  			      monoMass = Double.MaxValue,
  			      averageMass = Double.MaxValue,
  			      isRequired = true
  			      )
  			    
  			  nextPtmDef = Some( new PtmDefinition(
  			                           id = PtmDefinition.generateNewId,
  			                           location = location.toString,
  			                           names = PtmNames( modName, modName ),
  			                           ptmEvidences = Array(ptmEvidence),
  			                           residue = resChar,
  			                           classification = "Post-translational",
  			                           ptmId = ptmId
  			                       ) )
			  }
  			
			  ptmDefs += nextPtmDef.get
			  accessedPtms += nextPtmDef.get
			
  		} //End Ptm not already created or retrieved
    } //End go through residue
    
    val ptmDefsAsArray = ptmDefs.toArray
    ptmDefsByMascotModName(mascotMod) = ptmDefsAsArray
    
    ptmDefsAsArray
  }
  
    
  /**
   * FIXME : Get PTMs definition from mascot modification file (more information could be retrieve using ms_modification)
   * 
   */
  private def _getPTMDefsFromModFile( ptmProvider:IPTMProvider, ptmsStr: String ): Array[PtmDefinition] = {
    logger.debug(" Use Mascot modification file to get PtmDefinitions")
    return null
  }
  
  
  
  /**
   * Parse posConstraintAsStr containing the list of residues and/or location where a PTM could be applied
   * The String is Mascot formated (ex: "Protein N-term M", "Protein C-term" or "CM")
   * 
   * Return an Array containing tuple Option[residue] - location for each residue/site
   * 
   */
  private def _parsePositionConstraint2( posConstraintAsStr: String ): Array[Tuple2[Option[Char],PtmLocation.Location]] = {
   
    // TODO: create an ArrayBuffer of PtmSpecificity objects
    var posConstraints = new ArrayBuffer[Tuple2[Option[Char],PtmLocation.Location]]
    
    // Copy the variables
    var residuesAsStr = posConstraintAsStr
    var locConstraintAsStr = posConstraintAsStr
      
    // Check if the position constraint contains a location constraint
    if( posConstraintAsStr ~~ "(?i)term" ) {
      
      // If not strict match
      if( posConstraintAsStr =~ "(?i).+term" == false ) {
        // Extract the residue
        val regexMatch = posConstraintAsStr =# """(?i)(.+term)\s+(.+)"""
        locConstraintAsStr = regexMatch.get.group(1)
        residuesAsStr = regexMatch.get.group(2)
      } else {
        residuesAsStr = ""
      }
      
      // Add '-' char if missing
      if( locConstraintAsStr ~~ "(?i)-term" == false ) { locConstraintAsStr = locConstraintAsStr.replaceFirst("term", "-term")}
      
      // If the location constraint is not specific to the protein
      if( locConstraintAsStr ~~ "(?i)protein" == false ) { locConstraintAsStr = "Any " + locConstraintAsStr }
      // Else if the space is missing between protein and [NC]-term
      else  {
        val modSuffixMatch = (locConstraintAsStr =# "(?i)protein([NC]-term)" )
        //val modSuffix = """(?i)protein([NC]-term)""".r.findFirstMatchIn(locConstraintAsStr).get.group(1)
        
        if( modSuffixMatch != None ) {
          locConstraintAsStr = "Protein " + modSuffixMatch.get.group(1)
        }
      }
      
    }
    else { locConstraintAsStr = "Anywhere" }
    
    // Try to convert the parsed string into an entry of the Location enum
    var locConstraint: PtmLocation.Location = null
    try {
      locConstraint = PtmLocation.withName(locConstraintAsStr)
    } catch {
      case e: Exception => throw new Exception("can't parse Mascot PTM position constraint")
    }
    
    val residues = residuesAsStr.toCharArray()
    if( residues.length > 0 ) {
      for( residue <- residues ) {
        posConstraints += ( Some(residue) -> locConstraint )
      }
    } else {
      posConstraints += ( None -> locConstraint )
    }
    
    posConstraints.toArray
  }

}