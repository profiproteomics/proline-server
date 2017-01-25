/**
 * XTandemEnzymeVerifier.scala 
 * @author	Ibrahim YAPICI
 * @email		iyapici@unistra.fr
 * @description Use SAX parser to get enzymes from X!Tandem file
 */


package fr.proline.module.parser.xtandem
//Proline
import fr.proline.core.om.model.msi._
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.impl.SQLMsiSearchProvider  //getEnzyme

import fr.profi.chemistry.model.Enzyme
import fr.profi.chemistry.model.EnzymeCleavage

//Parser
import org.xml.sax._
import org.xml.sax.helpers._
import javax.xml.parsers._

import java.io._
import scala.collection.mutable.ArrayBuffer
import com.typesafe.scalalogging.LazyLogging

object XTandemEnzymeVerifier extends LazyLogging {
  
  def extractEnzymesFromXTandemString(parserContext: ProviderDecoratedExecutionContext, enzymesAsString: String, isSemiSpecific: Boolean): Array[Enzyme] = {
    val enzymes = new ArrayBuffer[Enzyme]
    
    val msiSearchProvider = new SQLMsiSearchProvider(parserContext.getUDSDbConnectionContext(), parserContext.getMSIDbConnectionContext(), parserContext.getPSDbConnectionContext())
    val commaParts: Array[String] = enzymesAsString.split(",")
    val inputParametersEnzymeCount = commaParts.length
    if(inputParametersEnzymeCount == 0) {
      logger.error("There is no cleavage enzyme(s) or the format is not correct")
    }
    var residue : String = ""
    var restrictiveResidue : String = ""
    var site : String = ""
    val allEnzymesArray = msiSearchProvider.getAllEnzymes()
    
    for (i <- 0 until commaParts.length) {
      commaParts(i) match {
        // most probable cases
        case "[RK]|{P}" => enzymes += new Enzyme(id = -1, name = "Trypsin", isSemiSpecific = false, enzymeCleavages = Array(new EnzymeCleavage(id = -1, site = "C-term", residues = "KR", restrictiveResidues = Some("P"))))
        case "[KR]|{P}" => enzymes += new Enzyme(id = -1, name = "Trypsin", isSemiSpecific = false, enzymeCleavages = Array(new EnzymeCleavage(id = -1, site = "C-term", residues = "KR", restrictiveResidues = Some("P"))))
        case "[KR]" => enzymes += new Enzyme(id = -1, name = "Trypsin/P", isSemiSpecific = false, enzymeCleavages = Array(new EnzymeCleavage(id = -1, site = "C-term", residues = "KR", restrictiveResidues = None)))
        case "[RK]" => enzymes += new Enzyme(id = -1, name = "Trypsin/P", isSemiSpecific = false, enzymeCleavages = Array(new EnzymeCleavage(id = -1, site = "C-term", residues = "KR", restrictiveResidues = None)))
        // all other cases
        case _ => 
          residue = ""
          restrictiveResidue = ""
          site = ""
          val pipeParts: Array[String] = commaParts(i).split("\\|")
          if(pipeParts.length ==2) {
            val leftResidue : String = pipeParts(0).substring(1,pipeParts(0).length-1)
            val rightResidue : String =  pipeParts(1).substring(1,pipeParts(1).length-1)
            if((pipeParts(0).length==3 && pipeParts(0).substring(0,1).equals("{") && pipeParts(0).substring(pipeParts(0).length-1,pipeParts(0).length).equals("}")) || pipeParts(0).equals("[X]")) {
              site = "N-term"
              residue = rightResidue
              if(!leftResidue.equals("X")) restrictiveResidue = leftResidue
            } else if((pipeParts(1).length==3 && pipeParts(1).substring(0,1).equals("{") && pipeParts(1).substring(pipeParts(1).length-1,pipeParts(1).length).equals("}")) || pipeParts(1).equals("[X]")) {
              site = "C-term"
              residue = leftResidue
              if(!rightResidue.equals("X")) restrictiveResidue = rightResidue
            } else {
              logger.error("Enzyme : can't etablish site of enzyme")  
            }
          } else {
            logger.error("More then 2 residues are found. Format should be for exemple [KR]|{P} for trypsin")
          }
          val enzyme = findEnzyme(allEnzymesArray, residue, restrictiveResidue, site, isSemiSpecific)
          if(!enzyme.isEmpty) {
            enzymes += enzyme.get 
          }
      }
    } 
    enzymes.toArray
  }
  
  // Find and return first found enzyme in a enzyme array 
	private def findEnzyme(allEnzymesArray : Array[Enzyme], residue : String, restrictiveResidue : String, site : String, semiSpecific : Boolean) : Option[Enzyme] = {
	  logger.debug("allEnzymesArray.length 2 = " + allEnzymesArray.length)
	  allEnzymesArray.foreach( enz => {
      if(enz.enzymeCleavages.length == 1
         && residue.length() == enz.enzymeCleavages.head.residues.length()
         && enz.enzymeCleavages.head.restrictiveResidues != None
         && restrictiveResidue.length() == enz.enzymeCleavages.head.restrictiveResidues.get.length()
         && residue.toUpperCase.sorted.equals(enz.enzymeCleavages.head.residues.toUpperCase.sorted)
         && restrictiveResidue.toUpperCase.sorted.equals(enz.enzymeCleavages.head.restrictiveResidues.get.toUpperCase.sorted)
         && site.toUpperCase().equals(enz.enzymeCleavages.head.site.toUpperCase())
         && semiSpecific == enz.isSemiSpecific
         ) {
//        logger.debug("Enzyme "+enz.name+" matches the search")
        return Some(enz)
      }
    })
    logger.error("No enzyme match for this search")
    None
	}
  
}

class XTandemEnzymeVerifier(val parserContext: ProviderDecoratedExecutionContext) extends DefaultHandler with LazyLogging {
  
	// flags indicate in which markups we are
  private var inBioml = false; private var inNote = false; private var inGroupParameters = false;
	
  // markup
	private var info : String = ""
	
	// buffer allow to collect "info" datas
	private var buffer : StringBuffer = new StringBuffer()
	private var canReadBuffer : Boolean = false
	private var parametersLabel : String = ""
	
  var usedEnzymes : ArrayBuffer[Enzyme] = new ArrayBuffer[Enzyme]
  
	// detection of opening of markup
	override def startElement(uri : String , localName : String ,
			qName : String, attributes : Attributes) : Unit = {
		if(qName.equals("bioml")) {
				inBioml = true
				
		} else if(qName.equals("group") && inBioml) {
			val typeMU = attributes.getValue("type")
			if(typeMU.equals("parameters")){
			  inGroupParameters = true
			}

		} else if(qName.equals("note") && inGroupParameters) {
			if(inGroupParameters ) {
			  if(//Enzymes
          (attributes.getValue("label").equals("protein, cleavage semi") || attributes.getValue("label").equals("refine, cleavage semi"))
          || attributes.getValue("label").equals("protein, cleavage site")
			    ){
			    parametersLabel = attributes.getValue("label")
			    canReadBuffer = true
			  } else {
			    canReadBuffer = false
			  }
			}
			inNote = true
			
		}
	}
	
	//detection of end of markup
	override def endElement(uri : String, localName : String, qName : String) /*throws SAXException*/: Unit = {
	  var isSemiSpecific : Boolean = false     // Yes if semi specific input parameters are "yes" : "protein, cleavage semi" or "refine, cleavage semi"
    var foundEnzymeInDBCount : Int = 0;          // foundEnzymeInDBCount should be equals to inputParametersEnzymeCount
    var inputParametersEnzymeCount : Int = 0;
	  
	  
		if(qName.equals("bioml")) {
			inBioml = false
			
		} else if(qName.equals("group") && inBioml && inGroupParameters) {
				inGroupParameters = false 
				
		} else if(qName.equals("note") && inGroupParameters) {
		  info = buffer.toString().replace("\n", "").trim
		  if(inGroupParameters && canReadBuffer && !info.isEmpty) {
        if ((parametersLabel.equals("protein, cleavage semi") || parametersLabel.equals("refine, cleavage semi")) 
             && info.equals("yes")) { // Refine modifications
        isSemiSpecific = true
  
        } else if (parametersLabel.equals("protein, cleavage site") && !info.isEmpty) {  // Format [RK]|{P}, [[X]|[D], ..]
          usedEnzymes ++= XTandemEnzymeVerifier.extractEnzymesFromXTandemString(parserContext, info, isSemiSpecific)
        }
	    }
			buffer.delete(0, buffer.length())
			inNote = false
		}
	}
	
	//Characters detection
	override def characters(ch : Array[Char],start : Int, length : Int) : Unit = {
		var lecture = new String(ch,start,length)
		if(buffer != null) buffer.append(lecture)
	}
	
	//Start of file parsing
	override def startDocument() : Unit = {
	  logger.info("Start of Handler for XTandemEnzymeVerifier")
	}
	
	//End of file parsing
	override def endDocument() : Unit = {
	  logger.info("End of Handler for XTandemEnzymeVerifier")
	}

}