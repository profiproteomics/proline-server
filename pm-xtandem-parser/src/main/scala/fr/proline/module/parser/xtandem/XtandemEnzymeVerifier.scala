/**
 * XTandemEnzymeVerifier.scala 
 * @author	Ibrahim YAPICI
 * @email		iyapici@unistra.fr
 * @description Use SAX parser to get enzymes from X!Tandem file
 */


package fr.proline.module.parser.xtandem
//Proline
import _root_.fr.proline.core.om.model.msi._
import _root_.fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import _root_.fr.proline.core.om.provider.msi.impl.SQLMsiSearchProvider  //getEnzyme

//Parser
import org.xml.sax._
import org.xml.sax.helpers._
import javax.xml.parsers._

import java.io._
import scala.collection.mutable.ArrayBuffer
import com.typesafe.scalalogging.slf4j.Logging

class XTandemEnzymeVerifier ( val parserContext: ProviderDecoratedExecutionContext
                              ) extends DefaultHandler with Logging {

  var isEnzymesDefinedInDB : Boolean = true

	// flags indicate in which markups we are
  private var inBioml = false; private var inNote = false; private var inGroupParameters = false;
	
  // Mark up
	private var note : String = ""
	
	// buffer allow to collect "info" datas
	private var buffer : StringBuffer = new StringBuffer()
	private var canReadBuffer : Boolean = false
	private var parametersLabel : String = ""
	
	
  private var isSemiSpecific : Boolean = false     // Yes if semi specific input parameters are "yes" : "protein, cleavage semi" or "refine, cleavage semi"
  private var foundEnzymeInDBCount : Int = 0;          // foundEnzymeInDBCount should be equals to inputParametersEnzymeCount
  private var inputParametersEnzymeCount : Int = 0;
  
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
		if(qName.equals("bioml")) {
			inBioml = false
			
		} else if(qName.equals("group") && inBioml && inGroupParameters) {
				inGroupParameters = false 
				
		} else if(qName.equals("note") && inGroupParameters) {
		  note = buffer.toString().replace("\n", "").trim
		  if(inGroupParameters && canReadBuffer && !note.isEmpty) {
        if ((parametersLabel.equals("protein, cleavage semi") || parametersLabel.equals("refine, cleavage semi")) 
             && note.equals("yes")) { // Refine modifications
        isSemiSpecific = true
  
        } else if (parametersLabel.equals("protein, cleavage site") && !note.isEmpty) {  // Format [RK]|{P}, [[X]|[D], ..]
          val msiSearchProvider = new SQLMsiSearchProvider(parserContext.getUDSDbConnectionContext(), parserContext.getMSIDbConnectionContext(), parserContext.getPSDbConnectionContext())
          val commaParts: Array[String] = note.split(",")
          inputParametersEnzymeCount = commaParts.length
          if(inputParametersEnzymeCount == 0) {
            logger.error("There is no cleavage enzyme(s) or the format is not correct")
          }
          var residues : String = ""
          var restrictiveResidues : String = ""
          var site : String = "C-term"
          for (i <- 0 until commaParts.length) {
          
            val pipeParts: Array[String] = commaParts(i).split("\\|")
  
            for(j <- 0 until pipeParts.length){
              if( pipeParts(j).length >2
                  && pipeParts(j).substring(0,1).equals("{") 
                  && pipeParts(j).substring(pipeParts(j).length-1,pipeParts(j).length).equals("}")){
                if (!restrictiveResidues.equals("X")) {
                  restrictiveResidues = pipeParts(j).substring(1, pipeParts(j).length()-1)
                }
                
              } else if (pipeParts(j).length > 2 
                         && pipeParts(j).substring(0,1).equals("[") 
                         && pipeParts(j).substring(pipeParts(j).length-1,pipeParts(j).length).equals("]")
                         ) {
                if (residues.equals("")) {
                  residues = pipeParts(j).substring(1, pipeParts(j).length()-1)
                } else if (j==1 && !pipeParts(j).substring(1,2).equals("X")) {
                  site = "N-term"
                }
              } else {
                logger.error(" Parsing error in Xtandem xml output file : parameter \"protein, cleavage site\"")
              }
            }

            val allEnzymesArray = msiSearchProvider.getAllEnzymes()
              
            var isFirstFoundEnzymeTaken : Boolean = false
            allEnzymesArray.foreach( enz => {
              if(!isFirstFoundEnzymeTaken 
                 && enz.enzymeCleavages.length == 1 
                 && residues.length() == enz.enzymeCleavages.head.residues.length()
                 && restrictiveResidues.length() == enz.enzymeCleavages.head.restrictiveResidues.get.length()
                 && residues.toUpperCase.sorted.equals(enz.enzymeCleavages.head.residues.toUpperCase.sorted)
                 && restrictiveResidues.toUpperCase.sorted.equals(enz.enzymeCleavages.head.restrictiveResidues.get.toUpperCase.sorted)
                 && site.toUpperCase().equals(enz.enzymeCleavages.head.site.toUpperCase())
                 && isSemiSpecific == enz.isSemiSpecific
                 ) {
                  
//                logger.debug("Match found ! Enzyme is  = "+ enz.name + 
//                        ", residues = " + enz.enzymeCleavages.head.residues +
//                        ", restrictiveResidues = " + enz.enzymeCleavages.head.restrictiveResidues +
//                        ", site = " + enz.enzymeCleavages.head.site +
//                        ", isSemiSpecific = " + enz.isSemiSpecific )
                usedEnzymes += enz
                isFirstFoundEnzymeTaken = true
                foundEnzymeInDBCount += 1
              }
            })
          }
          if(foundEnzymeInDBCount == 0 ){
            logger.error("Can't find cleavage enzyme in database")
            isEnzymesDefinedInDB = false 
          } else if(foundEnzymeInDBCount > inputParametersEnzymeCount ) {
            logger.error("Multiple enzymes found in database")
            isEnzymesDefinedInDB = false
          } else if(foundEnzymeInDBCount < inputParametersEnzymeCount ) {
            logger.error("Can't find all enzymes in database")
            isEnzymesDefinedInDB = false
          } 
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
		logger.info("Start of parsing for XTandemEnzymeVerifier")
		
	}
	
	//End of file parsing
	override def endDocument() : Unit = {
	  logger.info("End of parsing for XTandemEnzymeVerifier")
	}
}