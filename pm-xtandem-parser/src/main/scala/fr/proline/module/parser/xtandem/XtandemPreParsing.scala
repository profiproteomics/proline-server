/**
 * XTandemPreParsing.scala 
 * @author	Ibrahim YAPICI
 * @email		iyapici@unistra.fr
 * @description Contains following tests :
 *  - existence of PTMS in Proline DB
 *  - existence of enzymes in Proline DB.
 * Sax library is used to parse xml file
 */

package fr.proline.module.parser.xtandem
//Proline
import _root_.fr.proline.core.om.model.msi._
import _root_.fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import _root_.fr.proline.core.om.provider.msi.IPTMProvider
import _root_.fr.proline.core.om.provider.msi.impl.SQLMsiSearchProvider  //getEnzyme

//Parser
import org.xml.sax._
import org.xml.sax.helpers._
import javax.xml.parsers._

import java.io._
import scala.collection.mutable.ArrayBuffer
import com.typesafe.scalalogging.slf4j.Logging

class XtandemPreParsing(  val xtandemFilePath : String 
                          ) extends DefaultHandler with Logging {

  var isMarkUpTestOK : Boolean = false
  var areNeededMarkupPresent : Boolean = false
  
  def xtandemFileTest {
    try {
      val file = new File(xtandemFilePath)
    } catch {
      case e: IOException => logger.error("File test IOException : " + e.getMessage())
      case e: Throwable => logger.error("File test Throwable : " + e.getMessage())
    }
  }

	// Management of the parser
	private var displayTree = false	// Display tree structure of xml file in console

	// flags indicate in which markups we are
  private var inBioml = false; private var inGroupModel = false; private var inProtein = false; private var inNote = false; private var inFileMarkup = false;
	private var inPeptide = false; private var inDomain = false; private var inAAMarkup = false; private var inGroupSupport = false; private var inGroupParameters = false;
	private var inGAMLTrace = false; private var inGAMLAttribute = false; private var inGAMLXdata = false; private var inGAMLYdata = false; private var inGAMLValues = false ;
	
  // To store info/data between opening and closing markup
	private var info : String = ""
	
	// buffer allow to collect "info" datas
	private var buffer : StringBuffer = new StringBuffer()
	private var canReadBuffer : Boolean = false
	private var parametersLabel : String = ""
	
	private var residueModificationMassCount : Int = 0
	private var refineParamIsYes: Boolean = false    // considering or not input parameters with "refine" label
	
	
//	private var parameterCount : Int = 0
	// Existence of useful parameters in Xtandem file
	private var outputPath = false
	private var outputSortResultsBy = false
	private var proteinCterminalResidueModificationMass = false
	private var proteinNterminalResidueModificationMass = false
	private var proteinCleavageSemi = false
	private var proteinCleavageSite = false
	private var proteinTaxon = false
	private var refine = false
	private var refineCleavageSemi = false
	private var refineModificationMass = false
	private var refinePotentialCterminusModifications = false
	private var refinePotentialNterminusModifications = false
	private var refinePotentialModificationMass = false
	private var residueModificationMass = false
	private var residuePotentialModificationMass = false
	private var scoringIncludeReverse = false
	private var scoringMaximumMissedCleavageSites = false
	private var spectrumFragmentMonoisotopicMassError = false
	private var spectrumFragmentMonoisotopicMassErrorUnits = false
	private var spectrumMaximuParentCharge = false
	private var spectrumPath = false
	private var processVersion = false
	
	// detection of opening of markup
	override def startElement(uri : String, localName : String,
			qName : String, attributes : Attributes) : Unit = {
		if(qName.equals("bioml")) {
			if(displayTree) println("bioml")
				inBioml = true
				
		} else if(qName.equals("group") && inBioml) {
			val typeMU = attributes.getValue("type")
			if(typeMU.equals("model")) {
			if(displayTree) println(" - groupModel")
//			  println("attributes.getValue(id) = " + attributes.getValue("id"))
				inGroupModel = true

			} else if(typeMU.equals("support") && inGroupModel) {
				if(displayTree) println(" -  - groupSupport")
				inGroupSupport = true
				
			} else if(typeMU.equals("parameters")){
				if(displayTree) println(" - groupParameters")
				inGroupParameters = true
			}

		} else if(qName.equals("protein") && inGroupModel) {
			if(displayTree) println(" -  - protein")
			inProtein = true
			
		} else if(qName.equals("note") && (inProtein  || inGroupSupport || inGroupParameters )) {
			if(inGroupSupport & displayTree) println(" -  -  -  - note")
			else if(inProtein & displayTree) println(" -  -  - note")
			else if(inGroupParameters & displayTree) println(" -  - note")
			
			parametersLabel = attributes.getValue("label")
			inNote = true
			
		} else if(qName.equals("file") && inProtein) {
			if(displayTree) println(" -  -  - file")
			inFileMarkup = true
			
		} else if(qName.equals("peptide") && inProtein) {
			if(displayTree) println(" -  -  - peptide")
			inPeptide = true
			
		} else if(qName.equals("domain") && inPeptide) {
			if(displayTree) println(" -  -  -  - domain")
			inDomain = true
			
		} else if(qName.equals("aa") && inDomain) {
			if(displayTree) println(" -  -  -  -  - aa")
			inAAMarkup = true
			
		} else if(qName.equals("GAML:trace") && inGroupSupport) {
			if(displayTree) println(" -  -  -  - GAML:trace")
			inGAMLTrace = true
			
		} else if(qName.equals("GAML:attribute") && inGAMLTrace) {
			if(displayTree) println(" -  -  -  -  - GAML:attribute")
			inGAMLAttribute = true
			
		} else if(qName.equals("GAML:Xdata") && inGAMLTrace) {
			if(displayTree) println(" -  -  -  -  -  - GAML:Xdata")
			inGAMLXdata = true
			
		} else if(qName.equals("GAML:Ydata") && inGAMLTrace) {
			if(displayTree) println(" -  -  -  -  -  - GAML:Ydata")
			inGAMLYdata = true

		} else if(qName.equals("GAML:values") && (inGAMLXdata || inGAMLYdata)) {
			if(displayTree) println(" -  -  -  -  -  -  - GAML:values")
				inGAMLValues = true
				
		} else {	// if any markup is not managed
			throw new SAXException("Parsing error : unknown markup : "+qName)
		}
	}
	
	//detection of end of markup
	override def endElement(uri : String, localName : String, qName : String) /*throws SAXException*/: Unit = {
		if(qName.equals("bioml")) {
			inBioml = false
			
		} else if(qName.equals("group") && inBioml) {
			if(inGroupParameters) {
				inGroupParameters = false 
				
			} else if(inGroupSupport) {
				inGroupSupport = false
				
			} else if(inGroupModel) {
				inGroupModel = false
			}
		} else if(qName.equals("protein") && inGroupModel) {
			inProtein = false
			
		} else if(qName.equals("note") && (inProtein || inGroupSupport || inGroupParameters)) {
		  info = buffer.toString().replace("\n", "").trim

		  if(inGroupParameters) {
        
		    if (parametersLabel.equals("output, path")) outputPath = true
		    
		    else if (parametersLabel.equals("output, sort results by")) {
		      if(!info.equals("spectrum") && !info.equals("protein")) /*throw new Exception*/ logger.error("Parameter \'output, path\' should be \'spectrum\' or \'protein\'")
		      else outputSortResultsBy = true
		      
		    } else if(parametersLabel.equals("protein, C-terminal residue modification mass")) {
		      if(!checkDouble(info).isDefined) logger.error("Parameter \'protein, C-terminal residue modification mass\' should be a real number")
		      else proteinCterminalResidueModificationMass = true
		      
		    } else if(parametersLabel.equals("protein, N-terminal residue modification mass")) {
		      if(!checkDouble(info).isDefined) logger.error("Parameter \'protein, N-terminal residue modification mass\' should be a real number")
		      else proteinNterminalResidueModificationMass = true
		      
		    } else if(parametersLabel.equals("protein, cleavage semi")){
		      if(!info.equals("yes") || !info.equals("no") ) logger.error("Parameter \'protein, cleavage semi\' should be equals to \'yes\' or \'no\'")
		      else proteinCleavageSemi = true
		      
		    } else if(parametersLabel.equals("protein, cleavage site")) {
		      if(!isCleavageSiteFormatOK(info)){logger.error("Parameter \'protein, cleavage site\' should respect following format : \nThis parameter is a formatted text string with three fields. The first and third fields are square - [] - or french - {} - brace pairs, containing single amino acid residue symbols. These two fields are separated by a vertical line, e.g., [KR]|{P}.")}
		      else proteinCleavageSite = true
		      
		    }
		    
		    
//		    None.isDefined
//		    Some(1).isDefined
		    
		    
		    

		    
		    else if(parametersLabel.equals("protein, taxon")) proteinTaxon = true
		    else if(parametersLabel.equals("refine")) {
		      refine = true
  		    if(parametersLabel.equals("refine, cleavage semi")) refineCleavageSemi = true
  		    else if(parametersLabel.equals("refine, modification mass")) refineModificationMass = true
  		    else if(parametersLabel.equals("refine, potential C-terminus modifications")) refinePotentialCterminusModifications = true
  		    else if(parametersLabel.equals("refine, potential N-terminus modifications")) refinePotentialNterminusModifications = true
  		    else if(parametersLabel.equals("refine, potential modification mass")) refinePotentialModificationMass = true
		    }
		    else if(parametersLabel.contains("residue, modification mass")) residueModificationMass = true
		    else if(parametersLabel.equals("residue, potential modification mass")) residuePotentialModificationMass = true
		    else if(parametersLabel.equals("scoring, include reverse")) scoringIncludeReverse = true
		    else if(parametersLabel.equals("scoring, maximum missed cleavage sites")) scoringMaximumMissedCleavageSites = true
		    else if(parametersLabel.equals("spectrum, fragment monoisotopic mass error")) spectrumFragmentMonoisotopicMassError = true
		    else if(parametersLabel.equals("spectrum, fragment monoisotopic mass error units")) spectrumFragmentMonoisotopicMassErrorUnits = true
		    else if(parametersLabel.equals("spectrum, maximum parent charge")) spectrumMaximuParentCharge = true
		    else if(parametersLabel.equals("spectrum, path")) spectrumPath = true
		    else if(parametersLabel.equals("process, version")) processVersion = true
//		    println("parameterCount = " + parameterCount)

		  }
			buffer.delete(0, buffer.length())
			inNote = false
//			
		} else if(qName.equals("file") && inProtein) {
			inFileMarkup = false
			
		} else if(qName.equals("peptide") && inProtein) {
			inPeptide = false
			
		} else if(qName.equals("domain") && inPeptide) {
			inDomain = false
			
		} else if(qName.equals("aa") && inDomain) {
			inAAMarkup = false
			
		} else if(qName.equals("GAML:trace") && inGroupSupport) {
			inGAMLTrace = false
			
		} else if(qName.equals("GAML:attribute") && inGAMLTrace) {
			inGAMLAttribute = false
			
		} else if(qName.equals("GAML:Xdata") && inGAMLTrace) {
			inGAMLXdata = false
			
		} else if(qName.equals("GAML:Ydata") && inGAMLTrace) {
			inGAMLYdata = false
			
		} else if(qName.equals("GAML:values") && (inGAMLXdata || inGAMLYdata)) {
			inGAMLValues = false
			
		} else {
			throw new SAXException("Parsing error : unknown markup : "+qName)
		}
	}
	
	//Characters detection
	override def characters(ch : Array[Char],start : Int, length : Int) : Unit = {
		var lecture = new String(ch,start,length)
		if(buffer != null) buffer.append(lecture)
//		println("buffer = " + buffer)
	}
	
	//Start of file parsing
	override def startDocument() : Unit = {
//	  xtandemFileTest
		logger.info("Start of Handler for XTandemPreParsing")
	}
	
	//End of file parsing
	override def endDocument() : Unit = {
	  logger.info("End of Handler for XTandemPreParsing")
//		isMarkUpTestOK = true
		// generate throw error if structure is wrong or needed markups and labels are missing
	}
	
	// Check Type and return converted value, return None else
	def checkDouble(s: String) = try { Some(augmentString(s).toDouble) } catch { case _: Throwable => None }
	def checkInt(s: String) = try { Some(augmentString(s).toInt) } catch { case _: Throwable => None }
	def checkChar(s: String) = {if(s.length() == 1) Some(s.charAt(0)) else None}
//	checkChar("1")
	
	def isCleavageSiteFormatOK(info : String) : Boolean = {
	  // Format [RK]|{P}, [[X]|[D], ..]
    val commaParts: Array[String] = info.split(",")
    var residue : String = ""
    var restrictiveResidue : String = ""
    
    for (i <- 0 until commaParts.length) {
      residue = ""
      restrictiveResidue = ""
      val pipeParts: Array[String] = commaParts(i).split("\\|")
      
      if(pipeParts.length ==2) {
        val leftResidue : String = pipeParts(0).substring(1,pipeParts(0).length-1)
        val rightResidue : String =  pipeParts(1).substring(1,pipeParts(1).length-1)
        
        if( (pipeParts(0).length==3 && pipeParts(0).substring(0,1).equals("{") && pipeParts(0).substring(pipeParts(0).length-1,pipeParts(0).length).equals("}")) 
            || leftResidue.equals("X"))
        {
          residue = rightResidue
          restrictiveResidue = leftResidue
          
        } else if( (pipeParts(1).length==3 && pipeParts(1).substring(0,1).equals("{") && pipeParts(1).substring(pipeParts(1).length-1,pipeParts(1).length).equals("}")) 
            || rightResidue.equals("X"))
        {
          residue = leftResidue
          restrictiveResidue = rightResidue
          
        } else {
          logger.error("Enzyme : can't etablish site of enzyme. For exemple, format should be [KR]|{P} for trypsin")
        }
      } else {
        logger.error("More then 2 cleavages site are found. For exemple, format should be [KR]|{P} for trypsin")
      }
     
      residue.foreach( char => {
        val charToInt = char.toInt
        println("residue char = " + char + ", char.toInt = " + char.toInt)
        if(!(charToInt >=65 && charToInt <=90) && !(charToInt >=97 && charToInt >=122)) return false
      })
      
      restrictiveResidue.foreach( char => {
        val charToInt = char.toInt
        println("restrictiveResidue char = " + char + ", char.toInt = " + char.toInt)
        if(!(charToInt >=65 && charToInt <=90) && !(charToInt >=97 && charToInt >=122)) return false
      })
    }

    true
	}
}
