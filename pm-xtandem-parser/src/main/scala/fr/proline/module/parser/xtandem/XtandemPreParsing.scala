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
	
	
	// Existence of useful parameters in Xtandem file
	private var outputHistograms = false
	private var outputParameters = false
	private var outputProteins = false
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
        
		    // Here we test input parameters in terms of compulsory( or not ) values. Some values are OK even there is note available or empty 
		    if (parametersLabel.equals("output, histograms")) {
		      if(!info.equals("yes"))  logger.error("Parameter \'output, histograms\' should be \'yes\' to be parsed by Proline Xtandem parser")
		      else outputHistograms = true
		     
		    } else if (parametersLabel.equals("output, parameters")) {
		    	if(!info.equals("yes"))  logger.error("Parameter \'output, parameters\' should be \'yes\' to be parsed by Proline Xtandem parser")
		      else outputParameters = true
		     
		    }  else if (parametersLabel.equals("output, proteins")) {
		    	if(!info.equals("yes"))  logger.error("Parameter \'output, proteins\' should be \'yes\' to be parsed by Proline Xtandem parser")
		      else outputProteins = true
		     
		    }else if (parametersLabel.equals("output, path")) {
		    	outputPath = true
		      if(!info.isEmpty() && !isXmlFileFormat(info))  logger.warn("Parameter \'output, path\' should be an xml file")
		     
		    } else if (parametersLabel.equals("output, sort results by")) {/*compulsory*/
		      if(info.isEmpty() || (!info.equals("spectrum") && !info.equals("protein"))) /*throw new Exception*/ logger.error("Parameter \'output, sort results by\' should be \'spectrum\' or \'protein\'")
		      else outputSortResultsBy = true
		      
		    } else if(parametersLabel.equals("protein, C-terminal residue modification mass")) {
		    	proteinCterminalResidueModificationMass = true
		      if(!info.isEmpty() && !checkDouble(info).isDefined) logger.error("Parameter \'protein, C-terminal residue modification mass\' should be a real number")
		      
		    } else if(parametersLabel.equals("protein, N-terminal residue modification mass")) {
		    	proteinNterminalResidueModificationMass = true
		      if(!info.isEmpty() && !checkDouble(info).isDefined) logger.error("Parameter \'protein, N-terminal residue modification mass\' should be a real number")
		      
		    } else if(parametersLabel.equals("protein, cleavage semi")){
		      proteinCleavageSemi = true
		      if(!info.isEmpty() && !isYesOrNo(info)) logger.warn("Parameter \'protein, cleavage semi\' should be equals to \'yes\' or \'no\'")
		      
		    } else if(parametersLabel.equals("protein, cleavage site")) {/*compulsory*/
		      if(info.isEmpty() || !isCleavageSiteFormat(info)) logger.error("Parameter \'protein, cleavage site\' should respect following format : \nThis parameter is a formatted text string with three fields. The first and third fields are square - [] - or french - {} - brace pairs, containing single amino acid residue symbols. These two fields are separated by a vertical line, e.g., [KR]|{P}.")
		      else proteinCleavageSite = true
		      
		    } else if(parametersLabel.equals("protein, taxon")){
		      proteinTaxon = true
		      
		    } else if(parametersLabel.equals("refine")) {
		      refine = true
		      if(!info.isEmpty() && !isYesOrNo(info)) logger.warn("Parameter \'refine\' should be equals to \'yes\' or \'no\'")
		    
		    //refine is true
		    } else if(refine && parametersLabel.equals("refine, cleavage semi")) {
		      refineCleavageSemi = true
          if(!info.isEmpty() && !isYesOrNo(info)) logger.warn("Parameter \'refine, cleavage semi\' should be equals to \'yes\' or \'no\'")
          
		    } else if(refine && parametersLabel.equals("refine, modification mass")) {
		    	refineModificationMass = true
          if(!info.isEmpty() && !isPtmWithResidueFormat(info)) logger.error("Parameter \'refine, modification mass\' should respect following format : Mass1@Residu1,Mass2@Residu2,..., MassN@ResiduN")
          
        } else if(refine && parametersLabel.equals("refine, potential C-terminus modifications")) {
        	refinePotentialCterminusModifications = true
          if(!info.isEmpty() && !isPtmCtermSquareBracketsFormat(info))  logger.error("Parameter \'refine, modification mass\' should respect following format : Mass1@],Mass2@],..., MassN@]")
          
        } else if(refine && parametersLabel.equals("refine, potential N-terminus modifications")) {
        	refinePotentialNterminusModifications = true
          if(!info.isEmpty() && !isPtmNtermSquareBracketsFormat(info))  logger.error("Parameter \'refine, modification mass\' should respect following format : Mass1@[,Mass2@[,..., MassN@[")
          
        } else if(refine && parametersLabel.equals("refine, potential modification mass")) {
        	refinePotentialModificationMass = true
          if(!info.isEmpty() && !isPtmWithResidueFormat(info)) logger.error("Parameter \'refine, potential modification mass\' should respect following format : Mass1@Residu1,Mass2@Residu2,..., MassN@ResiduN")
          // end refine is true 
          
        } else if(parametersLabel.contains("residue, modification mass")) {
        	residueModificationMass = true
          if(!info.isEmpty() && !isPtmWithResidueFormat(info)) logger.error("Parameter \'residue, modification mass\' should respect following format : Mass1@Residu1,Mass2@Residu2,..., MassN@ResiduN")
          
        } else if(parametersLabel.equals("residue, potential modification mass")) {
        	residuePotentialModificationMass = true
          if(!info.isEmpty() && !isPtmWithResidueFormat(info)) logger.error("Parameter \'residue, potential modification mass\' should respect following format : Mass1@Residu1,Mass2@Residu2,..., MassN@ResiduN")
          
        } else if(parametersLabel.equals("scoring, include reverse")) {
          scoringIncludeReverse = true 
          if(!info.isEmpty() && !isYesOrNo(info)) logger.warn("Parameter \'scoring, include reverse\' should be equals to \'yes\' or \'no\'")
          
        } else if(parametersLabel.equals("scoring, maximum missed cleavage sites")) {/*compulsory*/
          if(info.isEmpty() || !checkInt(info).isDefined || info.toInt < 0) logger.error("Parameter \'scoring, maximum missed cleavage sites\' should be a pozitive natural number")
          else scoringMaximumMissedCleavageSites = true
          
        } else if(parametersLabel.equals("spectrum, fragment monoisotopic mass error")) {/*compulsory*/
          if(info.isEmpty() || !checkDouble(info).isDefined || info.toDouble < 0) logger.error("Parameter \'spectrum, fragment monoisotopic mass error\' should be a pozitive real number")
          else spectrumFragmentMonoisotopicMassError = true
          
        } else if(parametersLabel.equals("spectrum, fragment monoisotopic mass error units")) {/*compulsory*/
          if(info.isEmpty() || (!info.equals("Daltons") && !info.equals("ppm"))) logger.error("Parameter \'spectrum, fragment monoisotopic mass error units\' should be \'Daltons\' or \'ppm\'")
          spectrumFragmentMonoisotopicMassErrorUnits = true
          
        } else if(parametersLabel.equals("spectrum, maximum parent charge")) {
          if(info.isEmpty() || !checkInt(info).isDefined || info.toInt < 0) logger.error("Parameter \'spectrum, maximum parent charge\' should be a pozitive natural number")
          else spectrumMaximuParentCharge = true
          
        } else if(parametersLabel.equals("spectrum, path")) {
//          if(info.isEmpty() || !isSpectraFileFormat(info)) logger.error("Parameter \'spectrum, path\' should be .gaml, .dta, .pkl, .mgf, .mzdata or .mzXML file")
          spectrumPath = true
          
        } else if(parametersLabel.equals("process, version")) {
          processVersion = true
          
        } else if(parametersLabel.equals("process, version")) {
          processVersion = true
          
        }
        
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
		// generate throw error if structure is wrong or needed markups and labels are missing
	  if(!outputHistograms) println("outputHistograms is false")
	  if(!outputParameters) println("outputParameters is false")
	  if(!outputProteins) println("outputProteins is false")
	  if(!outputPath) println("outputPath is false")
	  if(!outputSortResultsBy) println("outputSortResultsBy is false")
	  if(!proteinCterminalResidueModificationMass) println("proteinCterminalResidueModificationMass is false")
	  if(!proteinNterminalResidueModificationMass) println("proteinNterminalResidueModificationMass is false")
	  if(!proteinCleavageSemi) println("proteinCleavageSemi is false")
	  if(!proteinCleavageSite) println("proteinCleavageSite is false")
	  if(!proteinTaxon) println("proteinTaxon is false")
	  if(!refine) println("refine is false")
	  if(!refineCleavageSemi) println("refineCleavageSemi is false")
	  if(!refineModificationMass) println("refineModificationMass is false")
	  if(!refinePotentialCterminusModifications) println("refinePotentialCterminusModifications is false")
	  if(!refinePotentialNterminusModifications) println("refinePotentialNterminusModifications is false")
	  if(!refinePotentialModificationMass) println("refinePotentialModificationMass is false")
	  if(!residueModificationMass) println("residueModificationMass is false")
	  if(!residuePotentialModificationMass) println("residuePotentialModificationMass is false")
	  if(!scoringIncludeReverse) println("scoringIncludeReverse is false")
	  if(!scoringMaximumMissedCleavageSites) println("scoringMaximumMissedCleavageSites is false")
	  if(!spectrumFragmentMonoisotopicMassError) println("spectrumFragmentMonoisotopicMassError is false")
	  if(!spectrumFragmentMonoisotopicMassErrorUnits) println("spectrumFragmentMonoisotopicMassErrorUnits is false")
	  if(!spectrumMaximuParentCharge) println("spectrumMaximuParentCharge is false")
	  if(!spectrumPath) println("spectrumPath is false")
	  if(!processVersion) println("processVersion is false")
	  if(!outputPath) println("outputPath is false")
	}
	
	// Check Type and return converted value, return None else
	private def checkDouble(s: String) = try { Some(augmentString(s).toDouble) } catch { case _: Throwable => None }
	private def checkInt(s: String) = try { Some(augmentString(s).toInt) } catch { case _: Throwable => None }
	private def checkChar(s: String) = {if(s.length() == 1) Some(s.charAt(0)) else None}
	
	private def isAlphabeticChar(c : Char ) : Boolean = {
	  var result = false
	  val charToInt = c.toInt
    if((charToInt >=65 && charToInt <=90) || (charToInt >=97 && charToInt >=122)) result = true
    result
	}
	
	private def isCleavageSiteFormat(info : String) : Boolean = {
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
        if(!isAlphabeticChar(char)) return false
      })
      
      restrictiveResidue.foreach( char => {
        if(!isAlphabeticChar(char)) return false
      })
    }

    true
	}
	
	def isXmlFileFormat(filePath : String ) : Boolean = { 
	  if(filePath.length > 4 && filePath.substring(filePath.length - 4, filePath.length).equals(".xml")) return true 
	  else return false
	}

//	def isSpectraFileFormat(filePath : String ) : Boolean = {
//	  if(filePath.length > 4 &&
//	      (filePath.substring(filePath.length - 4, filePath.length).toLowerCase.equals(".dta")
//	      || filePath.substring(filePath.length - 4, filePath.length).toLowerCase.equals(".mgf")
//	      || filePath.substring(filePath.length - 4, filePath.length).toLowerCase.equals(".pkl")
//	      )) { return true }
//	  else if (filePath.length > 5 &&
//	      (filePath.substring(filePath.length - 4, filePath.length).toLowerCase.equals(".gaml")
//	      || filePath.substring(filePath.length - 4, filePath.length).toLowerCase.equals(".mzml")
//	      )
//	     ) { return true}
//	  else return false
//	  
//	}
	  
	def isYesOrNo(str : String ) : Boolean = {  str.equals("yes") || str.equals("no")  }
	
	def isPtmWithResidueFormat(ptm : String) : Boolean = {
	  var i = 0
	  val commaParts: Array[String] = ptm.split(",") // Format : M1@X1,M2@X2,.... for, ex 57.022@C,42,010565@C
    for (i <- 0 until commaParts.length) {
      if(commaParts(i).length > 5 && commaParts(i).substring(commaParts(i).length-2, commaParts(i).length-1).equals("@"))
      {
        val atSignParts: Array[String] = commaParts(i).split("@")
        if(!checkDouble(atSignParts(0)).isDefined || atSignParts(1).length != 1 || !isAlphabeticChar(atSignParts(1).head)) return false
      } else {
        return false
      }
    }
	  return true
	}
	
  def isPtmCtermSquareBracketsFormat(ptm : String) : Boolean = {
	  var i = 0
	  val commaParts: Array[String] = ptm.split(",") // Format : M1@X1,M2@X2,.... for, ex 42.010565@[
    for (i <- 0 until commaParts.length) {
      if(commaParts(i).length > 5 && commaParts(i).substring(commaParts(i).length-2, commaParts(i).length-1).equals("@"))
      {
        val atSignParts: Array[String] = commaParts(i).split("@")
        if(!checkDouble(atSignParts(0)).isDefined || atSignParts(1).length != 1 || !atSignParts(1).equals("]")) return false
      } else {
        return false
      }
    }
	  return true
	}
  	
  def isPtmNtermSquareBracketsFormat(ptm : String) : Boolean = {
    var isFormatOK = true
	  var i = 0
	  val commaParts: Array[String] = ptm.split(",") // Format : M1@X1,M2@X2,.... for, ex 42.010565@[
    for (i <- 0 until commaParts.length) {
      if(commaParts(i).length > 5 && commaParts(i).substring(commaParts(i).length-2, commaParts(i).length-1).equals("@"))
      {
        val atSignParts: Array[String] = commaParts(i).split("@")
        if(!checkDouble(atSignParts(0)).isDefined || atSignParts(1).length != 1 || !atSignParts(1).equals("[")) return false
      } else {
        return false
      }
    }
	  return true
	}
  
}
