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

class XtandemPreParsing(  val xtandemFilePath : String, 
                          val parserContext: ProviderDecoratedExecutionContext
                          ) extends DefaultHandler with Logging {

  var isPTMsDefinedInDB : Boolean = true
  var isEnzymesDefinedInDB : Boolean = true
  var isMarkUpTestOK : Boolean = false
  var isFileTestOK : Boolean = false
  
  def xtandemFileTest {
    try {
      val file = new File(xtandemFilePath)
    } catch {
      case e: IOException => logger.error("File test IOException : " + e.getMessage())
      case e: Throwable => logger.error("File test Throwable : " + e.getMessage())
    }
    isFileTestOK = true
  }

	// Management of the parser
	private var displayTree = false	// Display tree structure of xml file in console

	// flags indicate in which markups we are
  private var inBioml = false; private var inGroupModel = false; private var inProtein = false; private var inNote = false; private var inFileMarkup = false;
	private var inPeptide = false; private var inDomain = false; private var inAAMarkup = false; private var inGroupSupport = false; private var inGroupParameters = false;
	private var inGAMLTrace = false; private var inGAMLAttribute = false; private var inGAMLXdata = false; private var inGAMLYdata = false; private var inGAMLValues = false ;
	
  // Mark up
	private var note : String = ""
	
	// buffer allow to collect "info" datas
	private var buffer : StringBuffer = new StringBuffer()
	private var canReadBuffer : Boolean = false
	private var parametersLabel : String = ""
	
	private val ptmProvider: IPTMProvider = parserContext.getProvider(classOf[IPTMProvider])
  private val ptmMonoMassMargin = 0.03
	private var fixedPtms: ArrayBuffer[Tuple3[Double, Char, PtmLocation.Value]] = new ArrayBuffer[Tuple3[Double, Char, PtmLocation.Value]]
  private var variablePtms: ArrayBuffer[Tuple3[Double, Char, PtmLocation.Value]] = new ArrayBuffer[Tuple3[Double, Char, PtmLocation.Value]]
	var fixedPtmDefs: ArrayBuffer[PtmDefinition] = new ArrayBuffer[PtmDefinition]
  var variablePtmDefs: ArrayBuffer[PtmDefinition] = new ArrayBuffer[PtmDefinition]
	variablePtmDefs.append(ptmProvider.getPtmDefinition(-17.02655, ptmMonoMassMargin, 'Q', PtmLocation.ANY_N_TERM).get)
  variablePtmDefs.append(ptmProvider.getPtmDefinition(-17.02655, ptmMonoMassMargin, 'C', PtmLocation.ANY_N_TERM).get)
  variablePtmDefs.append(ptmProvider.getPtmDefinition(-18.01056, ptmMonoMassMargin, 'E', PtmLocation.ANY_N_TERM).get)
  
  private var residueModificationMassCount = 1     // Increment number of label : "residue, modification mass 1", "residue, modification mass 2", ...
	private var refineParamIsYes: Boolean = false    // considering or not input parameters with "refine" label  
  private var isSemiSpecific : Boolean = false     // Yes if semi specific input parameters are yes : "protein, cleavage semi" or "refine, cleavage semi"
  private var inputParametersEnzymeCount : Int = 0
  
  var usedEnzymes : ArrayBuffer[Enzyme] = new ArrayBuffer[Enzyme]
  
	// detection of opening of markup
	override def startElement(uri : String , localName : String ,
			qName : String, attributes : Attributes) : Unit = {
		if(qName.equals("bioml")) {

			inBioml = true
		} else if(qName.equals("group") && inBioml) {
			val typeMU = attributes.getValue("type")
			if(typeMU.equals("model")) {
				inGroupModel = true

			} else if(typeMU.equals("support") && inGroupModel) {
				inGroupSupport = true
				
			} else if(typeMU.equals("parameters")){

				inGroupParameters = true
			}

		} else if(qName.equals("protein") && inGroupModel) {
			inProtein = true
			
		} else if(qName.equals("note") && (inProtein  || inGroupSupport || inGroupParameters )) {
		  
			if(inGroupParameters ) {
			  if(//PTMs
          attributes.getValue("label").contains("residue, modification mass") 
          || attributes.getValue("label").equals("residue, potential modification mass")
          || attributes.getValue("label").equals("protein, C-terminal residue modification mass")
          || attributes.getValue("label").equals("protein, N-terminal residue modification mass")
          || attributes.getValue("label").equals("refine")    // refine should be yes
          || attributes.getValue("label").equals("refine, modification mass")
          || attributes.getValue("label").equals("refine, potential modification mass")
          || attributes.getValue("label").equals("refine, potential C-terminus modifications")
          || attributes.getValue("label").equals("refine, potential N-terminus modifications")
          //Enzymes
          || (attributes.getValue("label").equals("protein, cleavage semi") || attributes.getValue("label").equals("refine, cleavage semi"))
          || attributes.getValue("label").equals("protein, cleavage site")
			    ){
			    parametersLabel = attributes.getValue("label")
			    canReadBuffer = true
			  } else {
			    canReadBuffer = false
			  }
			}
			
			inNote = true
			
		} else if(qName.equals("file") && inProtein) {
			inFileMarkup = true
			
		} else if(qName.equals("peptide") && inProtein) {
			inPeptide = true
			
		} else if(qName.equals("domain") && inPeptide) {
			inDomain = true
			
		} else if(qName.equals("aa") && inDomain) {
			inAAMarkup = true
			
		} else if(qName.equals("GAML:trace") && inGroupSupport) {
			inGAMLTrace = true
			
		} else if(qName.equals("GAML:attribute") && inGAMLTrace) {
			inGAMLAttribute = true
			
		} else if(qName.equals("GAML:Xdata") && inGAMLTrace) {
			inGAMLXdata = true
			
		} else if(qName.equals("GAML:Ydata") && inGAMLTrace) {
			inGAMLYdata = true

		} else if(qName.equals("GAML:values") && (inGAMLXdata || inGAMLYdata)) {
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
		  note = buffer.toString().replace("\n", "").trim
		  if(inGroupParameters && canReadBuffer && !note.isEmpty) {
        if (parametersLabel.equals("output, sort results by")) {
          if(note.equals("protein") || note.isEmpty) logger.error("Xtandem Parser does not manage protein sorted Xtandem File")
          else if(!note.equals("spectrum")) logger.error("Parameter \'sort results by\' should be \'spectrum\' to be manage by Xtandemm Parser")  //This case shouldn't be appear

        } else if (parametersLabel.contains("residue, modification mass")) {
        // Protein fixed anywhere ptms
        if (parametersLabel.equals("residue, modification mass")) {
        	val commaParts: Array[String] = note.split(",") // Format : M1@X1,M2@X2,.... for, ex 57.022@C,42,010565@C
          for (i <- 0 until commaParts.length) {
            val atSignParts: Array[String] = commaParts(i).split("@")
            fixedPtms.append(Tuple3(augmentString(atSignParts(0)).toDouble, atSignParts(1).charAt(0), PtmLocation.ANYWHERE))
          }
  
        } else if (parametersLabel.equals("residue, modification mass ".concat(residueModificationMassCount.toString()))) {
          // If it contains several fixed PTMs in several lines
          val commaParts: Array[String] = note.split(",") // Format : M1@X1,M2@X2,.... for, ex 57.022@C,42,010565@C
          for (i <- 0 until commaParts.length) {
            val atSignParts: Array[String] = commaParts(i).split("@")
            fixedPtms.append(Tuple3(augmentString(atSignParts(0)).toDouble, atSignParts(1).charAt(0), PtmLocation.ANYWHERE))
          }
          residueModificationMassCount += 1
        }
  
      } else if (parametersLabel.equals("residue, potential modification mass")) {
        // Protein variable anywhere PTMs
        val commaParts: Array[String] = note.split(",") // Format : M1@X1,M2@X2,.... for, ex 57.022@C,42,010565@C
        for (i <- 0 until commaParts.length) {
          val atSignParts: Array[String] = commaParts(i).split("@")
          variablePtms.append(Tuple3(augmentString(atSignParts(0)).toDouble, atSignParts(1).charAt(0), PtmLocation.ANYWHERE))
        }
  
      } else if (parametersLabel.equals("protein, C-terminal residue modification mass") && augmentString(note).toDouble != 0.0) { // 0.0 is the default value for this paramater in XTandem
        // Protein fixed C-term PTMs
        fixedPtms.append(Tuple3(augmentString(note).toDouble, '\0', PtmLocation.PROT_C_TERM))
  
      } else if (parametersLabel.equals("protein, N-terminal residue modification mass") && augmentString(note).toDouble != 0.0) { // fixed ptms
        // Protein fixed N-term PTM
        fixedPtms.append(Tuple3(augmentString(note).toDouble, '\0', PtmLocation.PROT_N_TERM))
  
      } else if (parametersLabel.equals("refine") && note.equals("yes")) { // Refine modifications
        refineParamIsYes = true
  
      } else if ((parametersLabel.equals("protein, cleavage semi") || parametersLabel.equals("refine, cleavage semi")) 
                  && note.equals("yes")) { // Refine modifications
        isSemiSpecific = true
  
      } else if (refineParamIsYes && parametersLabel.equals("refine, modification mass") && !note.isEmpty) { // variable ptms
        // Protein fixed anywhere PTM
        val commaParts: Array[String] = note.split(",") // Format : M1@X1,M2@X2,.... for, ex 57.022@C,42,010565@C
        for (i <- 0 until commaParts.length) {
          val atSignParts: Array[String] = commaParts(i).split("@")
          fixedPtms.append(Tuple3(augmentString(atSignParts(0)).toDouble, atSignParts(1).charAt(0), PtmLocation.ANYWHERE))
        }
  
      } else if (refineParamIsYes && parametersLabel.equals("refine, potential modification mass") && !note.isEmpty) { // variable ptms
        // Protein variable anywhere PTM
        val commaParts: Array[String] = note.split(",") // Format : M1@X1,M2@X2,.... for, ex 57.022@C,42,010565@C
        for (i <- 0 until commaParts.length) {
          val atSignParts: Array[String] = commaParts(i).split("@")
          variablePtms.append(Tuple3(augmentString(atSignParts(0)).toDouble, atSignParts(1).charAt(0), PtmLocation.ANYWHERE))
        }
  
      } else if (refineParamIsYes && parametersLabel.equals("refine, potential C-terminus modifications") && !note.isEmpty) { // variable ptms
        // Protein variable C-term PTM
        val commaParts: Array[String] = note.split(",") // Format : M1@X1,M2@X2,.... for, ex 57.022@C,42,010565@C
        for (i <- 0 until commaParts.length) {
          val atSignParts: Array[String] = commaParts(i).split("@")
          val residue = if (atSignParts(1).charAt(0) == '[') '\0' else atSignParts(1).charAt(0)
          variablePtms.append(Tuple3(augmentString(atSignParts(0)).toDouble, residue, PtmLocation.PROT_C_TERM))
        }
  
      } else if (refineParamIsYes && parametersLabel.equals("refine, potential N-terminus modifications") && !note.isEmpty) { // variable ptms
        // Protein variable N-term PTM
        val commaParts: Array[String] = note.split(",") // Format : M1@X1,M2@X2,.... for, ex 57.022@C,42,010565@C
        for (i <- 0 until commaParts.length) {
          val atSignParts: Array[String] = commaParts(i).split("@")
          val residue = if (atSignParts(1).charAt(0) == '[') '\0' else atSignParts(1).charAt(0)
          variablePtms.append(Tuple3(augmentString(atSignParts(0)).toDouble, residue, PtmLocation.PROT_N_TERM))
        }
      } else if (parametersLabel.equals("protein, cleavage site") && !note.isEmpty) {  // Format [RK]|{P}, [[X]|[D], ..]
        val msiSearchProvider = new SQLMsiSearchProvider(parserContext.getUDSDbConnectionContext(), parserContext.getMSIDbConnectionContext(), parserContext.getPSDbConnectionContext())
        val commaParts: Array[String] = note.split(",")
        inputParametersEnzymeCount = commaParts.length
        if(inputParametersEnzymeCount == 0) {
          logger.error("There is no cleavage enzyme(s) or the format is not correct")
          } else {
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
                
            allEnzymesArray.foreach( enz => {
              if( usedEnzymes.length == 0   // Get first found enzyme 
                   && enz.enzymeCleavages.length == 1 
                 && residues.length() == enz.enzymeCleavages.head.residues.length()
                 && restrictiveResidues.length() == enz.enzymeCleavages.head.restrictiveResidues.get.length()
                 && residues.toUpperCase.sorted.equals(enz.enzymeCleavages.head.residues.toUpperCase.sorted)
                 && restrictiveResidues.toUpperCase.sorted.equals(enz.enzymeCleavages.head.restrictiveResidues.get.toUpperCase.sorted)
                 && site.toUpperCase().equals(enz.enzymeCleavages.head.site.toUpperCase())
                 && isSemiSpecific == enz.isSemiSpecific
                 ) {
                  
                logger.info("Match found ! Enzyme is  = "+ enz.name + 
                        ", residues = " + enz.enzymeCleavages.head.residues +
                        ", restrictiveResidues = " + enz.enzymeCleavages.head.restrictiveResidues +
                        ", site = " + enz.enzymeCleavages.head.site +
                        ", isSemiSpecific = " + enz.isSemiSpecific )
                usedEnzymes += enz
              }
            })
            }
            logger.debug("usedEnzymes.length = " + usedEnzymes.length)
  //          logger.debug("inputParametersEnzymeCount = " + inputParametersEnzymeCount)
            if(usedEnzymes.length == 0 ){
              logger.error("Can't find cleavage enzyme in database")
              isEnzymesDefinedInDB = false 
            }
          }
      }
	  }
			buffer.delete(0, buffer.length())
			inNote = false
			
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
	}
	
	//Start of file parsing
	override def startDocument() : Unit = {
	  xtandemFileTest
		logger.info("Start of parsing for XTandemPreParsing")
	}
	
	//End of file parsing
	override def endDocument() : Unit = {
	  logger.info("End of parsing for XTandemPreParsing")
		isMarkUpTestOK = true
		
		fixedPtms.foreach(ptms => {
      val _ptm = ptmProvider.getPtmDefinition(ptms._1, ptmMonoMassMargin, ptms._2, ptms._3)
      if (_ptm.get != null) { fixedPtmDefs.append(_ptm.get); /*logger.debug("fixedPtms _ptm = " + _ptm.get + ", monoMass = " + _ptm.get.ptmEvidences(0).monoMass)*/ }
      else { /*_ptm = Some(null) */ logger.error("Ptm don't exists in database : mono mass = " + ptms._1 + ", residue = " + ptms._2 + ", location = " + ptms._3); isPTMsDefinedInDB = false }
    })
    variablePtms.foreach(ptms => {
      val _ptm = ptmProvider.getPtmDefinition(ptms._1, ptmMonoMassMargin, ptms._2, ptms._3)
      if (_ptm.get != null) { variablePtmDefs.append(_ptm.get); /*logger.debug("variablePtms_ptm = " + _ptm.get + "_ptm.get.ptmEvidences.monoMass = " + _ptm.get.ptmEvidences(0).monoMass)*/ }
      else { /*_ptm = Some(null) */ logger.error("Ptm don't exists in database : mono mass = " + ptms._1 + ", residue = " + ptms._2 + ", location = " + ptms._3); isPTMsDefinedInDB = false }
    })

//  	logger.debug("fixedPtmDefs number is OK = " + (fixedPtmDefs.length == fixedPtms.length))
//    logger.debug("variablePtmDefs number is OK = " + (variablePtmDefs.length == variablePtms.length+3))
    
    if(fixedPtmDefs.length != fixedPtms.length || variablePtmDefs.length != variablePtms.length+3) {
      logger.error("Given PTMs count don't match with PTMs found in database")
      isPTMsDefinedInDB = false
    }
    logger.debug("fixedPtmDefs.length = " + fixedPtmDefs.length)
    logger.debug("variablePtmDefs.length = " + variablePtmDefs.length)
    fixedPtmDefs.foreach(ptm => logger.debug("fixedPtmDefs ptm = " + ptm))
    variablePtmDefs.foreach(ptm => logger.debug("variablePtmDefs ptm = " + ptm))
    usedEnzymes.foreach(enz => logger.debug("usedEnzymes enz = " + enz))
	}
}