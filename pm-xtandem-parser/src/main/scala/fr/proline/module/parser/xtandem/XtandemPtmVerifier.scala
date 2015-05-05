/**
 * XtandemPTMVerifier.scala 
 * @author	Ibrahim YAPICI
 * @email		iyapici@unistra.fr
 * @description Use SAX parser to get PTMS from X!Tandem file
 */

package fr.proline.module.parser.xtandem
//Proline
import fr.proline.core.om.model.msi._
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IPTMProvider

//Parser
import org.xml.sax._
import org.xml.sax.helpers._
import javax.xml.parsers._

import java.io._
import scala.collection.mutable.ArrayBuffer
import com.typesafe.scalalogging.slf4j.Logging

class XtandemPtmVerifier( val parserContext: ProviderDecoratedExecutionContext
                          ) extends DefaultHandler with Logging {

//  var isPTMsDefinedInDB : Boolean = true

	// flags indicate in which markups we are
  private var inBioml = false; private var inNote = false; private var inGroupParameters = false;
	
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
  
  private var residueModificationMassCount = 1     // To count "residue, modification mass 1", "residue, modification mass 2", ...
	private var refineParamIsYes: Boolean = false    // For considering or not input parameters with "refine" label  

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
        if (parametersLabel.contains("residue, modification mass")) {
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
  
      } else if (parametersLabel.equals("protein, N-terminal residue modification mass") && augmentString(note).toDouble != 0.0) {
        // Protein fixed N-term PTM
        fixedPtms.append(Tuple3(augmentString(note).toDouble, '\0', PtmLocation.PROT_N_TERM))
  
      } else if (parametersLabel.equals("refine") && note.equals("yes")) { // Refine modifications
        refineParamIsYes = true
  
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
		logger.info("Start of parsing for XTandemPtmVerifier")
		
	}
	
	//End of file parsing
	override def endDocument() : Unit = {
		logger.info("End of parsing for XTandemPtmVerifier")

		fixedPtms.foreach(ptms => {
      val _ptm = ptmProvider.getPtmDefinition(ptms._1, ptmMonoMassMargin, ptms._2, ptms._3)
      if (_ptm.get != null) { fixedPtmDefs.append(_ptm.get); logger.debug("fixedPtms _ptm = " + _ptm.get + ", monoMass = " + _ptm.get.ptmEvidences(0).monoMass) }
      else { /*_ptm = Some(null) */ logger.error("Ptm don't exists in database : mono mass = " + ptms._1 + ", residue = " + ptms._2 + ", location = " + ptms._3); /*isPTMsDefinedInDB = false*/ }
    })
    variablePtms.foreach(ptms => {
      val _ptm = ptmProvider.getPtmDefinition(ptms._1, ptmMonoMassMargin, ptms._2, ptms._3)
      if (_ptm.get != null) { variablePtmDefs.append(_ptm.get); logger.debug("variablePtms_ptm = " + _ptm.get + "_ptm.get.ptmEvidences.monoMass = " + _ptm.get.ptmEvidences(0).monoMass) }
      else { /*_ptm = Some(null) */ logger.error("Ptm don't exists in database : mono mass = " + ptms._1 + ", residue = " + ptms._2 + ", location = " + ptms._3); /*isPTMsDefinedInDB = false*/ }
    })
    
    if(fixedPtmDefs.length != fixedPtms.length || variablePtmDefs.length != variablePtms.length+3) {
      logger.error("XtandemPtmVerifier :: Given PTMs count don't match with PTMs found in database")
//      isPTMsDefinedInDB = false
    }
	}
}