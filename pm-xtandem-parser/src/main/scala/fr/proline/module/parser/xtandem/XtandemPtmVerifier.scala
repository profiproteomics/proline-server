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
import org.xml.sax._
import org.xml.sax.helpers._
import javax.xml.parsers._
import java.io._
import scala.collection.mutable.ArrayBuffer
import com.typesafe.scalalogging.LazyLogging
import scala.collection.mutable.HashMap

object XtandemPtmVerifier extends LazyLogging {
  
  val ptmMonoMassMargin = 0.01

  def getFixedPtms(ptmProvider: IPTMProvider, proteinCtermPtms: String, proteinNtermPtms: String, residuePtms: String, refinePtms: String, refine: Boolean): Array[PtmDefinition] = {
    val ptms = new ArrayBuffer[PtmDefinition]
    ptms.appendAll(extractPtmDefinitions(ptmProvider, proteinCtermPtms, PtmLocation.PROT_C_TERM))
    ptms.appendAll(extractPtmDefinitions(ptmProvider, proteinNtermPtms, PtmLocation.PROT_N_TERM))
    ptms.appendAll(extractPtmDefinitions(ptmProvider, residuePtms, null))
    if(refine)
      ptms.appendAll(extractPtmDefinitions(ptmProvider, refinePtms, null))
    ptms.distinct.toArray
  }
  
  def getVariablePtms(ptmProvider: IPTMProvider, residuePtms: String, refinePtms: String, refineCtermPtms: String, refineNtermPtms: String, quickAcetyl: Boolean, quickPyrolidone: Boolean, refine: Boolean): Array[PtmDefinition] = {
    val ptms = new ArrayBuffer[PtmDefinition]
    if(quickAcetyl) {
      val ptm = ptmProvider.getPtmDefinition(42.01057, ptmMonoMassMargin, '\0', PtmLocation.PROT_N_TERM)
      if(ptm.isDefined) ptms.append(ptm.get) else logger.error("Acetylation on PROT_N_TERM not found")
    }
    if(quickPyrolidone) {
      val ptmQ = ptmProvider.getPtmDefinition(-17.02655, ptmMonoMassMargin, 'Q', PtmLocation.ANY_N_TERM)
      if(ptmQ.isDefined) ptms.append(ptmQ.get) else logger.error("Pyrolidone Q on ANY_N_TERM not found")
      val ptmC = ptmProvider.getPtmDefinition(-17.02655, ptmMonoMassMargin, 'C', PtmLocation.ANY_N_TERM)
      if(ptmC.isDefined) ptms.append(ptmC.get) else logger.error("Pyrolidone C on ANY_N_TERM not found")
      val ptmE = ptmProvider.getPtmDefinition(-18.01056, ptmMonoMassMargin, 'E', PtmLocation.ANY_N_TERM)
      if(ptmE.isDefined) ptms.append(ptmE.get) else logger.error("Pyrolidone E on ANY_N_TERM not found")
    }
    ptms.appendAll(extractPtmDefinitions(ptmProvider, residuePtms, null))
    if(refine) {
      ptms.appendAll(extractPtmDefinitions(ptmProvider, refinePtms, null))
      ptms.appendAll(extractPtmDefinitions(ptmProvider, refineCtermPtms, null))
      ptms.appendAll(extractPtmDefinitions(ptmProvider, refineNtermPtms, null))
    }
    ptms.distinct.toArray
  }
  
  def extractPtmDefinitions(ptmProvider: IPTMProvider, ptmsAsString: String, location: PtmLocation.Value): Array[PtmDefinition] = {
    val ptms = new ArrayBuffer[PtmDefinition]
    // default value is "0.0", do not consider it
    if(!ptmsAsString.isEmpty && !ptmsAsString.equals("0.0")) {
      ptmsAsString.split(",").foreach(ptmAsString => {
        var ptm: Option[PtmDefinition] = None
        if(ptmAsString.contains("@")) {
          val Array(mass, residue) = ptmAsString.split("@")
          residue match {
            case "[" => ptm = ptmProvider.getPtmDefinition(mass.toDouble, ptmMonoMassMargin, '\0', PtmLocation.ANY_N_TERM)
            case "]" => ptm = ptmProvider.getPtmDefinition(mass.toDouble, ptmMonoMassMargin, '\0', PtmLocation.ANY_C_TERM)
            case _   => ptm = ptmProvider.getPtmDefinition(mass.toDouble, ptmMonoMassMargin, residue.charAt(0), PtmLocation.ANYWHERE)
          }
        } else if(location != null) {
          ptm = ptmProvider.getPtmDefinition(ptmAsString.toDouble, ptmMonoMassMargin, '\0', location)
        }
        if(ptm.isDefined) ptms.append(ptm.get)
      })
    }
    ptms.toArray
  }
  
}

class XtandemPtmVerifier( val parserContext: ProviderDecoratedExecutionContext
                          ) extends DefaultHandler with LazyLogging {

	// flags indicate in which markups we are
  private var inBioml = false; private var inNote = false; private var inGroupParameters = false;
	
  // Mark up
	private var note : String = ""
	
	// buffer allow to collect "info" datas
	private var buffer : StringBuffer = new StringBuffer()
	private var canReadBuffer : Boolean = false
	private var parametersLabel : String = ""
	
	private val ptmProvider: IPTMProvider = parserContext.getProvider(classOf[IPTMProvider])
	private var refineParamIsYes: Boolean = false    // For considering or not input parameters with "refine" label  

  private var quickAcetyl = true
  private var quickPyrolidone = true
  private val fixedPtms = new ArrayBuffer[PtmDefinition]
	private val variablePtms = new ArrayBuffer[PtmDefinition]
	
	def fixedPtmDefs = fixedPtms
	def variablePtmDefs = variablePtms
	
	private val settings = new HashMap[String, String]
	
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
			  if(attributes.getValue("label").contains("residue, modification mass") 
			      || attributes.getValue("label").equals("residue, potential modification mass")
            || attributes.getValue("label").equals("protein, C-terminal residue modification mass")
            || attributes.getValue("label").equals("protein, N-terminal residue modification mass")
            || attributes.getValue("label").equals("refine, modification mass")
            || attributes.getValue("label").equals("refine, potential modification mass")
            || attributes.getValue("label").equals("refine, potential C-terminus modifications")
            || attributes.getValue("label").equals("refine, potential N-terminus modifications")
            // settings
            || attributes.getValue("label").equals("refine")
            || attributes.getValue("label").equals("protein, quick acetyl")
            || attributes.getValue("label").equals("protein, quick pyrolidone")){
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
		    if (parametersLabel.startsWith("residue, modification mass")) {
		      settings.put(parametersLabel, note)
        } else if (parametersLabel.equals("residue, potential modification mass")) {
          settings.put(parametersLabel, note)
        } else if (parametersLabel.equals("protein, C-terminal residue modification mass") && augmentString(note).toDouble != 0.0) { // 0.0 is the default value for this paramater in XTandem
          settings.put(parametersLabel, note)
        } else if (parametersLabel.equals("protein, N-terminal residue modification mass") && augmentString(note).toDouble != 0.0) {
          settings.put(parametersLabel, note)
        } else if (parametersLabel.equals("refine") && note.equals("yes")) { // Refine modifications
          refineParamIsYes = true
        } else if (parametersLabel.equals("protein, quick acetyl")) {
          quickAcetyl = true
        } else if (parametersLabel.equals("protein, quick pyrolidone")) {
          quickPyrolidone = true
        } else if (parametersLabel.equals("refine, modification mass") && !note.isEmpty) { // variable ptms
          settings.put(parametersLabel, note)
        } else if (parametersLabel.equals("refine, potential modification mass") && !note.isEmpty) { // variable ptms
          settings.put(parametersLabel, note)
        } else if (parametersLabel.equals("refine, potential C-terminus modifications") && !note.isEmpty) { // variable ptms
          settings.put(parametersLabel, note)
        } else if (parametersLabel.equals("refine, potential N-terminus modifications") && !note.isEmpty) { // variable ptms
          settings.put(parametersLabel, note)
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
	}

	//End of file parsing
	override def endDocument() : Unit = {
		fixedPtms ++= XtandemPtmVerifier.getFixedPtms(
        ptmProvider, 
        settings.getOrElse("protein, C-terminal residue modification mass", "").toString, 
        settings.getOrElse("protein, N-terminal residue modification mass", "").toString, 
        settings.getOrElse("residue, modification mass", "").toString, 
        settings.getOrElse("refine, modification mass", "").toString, 
        refineParamIsYes)
    variablePtms ++= XtandemPtmVerifier.getVariablePtms(
        ptmProvider, 
        settings.getOrElse("residue, potential modification mass", "").toString, 
        settings.getOrElse("refine, potential modification mass", "").toString, 
        settings.getOrElse("refine, potential C-terminus modifications", "").toString, 
        settings.getOrElse("refine, potential N-terminus modifications", "").toString, 
        settings.getOrElse("protein, quick acetyl", "yes").toString.equals("yes"), 
        settings.getOrElse("protein, quick pyrolidone", "yes").toString.equals("yes"), 
        refineParamIsYes)
	}
}