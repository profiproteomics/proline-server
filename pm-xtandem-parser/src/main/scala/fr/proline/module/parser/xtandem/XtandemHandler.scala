/**
 * XTandemHandler.scala 
 * @author	Ibrahim YAPICI
 * @email		iyapici@unistra.fr
 * @description XTandemHandler is called to manage markups and store datas in XTandemClasses.
 * XTandemHandler use Sax library to parse Xtandem's .xml file
 */

//Proline
package fr.proline.module.parser.xtandem

//Parser
import org.xml.sax._
import org.xml.sax.helpers._
import javax.xml.parsers._

import java.io._
import scala.collection.mutable.ArrayBuffer
import com.typesafe.scalalogging.slf4j.Logging

class XtandemHandler extends DefaultHandler with Logging {
	// Management of the parser
	var displayTree = false	// Display tree structure of xml file in console
	// markup
	var bioml : XTBioml = new XTBioml()
	var groupModel : XTGroupModel = new XTGroupModel()
	var groupParameters : XTGroupParameters = new XTGroupParameters()
	var protein : XTProtein = new XTProtein()
	var note : XTNote = new XTNote()
	var fileMarkup : XTFileMarkup = new XTFileMarkup()
	var peptide : XTPeptide = new XTPeptide()
	var domain : XTDomain = new XTDomain()
	var aaMarkup : XTAAMarkup = new XTAAMarkup()
	var groupSupport : XTGroupSupport = new XTGroupSupport()
	var gamlTrace : XTGAMLTrace = new XTGAMLTrace()
	var gamlAttribute : XTGAMLAttribute = new XTGAMLAttribute()
	var gamlXdata : XTGAMLXdata = new XTGAMLXdata()
	var gamlYdata : XTGAMLYdata = new XTGAMLYdata()
	var gamlValues : XTGAMLValues = new XTGAMLValues()
	// flags indicate in which markups we are
  var inBioml = false; var inGroupModel = false; var inProtein = false; var inNote = false; var inFileMarkup = false;
	var inPeptide = false; var inDomain = false; var inAAMarkup = false; var inGroupSupport = false; var inGroupParameters = false;
	var inGAMLTrace = false; var inGAMLAttribute = false; var inGAMLXdata = false; var inGAMLYdata = false; var inGAMLValues = false ;
	var inGroupSupportFragmentIonMassSpectrum = false;
	// buffer allow to collect "info" datas
	var buffer : StringBuffer = new StringBuffer()
	var proteinList : ArrayBuffer[XTProtein] = new ArrayBuffer[XTProtein] 
	
	// detection of opening of markup
	override def startElement(uri : String , localName : String ,
			qName : String, attributes : Attributes) : Unit = {
		if(qName.equals("bioml")) {
			if(displayTree) println("bioml")
				inBioml = true
				
		} else if(qName.equals("group") && inBioml) {
			val typeMU = attributes.getValue("type")
			if(typeMU.equals("model")) {
				if(displayTree) println(" - groupModel")				
				groupModel.id = augmentString(attributes.getValue("id")).toInt
				groupModel.rt = try{ Some(augmentString(attributes.getValue("rt")).toDouble)} catch { case _ => None}
				groupModel.mh = augmentString(attributes.getValue("mh")).toDouble
				groupModel.z = augmentString(attributes.getValue("z")).toInt
				inGroupModel = true

			} else if(typeMU.equals("support") && inGroupModel) {
				if(displayTree) println(" -  - groupSupport")
				inGroupSupport = true
				if(attributes.getValue("label").equals("fragment ion mass spectrum")) inGroupSupportFragmentIonMassSpectrum = true
				
			} else if(typeMU.equals("parameters")){
				if(displayTree) println(" - groupParameters")
				inGroupParameters = true
			}

		} else if(qName.equals("protein") && inGroupModel) {
			if(displayTree) println(" -  - protein")
			protein.label = attributes.getValue("label")
			protein.sumI = augmentString(attributes.getValue("sumI")).toDouble
			inProtein = true
			
		} else if(qName.equals("note") && (inProtein  || inGroupSupportFragmentIonMassSpectrum || inGroupParameters )) {
		  
			if(inGroupSupportFragmentIonMassSpectrum & displayTree) println(" -  -  -  - note")
			else if(inProtein & displayTree) println(" -  -  - note") 
			else if(inGroupParameters & displayTree) println(" -  - note")
			note.label = attributes.getValue("label")
			inNote = true
			
		} else if(qName.equals("file") && inProtein) {
			if(displayTree) println(" -  -  - file")
			fileMarkup.URL = attributes.getValue("URL")
			inFileMarkup = true
			
		} else if(qName.equals("peptide") && inProtein) {
			if(displayTree) println(" -  -  - peptide")
			peptide.end = augmentString(attributes.getValue("end")).toInt
			inPeptide = true
			
		} else if(qName.equals("domain") && inPeptide) {
			if(displayTree) println(" -  -  -  - domain")
			domain.id = attributes.getValue("id")
			domain.start = augmentString(attributes.getValue("start")).toInt
			domain.end = augmentString(attributes.getValue("end")).toInt
			domain.mh = augmentString(attributes.getValue("mh")).toDouble
			domain.delta = augmentString(attributes.getValue("delta")).toDouble
			domain.hyperScore = augmentString(attributes.getValue("hyperscore")).toDouble
			domain.pre = attributes.getValue("pre")
			domain.post = attributes.getValue("post")
			domain.seq = attributes.getValue("seq")
			inDomain = true
			
		} else if(qName.equals("aa") && inDomain) {
			if(displayTree) println(" -  -  -  -  - aa")
			aaMarkup.typeMU = attributes.getValue("type").charAt(0)
			aaMarkup.at = augmentString(attributes.getValue("at")).toInt
			aaMarkup.modified = augmentString(attributes.getValue("modified")).toDouble
			inAAMarkup = true
			
		} else if(qName.equals("GAML:trace") && inGroupSupport) {
			if(displayTree) println(" -  -  -  - GAML:trace")
			inGAMLTrace = true
			
		} else if(qName.equals("GAML:attribute") && inGAMLTrace) {
			if(displayTree) println(" -  -  -  -  - GAML:attribute")
			inGAMLAttribute = true
			
		} else if(qName.equals("GAML:Xdata") && inGAMLTrace) {
			if(displayTree) println(" -  -  -  -  -  - GAML:Xdata")
			gamlXdata.units = attributes.getValue("units")
			inGAMLXdata = true
			
		} else if(qName.equals("GAML:Ydata") && inGAMLTrace) {
			if(displayTree) println(" -  -  -  -  -  - GAML:Ydata")
			inGAMLYdata = true

		} else if(qName.equals("GAML:values") && (inGAMLXdata || inGAMLYdata)) {
			if(displayTree) println(" -  -  -  -  -  -  - GAML:values")
				inGAMLValues = true
				
		}
	}
	
	//detection of end of markup
	override def endElement(uri : String, localName : String, qName : String) /*throws SAXException*/: Unit = {
		if(qName.equals("bioml")) {
			inBioml = false
			
		} else if(qName.equals("group") && inBioml) {
			if(inGroupParameters) {
				bioml.groupParametersList.append(groupParameters)
				groupParameters = new XTGroupParameters
				inGroupParameters = false 
				
			} else if(inGroupSupport) {
			  if(inGroupSupportFragmentIonMassSpectrum) {
			    // Add protein to 
	  	    groupSupport.proteinList = groupSupport.proteinList ++ proteinList 
    			proteinList = new ArrayBuffer[XTProtein]
			    groupModel.groupSupportList.append(groupSupport)
			    inGroupSupportFragmentIonMassSpectrum = false
			  }
//				groupModel.groupSupportList.append(groupSupport)
				groupSupport = new XTGroupSupport
				inGroupSupport = false
				
			} else if(inGroupModel) {
				bioml.groupModelList.append(groupModel)
				groupModel = new XTGroupModel
				inGroupModel = false
				
			}
		} else if(qName.equals("protein") && inGroupModel) {
		  proteinList.append(protein)
			protein = new XTProtein
			inProtein = false
			
		} else if(qName.equals("note") && (inProtein || inGroupSupport || inGroupParameters)) {
			note.info = buffer.toString().replace("\n", "").trim
			if(inGroupSupport) groupSupport.note = note
			else if(inProtein) protein.note = note
			else if(inGroupParameters) groupParameters.noteList.append(note)
			buffer.delete(0, buffer.length())
			note = new XTNote
			inNote = false
			
		} else if(qName.equals("file") && inProtein) {
			protein.fileMarkup = fileMarkup
			fileMarkup = new XTFileMarkup
			inFileMarkup = false
			
		} else if(qName.equals("peptide") && inProtein) {
			peptide.info = buffer.toString().replace("\n", " ").trim
			protein.peptide = peptide
			buffer.delete(0, buffer.length())
			peptide = new XTPeptide
			inPeptide = false
			
		} else if(qName.equals("domain") && inPeptide) {
			peptide.domainList.append(domain)
			domain = new XTDomain
			inDomain = false
			
		} else if(qName.equals("aa") && inDomain) {
			domain.aaMarkupList.append(aaMarkup)
			aaMarkup = new XTAAMarkup
			inAAMarkup = false
			
		} else if(qName.equals("GAML:trace") && inGroupSupport) {
			groupSupport.gamlTraceList.append(gamlTrace)
			gamlTrace = new XTGAMLTrace
			inGAMLTrace = false
			
		} else if(qName.equals("GAML:attribute") && inGAMLTrace) {
			gamlAttribute.info = buffer.toString().replace("\n", " ").trim
			gamlTrace.gamlAttributeList.append(gamlAttribute)
			buffer.delete(0, buffer.length())
			gamlAttribute = new XTGAMLAttribute
			inGAMLAttribute = false
			
		} else if(qName.equals("GAML:Xdata") && inGAMLTrace) {
			gamlTrace.gamlXdata = gamlXdata
			gamlXdata = new XTGAMLXdata
			inGAMLXdata = false
			
		} else if(qName.equals("GAML:Ydata") && inGAMLTrace) {
			gamlTrace.gamlYdata = gamlYdata
			gamlYdata = new XTGAMLYdata
			inGAMLYdata = false
			
		} else if(qName.equals("GAML:values") && (inGAMLXdata || inGAMLYdata)) {
			gamlValues.info = buffer.toString().replace("\n", " ").trim
			if(inGAMLXdata) gamlXdata.gamlValues = gamlValues
			else if(inGAMLYdata) gamlYdata.gamlValues = gamlValues
      buffer.delete(0, buffer.length())
			gamlValues = new XTGAMLValues
			inGAMLValues = false
			
		}
	}
	
	//Characters detection
	override def characters(ch : Array[Char],start : Int, length : Int) : Unit = {
		var lecture = new String(ch,start,length)
		if(buffer != null) buffer.append(lecture)
	}
	
	//Start of file parsing
	override def startDocument() : Unit = {
		logger.info("Start of Handler for XTandemHandler")
	}
	
	//End of file parsing
	override def endDocument() : Unit = {
		logger.info("End of Handler for XTandemHandler")
	}
}