/**
 * XTandemClasses.scala
 * @author	Ibrahim YAPICI
 * @email		iyapici@unistra.fr
 * @description Classes to be defined to store XTandem parsed datas
 * Note that the markup hierarchy structure are not respected regarding protein markup :
 * - in xml, protein is in group type='model' markup, in XtandemClasses protein is in group type='support' class
 */

package fr.proline.module.parser.xtandem

import scala.collection.mutable.ArrayBuffer

case class XTFragmentMatch(serie: String, nbMatches: Int, score: Double)

class XTAAMarkup {
	var typeMU : Char = _
	var at : Int  = _
	var modified : Double = _
}

class XTDomain {
	var id : String = _
	var start : Int = _
	var end : Int = _
	var mh : Double = _
	var delta : Double = _
	var hyperScore : Double = _
	var pre : String = _
	var post : String = _
	var seq : String = _
	var expectValue : Double = _
	var nextScore : Double = _
	var aaMarkupList : ArrayBuffer[XTAAMarkup] = new ArrayBuffer[XTAAMarkup]()
	var fragmentMatches = new ArrayBuffer[XTFragmentMatch]
}

class XTPeptide {
  var end : Int = _
	var info : String = _
	var domainList : ArrayBuffer[XTDomain] = new ArrayBuffer[XTDomain]()
}

class XTFileMarkup {
	var URL : String = _
}

class XTNote {
	var label : String = _
	var info : String  = _
}

class XTProtein {
	var label : String = _
	var sumI : Double = _
	var note: XTNote = _
	var fileMarkup : XTFileMarkup = _
	var peptide: XTPeptide = _
}

class XTGAMLValues {
	var info : String = _
}

class XTGAMLXdata {
	var units : String = _
	var gamlValues : XTGAMLValues = _
}

class XTGAMLYdata {
	var gamlValues : XTGAMLValues = _
}

class XTGAMLAttribute {
	var info : String = _
}

class XTGAMLTrace {
	var gamlAttributeList : ArrayBuffer[XTGAMLAttribute] = new ArrayBuffer[XTGAMLAttribute]()
	var gamlXdata : XTGAMLXdata = _
	var gamlYdata : XTGAMLYdata = _
}

class XTGroupSupport {
	var typeMU : String = "support"
	var gamlTraceList : ArrayBuffer[XTGAMLTrace] = new ArrayBuffer[XTGAMLTrace]()
	var note : XTNote = _
	var proteinList : ArrayBuffer[XTProtein] = new ArrayBuffer[XTProtein]()
}

class XTGroupModel {
	var id : Int = _
//	var typeMU : String ="model"
//	var rt : Option[Double] = _  // retention time
	var rt : Double = _  // retention time
	var mh : Double = _
	var z : Int = _
	var groupSupportList : ArrayBuffer[XTGroupSupport] = new ArrayBuffer[XTGroupSupport]()
}

class XTGroupParameters {
//	var typeMU : String = "parameters"
	var noteList : ArrayBuffer[XTNote] = new ArrayBuffer[XTNote]()
}

class XTBioml {
	var groupModelList : ArrayBuffer[XTGroupModel] = new ArrayBuffer[XTGroupModel]()
	var groupParametersList : ArrayBuffer[XTGroupParameters] = new ArrayBuffer[XTGroupParameters]()
}