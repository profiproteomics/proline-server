package fr.proline.module.exporter.mzidentml

import uk.ac.ebi.jmzidml.model.mzidml._
import fr.profi.cv.BuildParamList

object AmbiguousResidue {
  
  def apply( code: String, params: List[AbstractParam] = null ): AmbiguousResidue = {
    
    val ambiguousResidue = new AmbiguousResidue()
    ambiguousResidue.setCode(code)
    
    if( params != null ) {
      val paramList = ambiguousResidue.getParamGroup()
      params.foreach( paramList.add(_) )
    }
    
    ambiguousResidue
  }
  
}

object FileFormat {
  def apply( cvParam: CvParam ): FileFormat = {
    val fileFormat = new FileFormat()
    fileFormat.setCvParam( cvParam )
    fileFormat
  }
}

object FragmentationTable {  
  def apply( measures: List[Measure] ): FragmentationTable = {    
    val fragTable = new FragmentationTable()
    val measureList = fragTable.getMeasure()
    measures.foreach( measureList.add(_) )    
    fragTable
  }  
}

object ContactRole {
  def apply( contact: AbstractContact, role: Role ): ContactRole = {
    val contactRole = new ContactRole()
    contactRole.setContact(contact)
    contactRole.setRole(role)
    contactRole
  }
}

object Measure {
  
  def apply( id: String, cvParams: List[CvParam] ): Measure = {
    
    val measure = new Measure()
    measure.setId(id)
    
    val cvList = measure.getCvParam()
    cvParams.foreach( cvList.add(_) )
    
    measure
  }
  
}


object Residue {
  def apply( code: String, mass: Float ): Residue = {
    val residue = new Residue()
    residue.setCode(code)
    residue.setMass(mass)
    residue
  }
}

object Provider {
  def apply( id:String, contactRole: ContactRole, software: AnalysisSoftware = null ): Provider = {
    val provider = new Provider()
    provider.setId(id)
    provider.setContactRole(contactRole)
    if( software != null ) provider.setSoftware(software)
    provider
  }
}

object Role {
  def apply( cvParam: CvParam ): Role = {
    val role = new Role()
    role.setCvParam( cvParam )
    role
  }
}

object Threshold {
  def apply( params: List[AbstractParam] ): ParamList = BuildParamList(params)
}


