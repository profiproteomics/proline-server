package fr.proline.module.exporter.mzidentml

import uk.ac.ebi.jmzidml.model.mzidml._

trait ParamMaker {
  
  // Init the CVs
  val psiCvID = "PSI-MS"
  val psiCV = new Cv()
  psiCV.setUri("http://psidev.cvs.sourceforge.net/viewvc/*checkout*/psidev/psi/psi-ms/mzML/controlledVocabulary/psi-ms.obo")
  psiCV.setId(psiCvID);
  psiCV.setVersion("2.25.0")
  psiCV.setFullName("PSI-MS")

  val unimodID = "UNIMOD"
  val unimodCV = new Cv() 
  unimodCV.setUri("http://www.unimod.org/obo/unimod.obo")
  unimodCV.setId(unimodID)
  unimodCV.setFullName("UNIMOD")
  
  val unitCvID = "UO"
  val unitCV = new Cv() 
  unitCV.setUri("http://obo.cvs.sourceforge.net/*checkout*/obo/obo/ontology/phenotype/unit.obo")
  unitCV.setId(unitCvID)
  unitCV.setFullName("UNIT-ONTOLOGY")
  
  def makeParamGroup( cvParam: CvParam ): Param = {
    val p = new Param()    
    p.setParam(cvParam)
    p
  }
  def makeParamGroup( userParam: UserParam ): Param = {
    val p = new Param()    
    p.setParam(userParam)
    p
  }
 
  def makeUserParamGroup( name: String ): Param = {
    this.makeParamGroup(this.makeUserParam( name ))
  }
  
  def makeUserParam( name: String ): UserParam = {
    val up = new UserParam()
    up.setName(name)
    up
  }
  
  def makeUserParam( name: String, value: String, valueType: String ): UserParam = {
    val up = new UserParam()
    up.setName(name)
    if( value != null ) up.setValue(value)
    if( valueType != null ) up.setType(valueType)
    up
  }
  
  def makeUserParam( name: String, cv: Cv, value: String, valueType: String,
                     unitAccession: String, unitName: String, alternateUnitCV: Cv = null ): UserParam = {
    
    val up = new UserParam()
    up.setName(name)
    up.setValue(value)
    up.setType(valueType)
    
    up.setUnitAccession(unitAccession)
    up.setUnitName(unitName)
    
    if( alternateUnitCV != null ) up.setUnitCv(alternateUnitCV) else up.setUnitCv(unitCV)
    
    up
  }
  
  /**
   * Helper method to create and return a CvParam from accession, name and CV
   *
   * @return CvParam
   */
  def makeCvParam( accession: String, name: String, cv: Cv, value: String = null ): CvParam = {
    val cvParam = new CvParam()
    cvParam.setAccession(accession)
    cvParam.setName(name)
    cvParam.setCv(cv)
    if( value != null ) cvParam.setValue(value)
    
    cvParam
  }

  /**
   * Helper method to create and return a CvParam from accession, name, CV, unitAccession, unitName and unitCV
   *
   * @return CvParam
   */
  def makeCvParamWithUnit( accession: String,  name: String, cv: Cv,
                           unitAccession: String, unitName: String, alternateUnitCV: Cv = null,
                           value: String = null ): CvParam = {
    
    val cvParam = new CvParam()
    cvParam.setAccession(accession)
    cvParam.setName(name)
    cvParam.setCv(cv)
    cvParam.setUnitAccession(unitAccession)
    cvParam.setUnitName(unitName)
    
    if( alternateUnitCV != null ) cvParam.setUnitCv(alternateUnitCV) else cvParam.setUnitCv(unitCV)
    if( value != null ) cvParam.setValue(value)
    
    cvParam
  }
  
}

case class CvParamUnit( accession: String, name:String )

object CvParam extends ParamMaker {
  
  def apply(accession: String, name: String, cv: Cv): CvParam = {
    this.makeCvParam(accession, name, cv)
  }
  
  def apply(accession: String, name: String, cv: Cv, value: String): CvParam = {
    this.makeCvParam(accession, name, cv, value)
  }
  
  def apply( accession: String,  name: String, cv: Cv,
             unitAccession: String, unitName: String, alternateUnitCV: Cv = null,
             value: String = null ): CvParam = {
    this.makeCvParamWithUnit(accession, name, cv, unitAccession, unitName, alternateUnitCV, value)
  }
  
}

object UserParam extends ParamMaker {
  def apply( name: String ): UserParam = {
    this.makeUserParam(name)
  }
  
  def apply( name: String, value: String, valueType: String ): UserParam = {
    this.makeUserParam(name, value, valueType)
  }
  
  def apply( name: String, cv: Cv, value: String, valueType: String,
             unitAccession: String, unitName: String, alternateUnitCV: Cv = null ): UserParam = {
    this.makeUserParam(name, cv, value, valueType, unitAccession, unitName, alternateUnitCV)
  }
}

object ParamList {
  def apply( params: List[AbstractParam] ): ParamList = {
    val paramList = new ParamList()
    val paramGroup = paramList.getParamGroup()
    params.foreach( paramGroup.add(_) )
    paramList
  }
}

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
  def apply( params: List[AbstractParam] ): ParamList = ParamList(params)
}


