package fr.profi.cv

import uk.ac.ebi.jmzidml.model.mzidml._
import fr.profi.obo.OboTerm

trait ParamMaker {
  
  import ControlledVocabulary._
  
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

object BuildCvParam extends ParamMaker {
  
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

object BuildUserParam extends ParamMaker {
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

object BuildParamList {
  def apply( params: List[AbstractParam] ): ParamList = {
    val paramList = new ParamList()
    val paramGroup = paramList.getParamGroup()
    params.foreach( paramGroup.add(_) )
    paramList
  }
}