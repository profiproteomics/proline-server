package fr.profi.cv

import scala.collection.mutable.HashMap

import fr.profi.obo._

import uk.ac.ebi.jmzidml.model.mzidml.Cv
import uk.ac.ebi.jmzidml.model.mzidml.CvParam

object PsiCvParam extends ParamMaker {
  
  private val emptyCvParamCache = new HashMap[PsiMs.Value,CvParam]
  
  /*private def _getOrMakeEmptyCvParam(psiMsTermId: PsiMs.Value) {
    val oboTerm = PsiMsOntology.oboTermByIdString(psiMsTermId.toString)
    val emptyCvParam = this.apply(oboTerm.id, oboTerm.name)
  }*/
  
  def apply(psiMsTermId: PsiMs.Value): CvParam = {
    
    if( emptyCvParamCache.contains(psiMsTermId) ) {
      return emptyCvParamCache(psiMsTermId)
    }
    
    val oboTerm = PsiMsOntology.oboTermByIdString(psiMsTermId.toString)
    val emptyCvParam = this.apply(oboTerm.id, oboTerm.name)
    
    emptyCvParamCache.synchronized {
      emptyCvParamCache += psiMsTermId -> emptyCvParam
    }
    
    emptyCvParam
  }
  
  def apply(psiMsTermId: PsiMs.Value, value: String): CvParam = {
    val oboTerm = PsiMsOntology.oboTermByIdString(psiMsTermId.toString)
    this.apply(oboTerm.id, oboTerm.name, value )
  }
  
  def apply(
    psiMsTermId: PsiMs.Value,
    unitTermId: UnitTerm.Value,
    value: String = null
  ): CvParam = {
    
    val psiMsOboTerm = PsiMsOntology.oboTermByIdString(psiMsTermId.toString)
    val unitOboTerm = UnitOntology.oboTermByIdString(unitTermId.toString)
    
    this.makeCvParamWithUnit(
      psiMsOboTerm.id,
      psiMsOboTerm.name,
      ControlledVocabulary.psiMsCV,
      unitOboTerm.id,
      unitOboTerm.name,
      value = value
    )
  }
  
  def withAlternativeUnit(
    psiMsTermId: PsiMs.Value,
    unitPsiMsTermId: PsiMs.Value,
    value: String = null
  ): CvParam = {
    
    val psiMsOboTerm = PsiMsOntology.oboTermByIdString(psiMsTermId.toString)
    val unitPsiMsOboTerm = PsiMsOntology.oboTermByIdString(unitPsiMsTermId.toString)
    
    this.makeCvParamWithUnit(
      psiMsOboTerm.id,
      psiMsOboTerm.name,
      ControlledVocabulary.psiMsCV,
      unitPsiMsOboTerm.id,
      unitPsiMsOboTerm.name,
      ControlledVocabulary.psiMsCV,
      value
    )
  }
  
  def apply(accession: String, name: String): CvParam = {
    this.makeCvParam(accession, name, ControlledVocabulary.psiMsCV)
  }
  
  def apply(accession: String, name: String, value: String): CvParam = {
    this.makeCvParam(accession, name, ControlledVocabulary.psiMsCV, value)
  }
  
  /*
  def apply(
    accession: String,
    name: String,    
    unitAccession: String,
    unitName: String,
    alternateUnitCV: Cv,
    value: String
  ): CvParam = {
    this.makeCvParamWithUnit(
      accession, name, ControlledVocabulary.psiMsCV, unitAccession, unitName, alternateUnitCV, value
    )
  }*/

}