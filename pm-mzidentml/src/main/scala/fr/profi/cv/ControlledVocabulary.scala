package fr.profi.cv

import fr.profi.obo._
import uk.ac.ebi.jmzidml.model.mzidml.Cv

object ControlledVocabulary {
  
  // Init the CVs
  val patoCV = _ontologyToCv( PatoOntology )
  val psiMsCV = _ontologyToCv( PsiMsOntology )
  val unimodCV = _ontologyToCv( UnimodOntology )
  val unitCV = _ontologyToCv( UnitOntology )
  
  private def _ontologyToCv( ontology: Ontology ): Cv = {
    
    val cv = new Cv()  
    cv.setUri(ontology.sourceUrl)
    cv.setVersion(ontology.dataVersion)
    cv.setId(ontology.id)
    cv.setFullName(ontology.name)
    
    cv
  }
  
}