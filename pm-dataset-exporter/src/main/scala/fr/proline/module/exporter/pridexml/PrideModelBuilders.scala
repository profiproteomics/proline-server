package fr.proline.module.exporter.pridexml

import uk.ac.ebi.pride.jaxb.model._

object CvParam {
  
  def apply(accession: String, name: String, cvLabel: String, value: String): CvParam = {
    val cvp = new CvParam()
    cvp.setName(name)
    cvp.setAccession(accession)
    cvp.setCvLabel(cvLabel)
    if( value != null ) cvp.setValue(value)
    cvp
  }
  
   def apply(accession: String, name: String, value: String): CvParam = {
    val cvp = new CvParam()
    cvp.setName(name)
    cvp.setAccession(accession)
    cvp.setCvLabel("PRIDE")
    if( value != null ) cvp.setValue(value)
    cvp
  }
  
}

object UserParam  {

  def apply( name: String, value: String): UserParam = {
    val up = new UserParam()
    up.setName(name)
    up.setValue(value)
    up
  }
}

object CvLookup {
  
  val psiLookup = { 
    val lookup = new CvLookup()
    lookup.setCvLabel("PSI")
    lookup.setFullName("The PSI Ontology")
    lookup.setVersion("1.0.0")
    lookup.setAddress("http://psidev.sourceforge.net/ontology/")
  }
  
}
