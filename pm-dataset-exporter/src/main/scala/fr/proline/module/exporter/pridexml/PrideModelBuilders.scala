package fr.proline.module.exporter.pridexml

import uk.ac.ebi.pride.jaxb.model._
import uk.ac.ebi.pride.jaxb.xml.unmarshaller.PrideXmlUnmarshallerFactory


object FragmentMatchMapper {

  
  
  	def ionType2formulaeMap : Map[String,(String,String)] = {
  	  val mapper = Map.newBuilder[String,(String,String)]
  	  mapper += ("a" -> (PrideSchemaConstants.PRIDE_CV_A_ION_ACC, PrideSchemaConstants.PRIDE_CV_A_ION_NAME))
  	  mapper += ("a*" -> (PrideSchemaConstants.PRIDE_CV_ANH3_ION_ACC, PrideSchemaConstants.PRIDE_CV_ANH3_ION_NAME))
  	  mapper += ("a0" -> (PrideSchemaConstants.PRIDE_CV_AH20_ION_ACC, PrideSchemaConstants.PRIDE_CV_AH20_ION_NAME))
  	  mapper += ("b" -> (PrideSchemaConstants.PRIDE_CV_B_ION_ACC, PrideSchemaConstants.PRIDE_CV_B_ION_NAME))
  	  mapper += ("b*" -> (PrideSchemaConstants.PRIDE_CV_BNH3_ION_ACC, PrideSchemaConstants.PRIDE_CV_BNH3_ION_NAME))
  	  mapper += ("b0" -> (PrideSchemaConstants.PRIDE_CV_BH2O_ION_ACC, PrideSchemaConstants.PRIDE_CV_BH2O_ION_NAME))
  	  mapper += ("c" -> (PrideSchemaConstants.PRIDE_CV_C_ION_ACC, PrideSchemaConstants.PRIDE_CV_C_ION_NAME))
  	  mapper += ("c*" -> (PrideSchemaConstants.PRIDE_CV_CNH3_ION_ACC, PrideSchemaConstants.PRIDE_CV_CNH3_ION_NAME))
  	  mapper += ("c0" -> (PrideSchemaConstants.PRIDE_CV_CH20_ION_ACC, PrideSchemaConstants.PRIDE_CV_CH20_ION_NAME))
  	  mapper += ("x" -> (PrideSchemaConstants.PRIDE_CV_X_ION_ACC, PrideSchemaConstants.PRIDE_CV_X_ION_NAME))
  	  mapper += ("x*" -> (PrideSchemaConstants.PRIDE_CV_XNH3_ION_ACC, PrideSchemaConstants.PRIDE_CV_XNH3_ION_NAME))
  	  mapper += ("x0" -> (PrideSchemaConstants.PRIDE_CV_XH20_ION_ACC, PrideSchemaConstants.PRIDE_CV_XH20_ION_NAME))
  	  mapper += ("y" -> (PrideSchemaConstants.PRIDE_CV_Y_ION_ACC, PrideSchemaConstants.PRIDE_CV_Y_ION_NAME))
  	  mapper += ("y*" -> (PrideSchemaConstants.PRIDE_CV_YNH3_ION_ACC, PrideSchemaConstants.PRIDE_CV_YNH3_ION_NAME))
  	  mapper += ("y0" -> (PrideSchemaConstants.PRIDE_CV_YH2O_ION_ACC, PrideSchemaConstants.PRIDE_CV_YH2O_ION_NAME))
  	  mapper += ("z" -> (PrideSchemaConstants.PRIDE_CV_Z_ION_ACC, PrideSchemaConstants.PRIDE_CV_Z_ION_NAME))
  	  mapper += ("z*" -> (PrideSchemaConstants.PRIDE_CV_ZNH3_ION_ACC, PrideSchemaConstants.PRIDE_CV_ZNH3_ION_NAME))
  	  mapper += ("z0" -> (PrideSchemaConstants.PRIDE_CV_ZH20_ION_ACC, PrideSchemaConstants.PRIDE_CV_ZH20_ION_NAME))
  	  mapper.result
  	} 

  	def getPrideCVforIonSerie(serie : String) : (String, String)= {
  	  
      val ion = serie.charAt(0)
      val ionType = if(serie.length() > 1){ serie.charAt(1)} else ""
    	  
  	 //TODO VDS : get more precicely ion type
      val (cvIonAcc, cvIonName) = ion match {
        case 'a' | 'b' | 'c' | 'x' | 'y' | 'z' => { 
        	ionType match {
          		case '0' | '*' => ionType2formulaeMap(serie)
          		case _ => ionType2formulaeMap(ion.toString)
			}              
        }
        case _   => (PrideSchemaConstants.PRIDE_CV_ION_TYPE_ACC, PrideSchemaConstants.PRIDE_CV_ION_TYPE_NAME)
      }
      (cvIonAcc, cvIonName)
  	}
}

object CvParam {
  
  def apply(accession: String, name: String, cvLabel: String, value: String): uk.ac.ebi.pride.jaxb.model.CvParam = {
    val cvp = new CvParam()
    cvp.setName(name)
    cvp.setAccession(accession)
    cvp.setCvLabel(cvLabel)
    if( value != null ) cvp.setValue(value)
    cvp
  }
  
   def apply(accession: String, name: String, value: String): uk.ac.ebi.pride.jaxb.model.CvParam = {
    val cvp = new CvParam()
    cvp.setName(name)
    cvp.setAccession(accession)
    cvp.setCvLabel(PrideSchemaConstants.PRIDE_CV_NAME)
    if( value != null ) cvp.setValue(value)
    cvp
  }
   
   def apply(xmlString: String): uk.ac.ebi.pride.jaxb.model.CvParam = {     
	  val pof = PrideXmlUnmarshallerFactory.getInstance()
    val unmarshaller = pof.initializeUnmarshaller()
    val cvp =  unmarshaller.unmarshal(xmlString, classOf[CvParam])
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
  
  /** Obsolete Ontology, Use MS instead ! **/
  val psiLookup : CvLookup= { 
    val lookup = new CvLookup()
    lookup.setCvLabel(PrideSchemaConstants.PSI_CV_NAME)
    lookup.setFullName(PrideSchemaConstants.PSI_CV_FULLNAME)
    lookup.setVersion(PrideSchemaConstants.PSI_CV_VERSION)
    lookup.setAddress(PrideSchemaConstants.PSI_CV_ADDRESS)
    lookup
  }
  
  val msLookup : CvLookup = { 
    val lookup = new CvLookup()
    lookup.setCvLabel(PrideSchemaConstants.MS_CV_NAME)
    lookup.setFullName(PrideSchemaConstants.MS_CV_FULLNAME)
    lookup.setVersion(PrideSchemaConstants.MS_CV_VERSION)
    lookup.setAddress(PrideSchemaConstants.MS_CV_ADDRESS)
    lookup 
  }
  
  val prideLookup: CvLookup = { 
    val lookup = new CvLookup()
    lookup.setCvLabel(PrideSchemaConstants.PRIDE_CV_NAME)
    lookup.setFullName(PrideSchemaConstants.PRIDE_CV_FULLNAME)
    lookup.setVersion(PrideSchemaConstants.PRIDE_CV_VERSION)
    lookup.setAddress(PrideSchemaConstants.PRIDE_CV_ADDRESS)
    lookup
  }
}
