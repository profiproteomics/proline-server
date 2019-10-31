package fr.profi

import fr.profi.obo.PsiMs
import fr.profi.obo.PsiMsTermId
import uk.ac.ebi.jmzidml.model.mzidml.CvParam

package object cv {
  
  implicit def psiMsTermName2psiMsTermId( psiMsTermName: PsiMs.Value ): PsiMsTermId.Value = {
    PsiMsTermId.withName(psiMsTermName.toString)
  }

  //<cvParam accession="MS:1002404" cvRef="PSI-MS" value="4" name="count of identified proteins"/>
  def createCVParamTag(cvParam: CvParam): String = {
    val sb = new StringBuffer
    // tag opening plus accession attribute
    sb.append("<cvParam accession=\"").append(cvParam.getAccession).append("\"")
    // the 'cvRef' attribute, if provided
    sb.append(" cvRef=\"").append(cvParam.getCvRef).append("\"")
    // the 'name' attribute, if provided
    val name = cvParam.getName
    if (name != null) sb.append(" name=\"").append(name).append("\"")
    // the 'value' attribute, if provided
    val value = cvParam.getValue
    if (value != null) sb.append(" value=\"").append(value).append("\"")
    // finally close the tag
    sb.append(" ></cvParam>")
    sb.toString
  }
}