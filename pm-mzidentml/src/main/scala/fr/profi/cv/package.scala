package fr.profi

import fr.profi.obo.PsiMsOntology
import fr.profi.obo.PsiMsTermId
import fr.profi.obo.PsiMs

package object cv {
  
  implicit def psiMsTermName2psiMsTermId( psiMsTermName: PsiMs.Value ): PsiMsTermId.Value = {
    PsiMsTermId.withName(psiMsTermName.toString)
  }
  
}