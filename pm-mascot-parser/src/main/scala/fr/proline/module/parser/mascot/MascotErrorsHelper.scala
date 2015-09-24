package fr.proline.module.parser.mascot

import com.typesafe.scalalogging.LazyLogging 

import matrix_science.msparser.ms_errors
import matrix_science.msparser.ms_errs
import matrix_science.msparser.ms_modvector
import matrix_science.msparser.msparserConstants

case class MascotException(errorString:String, errorNumber: Int = -1 ) extends Exception(errorString)

object MascotErrorsHelper extends LazyLogging  {
  
  def checkMascotErrors( mascotErrorContainer: ms_errors, msg: String ) {
    if ( !mascotErrorContainer.isValid ) {
      this.logger.error(" ----- "+msg)
      this.logErrors(mascotErrorContainer.getErrorHandler)
    }
  }
  
  def logErrors(mascotErrors: ms_errs) {
    
    for ( k <- 1 to mascotErrors.getNumberOfErrors ) {
      val( errorString, errorNumber ) = (mascotErrors.getErrorString(k),mascotErrors.getErrorNumber(k))
      
      val severity = mascotErrors.getErrorSeverity(k)
      
      if( severity > 0 ) throw new MascotException(errorString,errorNumber)
      else {
        this.logger.warn("ms_parser warning (CODE=%d): %s".format(errorNumber,errorString) )
      }
      
      /*val severity = if( mascotErrors.getErrorSeverity(k) <= 0 ) "[W], " else "[F], "
        
      this.logger.error("  - ms_error CODE=" + mascotErrors.getErrorNumber(k) +
                        ", SEVERITY=" + severity + mascotErrors.getErrorString(k) )*/
    }
    
    mascotErrors.clearAllErrors()
  }
  
 /*def logFragment(frag: ms_fragment) {
    val strBuilder = new StringBuilder("Fragment : ")
                           .append(frag.getLabel())
                           .append(" :: ")
                           .append(frag.getMass())
                           .append(" NL = "+ frag.getNeutralLoss())
    
    this.logger.debug(strBuilder.toString())
  }*/
  
  def logMods( mods: ms_modvector ) {
    
    for ( k <- 0 until mods.getNumberOfModifications ) {
      
      val mod = mods.getModificationByNumber(k)
      val modType = new StringBuffer()
      
      mod.getModificationType match {
        case msparserConstants.MOD_TYPE_C_PROTEIN =>
          modType.append("C-Protein")
        case msparserConstants.MOD_TYPE_C_PROTEIN_RESIDUE =>
          modType.append("C-Protein (X)")
        case msparserConstants.MOD_TYPE_C_TERM =>
          modType.append("C-term ")
        case msparserConstants.MOD_TYPE_C_TERM_RESIDUE =>
          modType.append("C-term (X)")
        case msparserConstants.MOD_TYPE_N_PROTEIN =>
          modType.append("N-Protein")
        case msparserConstants.MOD_TYPE_N_PROTEIN_RESIDUE =>
          modType.append("N-Protein (X)")
        case msparserConstants.MOD_TYPE_N_TERM =>
          modType.append("N-term ")
        case msparserConstants.MOD_TYPE_N_TERM_RESIDUE =>
          modType.append("N-term (X)")
        case msparserConstants.MOD_TYPE_RESIDUE => {
          modType.append('(')
          for (  l <- 0 until mod.getNumberOfModifiedResidues ) {
            modType.append(mod.getModifiedResidue(l))
          }
          modType.append(')')                
        }
      }
      
      this.logger.debug(k+" - "+mod.getTitle()+" on "+modType.toString())
      this.logger.debug("   delta = "+mod.getDelta(msparserConstants.MASS_TYPE_MONO))
      
      val nlosses = mod.getNeutralLoss(msparserConstants.MASS_TYPE_MONO)
      val nlReq = mod.getNeutralLossRequired()
      
      for ( n <- 0 until nlosses.size.toInt ) {
        this.logger.debug("   nl["+n+"]="+nlosses.get(n)+" (req="+nlReq.get(n)+")")
      }
    }
  }
}
