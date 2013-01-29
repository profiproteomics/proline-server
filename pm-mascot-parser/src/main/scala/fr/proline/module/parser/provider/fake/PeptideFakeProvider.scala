package fr.proline.module.parser.provider.fake

import scala.collection.mutable.HashMap
import scala.collection.Seq
import fr.proline.core.om.model.msi.LocatedPtm
import fr.proline.core.om.model.msi.Peptide
import fr.proline.core.om.provider.msi.IPeptideProvider


object PeptideFakeProvider extends IPeptideProvider {
  
  val psDbCtx = null

  var pepByID:HashMap[Int, Peptide] = new HashMap[Int, Peptide]()
  var pepBySeqPtm:HashMap[String, Peptide] = new HashMap[String, Peptide]()
  
  /**
   * Get peptides with specified Ids if already created using this Provider  
   * or create a new fake one with specified ID + seq = "X", no ptm, calcularedMass = Double.MaxValue
   *  
   */
  def getPeptidesAsOptions(peptideIds: Seq[Int] ): Array[Option[Peptide]] = {    
    var peptidesLst = List[Option[Peptide]]()
        
	peptideIds foreach (id => {
	  if(pepByID.contains(id))
	    peptidesLst :+ pepByID.get(id)
	  else {
	    var pep =  new Peptide (id=id, sequence="X", ptmString=null, ptms=null, calculatedMass =Double.MaxValue)
		peptidesLst :+ Some(pep)
		pepByID += (id -> pep)
	  }
	})
    return peptidesLst.toArray 
  }
  
  def getPeptides(peptideIds: Seq[Int] ): Array[Peptide] = {
    this.getPeptidesAsOptions( peptideIds ).filter { _ != None }.map { _.get }
  }
  
  /**
   * Get peptide with specified sequence and ptms if already created using this Provider  
   * or create a new fake one with id=Int.MinValue, calcularedMass = Double.MaxValue and specifed properties
   *  
   */
 
  def getPeptide(peptideSeq:String, pepPtms:Array[LocatedPtm]) : Option[Peptide]  = {
    var keyBuilder :StringBuilder = new StringBuilder(peptideSeq) 
    for (ptm <- pepPtms)( keyBuilder.append(ptm.definition.names.fullName).append(ptm.seqPosition))  
      
      if(pepBySeqPtm.contains(keyBuilder.toString()))
	    return pepBySeqPtm.get(keyBuilder.toString())
	  else {
	    var pep =  new Peptide ( sequence=peptideSeq, ptms=pepPtms)		
  		pepBySeqPtm += (keyBuilder.toString() -> pep)
  		return Some(pep)
	  }
  }
   
	def getPeptidesAsOptionsBySeqAndPtms(peptideSeqsAndPtms: Seq[Pair[String, Array[LocatedPtm]]]) : Array[Option[Peptide]] = {
	    var result = Array.newBuilder[Option[Peptide]]
       peptideSeqsAndPtms.foreach( entry =>  {
         result += this.getPeptide(entry._1,entry._2)
       })
       result.result
	}
	

  
}