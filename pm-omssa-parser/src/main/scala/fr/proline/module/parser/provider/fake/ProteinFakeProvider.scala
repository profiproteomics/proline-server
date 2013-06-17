package fr.proline.module.parser.provider.fake

import scala.collection.mutable.HashMap
import fr.proline.core.om.model.msi.Protein
import fr.proline.core.om.model.msi.SeqDatabase
import fr.proline.core.om.provider.msi.IProteinProvider
//import fr.proline.repository.DatabaseContext

/**
 * Return only no value (Option.empty)
 */
object ProteinFakeProvider extends IProteinProvider {

  val pdiDbCtx = null
  //  val pdiDbCtx: DatabaseContext = null
  private var protByAcc: HashMap[String, Protein] = new HashMap[String, Protein]()

  def getProteinsAsOptions(protIds: Seq[Long]): Array[Option[Protein]] = {
    val retArray = new Array[Option[Protein]](1)
    retArray.update(0, Option.empty[Protein])
    return retArray
  }

  def getProtein(seq: String): Option[Protein] = Option.empty[Protein]

  def getProtein(accession: String, seqDb: SeqDatabase): Option[Protein] = {
    var retVal = protByAcc.get(accession.concat(seqDb.name))
    if (retVal == None) {
      val p: Protein = new Protein(sequence = "AACCCMMM", id = Protein.generateNewId, alphabet = "aa")
      protByAcc += accession.concat(seqDb.name) -> p
      retVal = Some(p)
    }
    retVal
  }

}