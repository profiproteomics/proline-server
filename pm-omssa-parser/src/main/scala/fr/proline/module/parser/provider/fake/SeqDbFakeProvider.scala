package fr.proline.module.parser.provider.fake

import fr.proline.core.om.model.msi.SeqDatabase
import fr.proline.core.om.provider.msi.ISeqDatabaseProvider
//import fr.proline.repository.DatabaseContext

object SeqDbFakeProvider extends ISeqDatabaseProvider {

  val pdiDbCtx = null
  //  val pdiDbCtx: DatabaseContext = null
  def getSeqDatabasesAsOptions(seqDBIds: Seq[Long]): Array[Option[SeqDatabase]] = {
    var result = new Array[Option[SeqDatabase]](1)
    result(0) = None
    result
  }

  def getSeqDatabases(seqDBIds: Seq[Long]): Array[SeqDatabase] = {
    this.getSeqDatabasesAsOptions(seqDBIds).filter(_ != None).map(_.get)
  }

  def getSeqDatabase(seqDBName: String, fastaPath: String): Option[SeqDatabase] = {
    Some(new SeqDatabase(id = SeqDatabase.generateNewId,
      name = seqDBName,
      filePath = fastaPath,
      sequencesCount = 0,
      version = "",
      //      releaseDate = ""))
      releaseDate = new java.util.Date))
    //      releaseDate = Some(new java.util.Date)))
  }

}