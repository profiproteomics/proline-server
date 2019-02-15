package fr.proline.module.exporter.mzidentml

import java.io.ByteArrayInputStream
import java.io.FileWriter
import java.util
import java.util.Calendar
import javax.xml.bind.JAXBContext
import javax.xml.bind.JAXBElement
import javax.xml.bind.Unmarshaller
import javax.xml.parsers.SAXParserFactory

import com.typesafe.scalalogging.LazyLogging
import fr.profi.chemistry.algo.IsoelectricPointComputer
import fr.profi.chemistry.model.ProteinogenicAminoAcidTable
import fr.profi.cv._
import fr.profi.obo._
import fr.profi.util.collection._
import fr.profi.util.serialization.CustomDoubleJacksonSerializer
import fr.profi.util.serialization.ProfiJSMSerialization
import fr.proline.context.IExecutionContext
import fr.proline.core.algo.msi.filtering.ResultSummaryFilterBuilder
import fr.proline.core.dal.DoJDBCReturningWork
import fr.proline.core.dal.helper.MsiDbHelper
import fr.proline.core.dal.tables.SelectQueryBuilder.any2ClauseAdd
import fr.proline.core.dal.tables.SelectQueryBuilder2
import fr.proline.core.dal.tables.msi.MsiDbObjectTreeTable
import fr.proline.core.dal.tables.msi.MsiDbPeptideMatchObjectTreeMapTable
import fr.proline.core.om.model.msi.FragmentIonSeries
import fr.proline.core.om.model.msi._
import fr.proline.core.om.provider.PeptideCacheExecutionContext
import fr.proline.core.om.provider.msi.IMSISearchProvider
import fr.proline.core.om.provider.msi.impl.SQLBioSequenceProvider
import fr.proline.core.om.provider.msi.impl.SQLMsiSearchProvider
import fr.proline.core.om.provider.msi.impl.SQLResultSummaryProvider
import fr.proline.core.orm.msi.Scoring
import fr.proline.module.exporter.mzidentml.model.Contact
import fr.proline.module.exporter.mzidentml.model.Organization
import org.xml.sax.XMLReader
import uk.ac.ebi.jmzidml.model.mzidml._
import uk.ac.ebi.jmzidml.model.mzidml.{Enzyme => MzIdEnzyme}
import uk.ac.ebi.jmzidml.model.mzidml.{Peptide => MzIdPeptide}
import uk.ac.ebi.jmzidml.model.mzidml.{Organization => MzIdOrganization}
import uk.ac.ebi.jmzidml.model.utils.ModelConstants
import uk.ac.ebi.jmzidml.xml.io.MzIdentMLMarshaller

import scala.collection.JavaConversions
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

object MzIdExporter {

  object CustomSerializer extends ProfiJSMSerialization with CustomDoubleJacksonSerializer

  protected val _xmlReader: XMLReader = {
    // Create a new XML parser
    val saxFactory = SAXParserFactory.newInstance()
    saxFactory.setNamespaceAware(true)
    val saxParser = saxFactory.newSAXParser()
    saxParser.getXMLReader
  }

  protected val _unmarshaller: Unmarshaller = {
    JAXBContext.newInstance(ModelConstants.PACKAGE).createUnmarshaller()
  }

  /** Unmarshall the desired object by providing an XML string */
  def unmarshallXmlString[T](_class: Class[T], xmlStr: String): T = {
    //val inputSource = new InputSource(new StringReader(xmlStr))
    //val saxSource = new SAXSource(_xmlReader, inputSource)
    //this._unmarshaller.unmarshal(saxSource, _class).getValue()
    //this._unmarshaller.unmarshal(new StringReader(xmlStr)).asInstanceOf[JAXBElement[T]].getValue()

    val input = new ByteArrayInputStream(xmlStr.getBytes())
    val jaxbObject = this._unmarshaller.unmarshal(input)
    jaxbObject.asInstanceOf[JAXBElement[T]].getValue
  }

  def _loadResultSummary(rsmId: Long, execContext: IExecutionContext): ResultSummary = {
    val rsmProvider = new SQLResultSummaryProvider(PeptideCacheExecutionContext(execContext) )
    val rsm = rsmProvider.getResultSummary(rsmId, loadResultSet = true).get
    rsm
  }

  def _getUnimodIdByPtmId(execContext: IExecutionContext): mutable.LongMap[Long] = {
     new MsiDbHelper(execContext.getMSIDbConnectionContext).getUnimodIdByPtmId().toLongMap()
  }

  def _getSpectrumIndexById(execContext: IExecutionContext, rsId: Long): mutable.LongMap[Int] = {

    val msiDbHelper = new MsiDbHelper(execContext.getMSIDbConnectionContext)

    val msiIds = msiDbHelper.getResultSetsMsiSearchIds(Array(rsId))
    val pklIds = DoJDBCReturningWork.withEzDBC(execContext.getMSIDbConnectionContext) { msiEzDBC =>
      msiEzDBC.selectLongs("SELECT peaklist_id FROM msi_search WHERE id IN (" + msiIds.mkString(",") + ")")
    }

    msiDbHelper.getSpectrumIndexByIdByPL(pklIds).flatMap(_._2)
  }

  def _getMISearchProvider(execContext: IExecutionContext): IMSISearchProvider = {
    new SQLMsiSearchProvider(udsDbCtx = execContext.getUDSDbConnectionContext, msiDbCtx = execContext.getMSIDbConnectionContext)
  }

  def _getSpectrumMatchByPeptideMatchId(execContext: IExecutionContext, pepMatches: Array[PeptideMatch]): Map[Long, SpectrumMatch] = {
    val spectrumMatchesByPeptMatchId = new mutable.HashMap[Long, SpectrumMatch]
    DoJDBCReturningWork.withEzDBC(execContext.getMSIDbConnectionContext) (msiEzDBC => {
      val pepMatchesIds = pepMatches.map(_.id)
      val pepMatchSpectrumMatchQuery = new SelectQueryBuilder2(MsiDbPeptideMatchObjectTreeMapTable, MsiDbObjectTreeTable).mkSelectQuery((pmT, pmC, otT, otC) =>
        List(pmT.PEPTIDE_MATCH_ID, otT.CLOB_DATA) -> " WHERE " ~ pmT.OBJECT_TREE_ID ~ "=" ~ otT.ID ~ " AND " ~ pmT.SCHEMA_NAME ~ "= 'peptide_match.spectrum_match' AND " ~
          pmT.PEPTIDE_MATCH_ID ~ " IN (" ~ pepMatchesIds.mkString(",") ~ ")")

      msiEzDBC.selectAndProcess(pepMatchSpectrumMatchQuery) { r =>
        val id = r.nextLong
        val spectrumMatch = CustomSerializer.deserialize[SpectrumMatch](r.nextString)
        spectrumMatchesByPeptMatchId += (id -> spectrumMatch)
      }
    })
    Map() ++ spectrumMatchesByPeptMatchId
  }

}

/**
 * @author David Bouyssie
 *
 */
class MzIdExporter(rsmId: Long, executionContext: IExecutionContext) extends ParamMaker with LazyLogging {

  // CONSTANT VALUE Used As ID IN EXPORT. May be used in ref
  final val spectraDataIdPrefix: String = "SD_"
  final val searchDatabaseRefPrefix: String = "SDB_"
  final val protDetectProtoId = "PDP_MascotParser_1" // "PDP_Proline"
  final val profiOrganizationId = "ORG_PROFI"
  final val matrixScienceOrganizationId = "ORG_MSL"
  final val profiPersonId  = "PROFI_CONTACT"
  final val matrixPersonId  ="Matrix_Science_Limited"
  final val submitterPersonId  ="PERSON_DOC_OWNER"
  final val submitterOrganizationId = "ORG_DOC_OWNER"
  final val prolineSoftwareId  = "AS_Proline"
  final val softwarePrefixId  = "AS_"
  final val spectrumIdentificationListId = "SIL_1"
  final val spectrumIdentificationId = "SI"
  final val spectrumIdentificationIdPrefix = "SI_"
  final val spectrumIdentificationResultPrefix = "SIR_"
  final val spectrumIdentificationItemPrefix = "SII_"
  final val proteinDetectionListId = "PDL_1"
  final val proteinDetectionId = "PD_1"
  final val peptidePrefix = "peptide_"
  final val spectrumIdentificationProtocolIdPrefix = "SIP_"
  final val enzymePrefix = "ENZ_"
  final val proteinAmbiguityGroupPrefix = "PAG_"
  final val proteinDetectionHypothesis = "PDH_"
  final val peptideEvidencePrefix = "PE"

  final val measureMZId = "m_mz"
  final val measureIntensityId = "m_intensity"
  final val measureErrId = "m_error"

  // Initialize some some proline objects
  val rsm: ResultSummary = MzIdExporter._loadResultSummary(rsmId, executionContext)
  val rs: ResultSet = rsm.resultSet.get

  val unimodIdByPtmId: mutable.LongMap[Long]=  MzIdExporter._getUnimodIdByPtmId(executionContext)
  val spectrumNumberById: mutable.LongMap[Int] = MzIdExporter._getSpectrumIndexById(executionContext,  rs.id)

  val msiSearchIds: Array[Long] =  new MsiDbHelper(executionContext.getMSIDbConnectionContext).getResultSetsMsiSearchIds(Seq(rsm.getResultSetId))
  val msiSearches: Array[MSISearch] =  MzIdExporter._getMISearchProvider(executionContext).getMSISearches(msiSearchIds)
  require(!msiSearches.isEmpty , "ResultSet #" + rs.id + " has no associated MSI Search")
  logger.debug(" msiSearches "+msiSearches.length)

  val rsPepById: Map[Long, fr.proline.core.om.model.msi.Peptide] = rs.getPeptideById()
  val rsProtMatchesById: Map[Long, ProteinMatch] =  rs.getProteinMatchById()
  val pepInstByPepId: Map[Long, PeptideInstance] = Map() ++ rsm.peptideInstances.map(pi => pi.peptide.id -> pi)
  val validPepMatchIdSet: Set[Long] = rsm.peptideInstances.flatMap(_.peptideMatchIds).toSet


  // Set the main mzIdentML Element ids
  val searchDbRefBySeqDbIds: mutable.HashMap[Long, String] = new mutable.HashMap[Long,String]()
  msiSearches.flatMap(_.searchSettings.seqDatabases).foreach(seqDb => {
    searchDbRefBySeqDbIds += (seqDb.id -> (searchDatabaseRefPrefix + seqDb.id.toString))
  })

//TODO : Define which contact info we give

  val profiOrganization: MzIdOrganization = {
    val org = new MzIdOrganization()
    org.setId(profiOrganizationId)
    org.setName("Proteomics French Infrastructure")

    //Create ProFI contact info
    org.getCvParam.add(PsiCvParam(PsiMs.ContactEmail,"jerome.garin@cea.fr"))
    org.getCvParam.add(PsiCvParam(PsiMs.ContactURL,"http://www.profiproteomics.fr"))
    org
  }

  val profiContact: Person = {

    val proFI = new Person()
    proFI.setId(profiPersonId)
//    proFI.setFirstName()
//    proFI.setLastName()
    val affiliation = new Affiliation()
    affiliation.setOrganization(profiOrganization)
    proFI.getAffiliation.add(affiliation)
    //Create ProFI contact info
    proFI.getCvParam.add(PsiCvParam(PsiMs.ContactEmail,"christophe.bruley@cea.fr"))
    proFI.getCvParam.add(PsiCvParam(PsiMs.ContactURL,"http://www.profiproteomics.fr"))
    proFI
  }

  var submitterOrg : MzIdOrganization = {
    val org = new MzIdOrganization()
    org.setId(submitterOrganizationId)
    org
  }

  var submitterContact : Person  = {
    val submitter = new Person()
    submitter.setId(submitterPersonId)
    val affiliation = new Affiliation()
    affiliation.setOrganization(submitterOrg)
    submitter.getAffiliation.add(affiliation)
    submitter
  }


  lazy val matrixScienceOrganization: MzIdOrganization = {
    val org = new MzIdOrganization()
    org.setId(matrixScienceOrganizationId)
    org.setName("Matrix Science Limited")

    //Create Matrix Science contact info
    org.getCvParam.add(PsiCvParam(PsiMs.ContactEmail,"support@matrixscience.com"))
    org.getCvParam.add(PsiCvParam(PsiMs.ContactAddress,"64 Baker Street, London W1U 7GB, UK"))
    org.getCvParam.add(PsiCvParam(PsiMs.ContactPhoneNumber,"+44 (0)20 7486 1050"))

    org
  }

  lazy val matrixScienceContact: Person = {
    val matrix = new Person()
    matrix.setId(matrixPersonId)
    val affiliation = new Affiliation()
    affiliation.setOrganization(matrixScienceOrganization)
    matrix.getAffiliation.add(affiliation)
    matrix
  }

  val auditCollection: AuditCollection = {
    val auditCollec = new AuditCollection()
    auditCollec.getPerson.add(matrixScienceContact)
    auditCollec.getPerson.add(profiContact)
    auditCollec.getPerson.add(submitterContact)
    auditCollec.getOrganization.add(matrixScienceOrganization)
    auditCollec.getOrganization.add(profiOrganization)
    auditCollec.getOrganization.add(submitterOrg)
    auditCollec

  }

  //Create searchDatabases for all seq database of all MSISearches
  lazy val searchDatabases: Seq[SearchDatabase] = {

    val searchDbs = new ArrayBuffer[SearchDatabase]()

    msiSearches.foreach( msiSearch => {
      msiSearch.searchSettings.seqDatabases.foreach( seqDb => {
        val searchDbRef = searchDbRefBySeqDbIds(seqDb.id)
        if(!searchDbs.exists(_.getId.equals(searchDbRef))) {
          val seqDbFile = new java.io.File(seqDb.filePath)
          val fileFormat = FileFormat(PsiCvParam(PsiMs.FASTAFormat))

          val sd = new SearchDatabase()
          sd.setId(searchDbRef)
          sd.setName(seqDbFile.getName) // the file name
          sd.setDatabaseName(makeUserParamGroup(seqDb.name))
          sd.setNumDatabaseSequences(seqDb.sequencesCount.toLong)
          //sd.setNumResidues(value)

          val releaseDateCal = java.util.Calendar.getInstance()
          releaseDateCal.setTime(seqDb.releaseDate)
          sd.setReleaseDate(releaseDateCal)

          sd.setVersion(seqDb.version)
          //sd.setExternalFormatDocumentation(value)
          sd.setFileFormat(fileFormat)
          sd.setLocation(seqDb.filePath)
          searchDbs += sd
        }
      })
    })

    searchDbs
  }

  protected lazy val rsDbSeqByProtMatchId = {

    val tmpDbSeqByProtMatchId = new mutable.HashMap[Long, DBSequence]()

    for (protMatch <- rs.proteinMatches) {
      val sqDBId = protMatch.seqDatabaseIds(0)
      val searchDBRef= searchDbRefBySeqDbIds(sqDBId)
      val seq = new DBSequence()
      seq.setId(searchDBRef+"_"+protMatch.accession)
      seq.setAccession(protMatch.accession)

      seq.setSearchDatabase(searchDatabases.filter(_.getId.equals(searchDBRef)).head) // TODO: retrieve the right one => OK but if multiple

      val descParam = PsiCvParam(PsiMs.ProteinDescription, protMatch.description)
      seq.getCvParam.add(descParam)

      tmpDbSeqByProtMatchId += protMatch.id -> seq
    }


    tmpDbSeqByProtMatchId
  }


  private def initPepEvidencesMaps(mzidPepByPepId: mutable.HashMap[Long, MzIdPeptide], validatedOnly: Boolean ): (mutable.HashMap[Long, ArrayBuffer[PeptideEvidence]],  mutable.HashMap[SequenceMatch, PeptideEvidence]) = {
    val pepEvidencesByPepId = new mutable.HashMap[Long, ArrayBuffer[PeptideEvidence]]
    val pepEvidenceBySeqMatch = new mutable.HashMap[SequenceMatch, PeptideEvidence]

    val protMatches = if(!validatedOnly) {
      rs.proteinMatches
    } else {
      // If Validated, get Only Typical And SameSet ProtMatches
      val protMatchesIds = rsm.proteinSets.flatMap(_.getSameSetProteinMatchIds)
      protMatchesIds.map(rsProtMatchesById(_))
    }

    for (protMatch <- protMatches; seqMatch <- protMatch.sequenceMatches) {
      val pepId = seqMatch.getPeptideId()
      if( (!validatedOnly || (validatedOnly && pepInstByPepId.contains(pepId))) && mzidPepByPepId.contains(pepId)) {
        val peptide = mzidPepByPepId(pepId)

        val pepEv = new PeptideEvidence()
        pepEv.setId(List(peptideEvidencePrefix, protMatch.accession, pepId, seqMatch.start, seqMatch.end).mkString("_"))
        pepEv.setDBSequence(rsDbSeqByProtMatchId(protMatch.id))
        pepEv.setEnd(seqMatch.end)
        //pepEv.setFrame(value)
        pepEv.setIsDecoy(seqMatch.isDecoy)
        pepEv.setPeptide(peptide)
        pepEv.setPost(seqMatch.residueAfter.toString)
        pepEv.setPre(seqMatch.residueBefore.toString)
        pepEv.setStart(seqMatch.start)
        //pepEv.setTranslationTable(translationTable)

        pepEvidencesByPepId.getOrElseUpdate(pepId, new ArrayBuffer[PeptideEvidence]) += pepEv
        pepEvidenceBySeqMatch += seqMatch -> pepEv
      }
    }
    (pepEvidencesByPepId,pepEvidenceBySeqMatch)
  }

  val cvList = new CvList()
  cvList.getCv.add(ControlledVocabulary.psiMsCV)
  cvList.getCv.add(ControlledVocabulary.unimodCV)
  cvList.getCv.add(ControlledVocabulary.unitCV)

  // FIXME use only first SearchSettings params ?
  val mzidSearchEngine: AnalysisSoftware = {
    val searchSettings = msiSearches(0).searchSettings
    val softName = searchSettings.softwareName

    val searchEngine = new AnalysisSoftware()
    searchEngine.setId(softwarePrefixId + softName)
    searchEngine.setName(softName)
    searchEngine.setVersion(searchSettings.softwareVersion)

    val softCvParamByName = Map("Mascot" -> PsiCvParam(PsiMs.Mascot),
      "OMSSA" -> PsiCvParam(PsiMs.OMSSA)
    )
    val softCvParamOpt = softCvParamByName.get(softName)
    require(softCvParamOpt.isDefined, "unsupported search engine '" + softName + "'")

    searchEngine.setSoftwareName(makeParamGroup(softCvParamOpt.get))

//    val contactRole = ContactRole(
//      matrixScienceContact, // TODO: retrieve the right one
//      Role(PsiCvParam(PsiMs.SoftwareVendor))
//    )
//    searchEngine.setContactRole(contactRole)

    searchEngine
  }

  val prolineSuite: AnalysisSoftware = {
    val proline = new AnalysisSoftware()
    proline.setId(prolineSoftwareId)
    val version = new  fr.proline.module.exporter.mzidentml.Version()
    proline.setName("Proline (MzIdentML Exporter)")
    proline.setVersion(version.getVersion)

    proline.setSoftwareName(makeParamGroup(PsiCvParam(PsiMs.Proline)))

    val contactRole = ContactRole(
      profiContact,
      Role(PsiCvParam(PsiMs.Programmer))
    )
    proline.setContactRole(contactRole)

    proline
  }

  private def _getBioSequenceById(protMatches: Array[ProteinMatch] ) : mutable.LongMap[BioSequence] = {

    val bioSeqProvider = new SQLBioSequenceProvider(executionContext.getMSIDbConnectionContext)
    val bioSeqIds: Array[Long] = protMatches.map(_.getProteinId()).filter(_>0)

    val loadedBioSeq = bioSeqProvider.getBioSequences(bioSeqIds, loadSequence = true)
    loadedBioSeq.mapByLong(_.id)
  }

 private def _exportHeader(mzIdMarshaller: MzIdentMLMarshaller, writer:FileWriter ): Unit = {
    // XML header
    writer.write(mzIdMarshaller.createXmlHeader() + "\n")

    // mzIdentML start tag
    writer.write(mzIdMarshaller.createMzIdentMLStartTag("12345") + "\n")

    mzIdMarshaller.marshal(this.cvList, writer)
    writer.write("\n")

    mzIdMarshaller.marshal(this._buildAnalysisSoftwareList(), writer)
    writer.write("\n")

    mzIdMarshaller.marshal(this._buildProvider(), writer)
    writer.write("\n")

    mzIdMarshaller.marshal(auditCollection, writer)
    writer.write("\n")
  }

  def exportResultSummary(filePath: String, contact: Contact, organization: Organization): Unit = {
    //Get Contact Info
    this.submitterContact.setFirstName(contact.firstName)
    this.submitterContact.setLastName(contact.lastName)
    if(contact.email.isDefined)
      submitterContact.getCvParam.add(PsiCvParam(PsiMs.ContactEmail,contact.email.get))
    if(contact.url.isDefined)
      submitterContact.getCvParam.add(PsiCvParam(PsiMs.ContactURL,contact.url.get))

    //Get Organization Info
    this.submitterOrg.setName(organization.name)
    if(organization.url.isDefined)
      submitterOrg.getCvParam.add(PsiCvParam(PsiMs.ContactURL,organization.url.get))

    exportResult(filePath, validatedResult = true)
  }

 private def exportResult(filePath: String, validatedResult : Boolean) {

    val mzIdMarshaller = new MzIdentMLMarshaller()
    val writer = new FileWriter(filePath)

    // mzIdentML
    //     cvList
    //     AnalysisSoftwareList (optional)
    //       AnalysisSoftware [1..*]
    //     Provider (optional)
    //     AuditCollection (optional)
    //        Person
    //        Organization
    //     AnalysisSampleCollection (optional)
    //        Sample  [1..*]
    //     SequenceCollection (optional)
    //        DBSequence [0..*]
    //        Peptide [0..*]
    //        PeptideEvidence [0..*]
    //     AnalysisCollection
    //        SpectrumIdentification [1..*]
    //        ProteinDetection (optional)
    //     AnalysisProtocolCollection
    //        SpectrumIdentificationProtocol [1..*]
    //        ProteinDetectionProtocol (optional)
    //     DataCollection
    //         Inputs
    //         AnalysisData
    //             SpectrumIdentificationList [1..*]
    //             ProteinDetectionList (optional)
    //     BibliographicReference
    // /mzIdentML

    // Note: writing of '\n' characters is optional and only for readability of the produced XML document
    // Also note: since the XML is produced in individual parts, the overall formatting of the document
    //            is not as nice as it would be when marshalling the whole structure at once.


   //**** Export cvList;  AnalysisSoftwareList; Provider; AuditCollection
    _exportHeader(mzIdMarshaller,writer)
    logger.debug(" **** Exported Header")

    //  export the samples AnalysisSampleCollection : NO, information entered while submitting anyway...
    //AnalysisSampleCollection analysisSampleCollection = unmarshaller.unmarshal(MzIdentMLElement.AnalysisSampleCollection.getXpath());
    //mzIdMarshaller.marshal(analysisSampleCollection, writer);
    //writer.write("\n");

   //**** Export SequenceCollection
    val mzidPepByPepId = getPepByPepId(validatedResult)
    val (pepEvidencesByPepId,pepEvidenceBySeqMatch ) = initPepEvidencesMaps(mzidPepByPepId, validatedResult)
    mzIdMarshaller.marshal(this._buildSequenceCollection(pepEvidencesByPepId, mzidPepByPepId, validatedResult), writer)
    writer.write("\n")
    logger.debug(" **** Exported SequenceCollection ")

   //**** Export AnalysisCollection & AnalysisProtocolCollection
   // Export proteinDetection only if RSM is validation of search RS or Merged RS, not if merged RSM
    val rsmFromValidation = rs.isSearchResult || rs.mergedResultSummaryId == 0

    val analyseProtocolColl = this._buildAnalysisProtocolCollection(rsmFromValidation)
    val inputs = this._buildInputs()
    val analysisCollecNode = this._buildAnalysisCollection(rsmFromValidation, analyseProtocolColl,inputs)

    //writer.write(analysisCollecNode + "\n")
    mzIdMarshaller.marshal(analysisCollecNode, writer)
    writer.write("\n")
    mzIdMarshaller.marshal(analyseProtocolColl, writer)
    writer.write("\n")
   logger.debug(" **** Exported Analysis(Protocol)Collection ")

   //**** Start Exporting DataCollection
    writer.write(mzIdMarshaller.createDataCollectionStartTag() + "\n")

    mzIdMarshaller.marshal(inputs, writer) //--- Export Inputs
    writer.write("\n")

    writer.write(mzIdMarshaller.createAnalysisDataStartTag() + "\n")

   //**** Start Exporting SpectrumIdentificationList
    writer.write(
      mzIdMarshaller.createSpectrumIdentificationListStartTag(
        spectrumIdentificationListId, null, msiSearches(0).searchedSequencesCount.toLong
      ) + "\n"
    )

    //**** Exporting  FragmentationTable
    //FragmentationTable table = unmarshaller.unmarshal(MzIdentMLElement.FragmentationTable.getXpath());
    val fragTable: FragmentationTable =  this._buildFragmentationTable()
    mzIdMarshaller.marshal(fragTable, writer)
    writer.write("\n")
   logger.debug(" **** Exported FragmentationTable ")

   //**** Exporting  SpectrumIdentificationResult
   val pepMatches: Array[PeptideMatch] = if(validatedResult)  rsm.peptideInstances.flatMap(_.peptideMatches)   else rs.peptideMatches
   logger.debug(" **** Will export pepMatches "+pepMatches.length)
   val measureById: Map[String, Measure] = JavaConversions.asScalaBuffer(fragTable.getMeasure).map(m => m.getId ->m).toMap
   val allSpectrumIdentResults = _buildSpectrumIdentificationResult(pepMatches,measureById, mzidPepByPepId, pepEvidencesByPepId, inputs)
   logger.debug(" **** Got "+allSpectrumIdentResults.length+" SpectrumIdentResults ")
    allSpectrumIdentResults.foreach( sir => {
      mzIdMarshaller.marshal(sir, writer)
      writer.write("\n")
    })

   //**** End Exporting SpectrumIdentificationList
    writer.write(mzIdMarshaller.createSpectrumIdentificationListClosingTag() + "\n")

   //****  Exporting ProteinDetectionList
    writer.write(mzIdMarshaller.createProteinDetectionListStartTag(proteinDetectionListId, null) + "\n")
   if(!validatedResult) //May export each proteinMatch of RS...
     throw new RuntimeException("Unable to export unvalidated result")

    logger.debug(" **** Will export protSets "+rsm.proteinSets.length)
    for (protSet <- rsm.proteinSets) {

      val protAmbGroup = new ProteinAmbiguityGroup()
      protAmbGroup.setId(proteinAmbiguityGroupPrefix + protSet.id)
      protAmbGroup.getCvParam.add(PsiCvParam(PsiMs.ProteinGroupPassesThreshold, true.toString))
      val protDetectHypoList = protAmbGroup.getProteinDetectionHypothesis

      for (protMatchId <- protSet.getSameSetProteinMatchIds) { //Only export sameset ProtMatches
        val protMatch = rsProtMatchesById(protMatchId)

        val protDetectHypo = new ProteinDetectionHypothesis()
        protDetectHypo.setId(proteinDetectionHypothesis + protMatch.id)
        protDetectHypo.setDBSequence(rsDbSeqByProtMatchId(protMatch.id))
        protDetectHypo.setPassThreshold(protSet.isValidated)

        val pepHypoList = protDetectHypo.getPeptideHypothesis
        val confidentPepSeqs = new ArrayBuffer[String](protMatch.sequenceMatches.length)

        for (seqMatch <- protMatch.sequenceMatches) {
          val pepId = seqMatch.getPeptideId()
          if( pepInstByPepId.contains(pepId) ) {

            // Retrieve the validated peptide
            val pepInstance = pepInstByPepId(pepId)

            val pepHypo = new PeptideHypothesis()
            pepHypo.setPeptideEvidence(pepEvidenceBySeqMatch(seqMatch))
            val specIdentItemRefList = pepHypo.getSpectrumIdentificationItemRef

            // Retrieve peptide matches of the validated peptide
            for (pepMatchId <- pepInstance.getPeptideMatchIds()) {
              val specIdentItemRef = new SpectrumIdentificationItemRef()
              specIdentItemRef.setSpectrumIdentificationItemRef(spectrumIdentificationItemPrefix + pepMatchId)
              specIdentItemRefList.add(specIdentItemRef)
            }

            confidentPepSeqs += pepInstance.peptide.sequence
            pepHypoList.add(pepHypo)
          }//only for seqMatches of valid peptide Matches
        } //End for all prot Sequence Matches

        val protHypoCvParams = protDetectHypo.getCvParam
        if(protMatchId.equals(protSet.getRepresentativeProteinMatchId())) {
          protHypoCvParams.add(
            PsiCvParam(PsiMs.GroupRepresentative)
          )
        }
        protHypoCvParams.add(
          PsiCvParam(PsiMs.LeadingProtein)
        )

        protHypoCvParams.add(
          PsiCvParam(PsiMs.MascotScore, protMatch.score.toString) //TODO For Other Search Engine
        )
        if(protSet.proteinMatchCoverageById != null && protSet.proteinMatchCoverageById.contains(protMatch.id)) {
          protHypoCvParams.add(
            PsiCvParam(PsiMs.SequenceCoverage, protSet.proteinMatchCoverageById(protMatch.id).toString)
          )
        }
        /*protHypoCvParams.add(
          makeCvParam("MS:1001097", "distinct peptide sequences", psiCV, pepSeqs.distinct.length.toString)
        )*/
        protHypoCvParams.add(
          PsiCvParam(PsiMs.ConfidentDistinctPeptideSequences, confidentPepSeqs.distinct.length.toString)
        )

        protDetectHypoList.add(protDetectHypo)
      } //End for all SameSets

      mzIdMarshaller.marshal(protAmbGroup, writer)
      writer.write("\n")
    }
    logger.debug(" **** Exported protSets  ")

    // End of protein detection list
   writer.write(createCVParamTag(PsiCvParam(PsiMs.CountOfIdentifiedProteins,rsm.proteinSets.length.toString)))
   writer.write("\n")

   writer.write(mzIdMarshaller.createProteinDetectionListClosingTag() + "\n")

    // End of analysis data
    writer.write(mzIdMarshaller.createAnalysisDataClosingTag() + "\n")

    // End of data collection
    writer.write(mzIdMarshaller.createDataCollectionClosingTag() + "\n")

    //BibliographicReference ref = unmarshaller.unmarshal(MzIdentMLElement.BibliographicReference.getXpath());
    //mzIdMarshaller.marshal(ref, writer)
    //writer.write("\n")

    writer.write(mzIdMarshaller.createMzIdentMLClosingTag())

    // End of mzIdentML file
    writer.close()
  }

  private def _buildSpectrumIdentificationResult(pepMatches : Array[PeptideMatch], measureById: Map[String, Measure], mzidPepByPepId: mutable.HashMap[Long, MzIdPeptide], pepEvidencesByPepId: mutable.HashMap[Long, ArrayBuffer[PeptideEvidence]], inputs: Inputs) : Array[SpectrumIdentificationResult] = {
    val arraySpectrumIdentificationResult = new ArrayBuffer[SpectrumIdentificationResult]()
    val pepMatchesByMs2Query = pepMatches.groupBy(_.getMs2Query())
    val spectraDataPerMsiSearch = new mutable.HashMap[Long, SpectraData]()

    val spectrumMatchByPepMatchID = MzIdExporter._getSpectrumMatchByPeptideMatchId(executionContext, pepMatches)

    // TODO: find what to do whith PMF data
    for ((msQuery, qPepMatches) <- pepMatchesByMs2Query) {

      //Get Associated SpectraData
      if(!spectraDataPerMsiSearch.contains(msQuery.msiSearchId)){
        val msiSearch = msiSearches.filter(_.id.equals(msQuery.msiSearchId)).head
        val spDataIt = inputs.getSpectraData.iterator()
        var found = false
        while (spDataIt.hasNext && !found){
          val spD = spDataIt.next()
          if(spD.getId.equals(spectraDataIdPrefix+msiSearch.peakList.id)){
            spectraDataPerMsiSearch.put(msQuery.msiSearchId, spD)
            found = true
          }
        }
        if(!found)
          spectraDataPerMsiSearch.put(msQuery.msiSearchId, inputs.getSpectraData.get(0))
      }
      val spectraData =spectraDataPerMsiSearch(msQuery.msiSearchId)


//      var specIndex: Int = -1
       val thSpIndex : Int = spectrumNumberById(msQuery.spectrumId) - 1
//      try {
//        specIndex = msQuery.spectrumTitle.substring(0, msQuery.spectrumTitle.indexOf(":")).toInt - 1
//      } catch {
//        case _: Throwable => specIndex = thSpIndex
//      }

      val spectId = "index="+thSpIndex
//      logger.info(" For "+msQuery.spectrumTitle+" GOT "+spectId+ " MAY USE "+thSpIndex)
      val specIdentRes = new SpectrumIdentificationResult()
      specIdentRes.setId(spectrumIdentificationResultPrefix + msQuery.id)
      specIdentRes.setSpectraData(spectraData)

      specIdentRes.setSpectrumID(spectId)

      val specIdentItemList = specIdentRes.getSpectrumIdentificationItem

      val charge = msQuery.charge
      val expMoz = msQuery.moz

      for (pepMatch <- qPepMatches) {

        val pepId = pepMatch.peptide.id
        val specIdentItem = new SpectrumIdentificationItem()
        specIdentItem.setId(spectrumIdentificationItemPrefix + pepMatch.id)
        specIdentItem.setCalculatedMassToCharge(
          expMoz - pepMatch.deltaMoz
        )

        val piComputer = new IsoelectricPointComputer(ProteinogenicAminoAcidTable)
        specIdentItem.setCalculatedPI(piComputer.computePI(pepMatch.peptide.sequence )) // for PMF ???
        specIdentItem.setChargeState(charge)
        specIdentItem.setExperimentalMassToCharge(expMoz)
        //specIdentItem.setMassTable(massTable) // when is it needed ?
        specIdentItem.setPassThreshold(validPepMatchIdSet.contains(pepMatch.id))
        specIdentItem.setPeptide(mzidPepByPepId(pepId))
        specIdentItem.setRank(pepMatch.rank)
        //specIdentItem.setSample(sample)

        _addPepMatchParam(msQuery, pepMatch, specIdentItem)


        val pepEvList = specIdentItem.getPeptideEvidenceRef
        for (pepEv <- pepEvidencesByPepId(pepId)) {
          val pepEvRef = new PeptideEvidenceRef()
          pepEvRef.setPeptideEvidence(pepEv)
          pepEvList.add(pepEvRef)
        }
        if(spectrumMatchByPepMatchID.contains(pepMatch.id)) {
          val fragmentation: Fragmentation = _buildFragmentation(pepMatch, spectrumMatchByPepMatchID(pepMatch.id), measureById)
          specIdentItem.setFragmentation(fragmentation)
        }

        specIdentItemList.add(specIdentItem)
      } //End for all pepMatches
      arraySpectrumIdentificationResult += specIdentRes
    }
    arraySpectrumIdentificationResult.toArray
  }

  private def _buildFragmentation(pepMatch: PeptideMatch, spectrumMatch : SpectrumMatch, measureById: Map[String, Measure]) : Fragmentation = {
    //Add Fragmentation
    val fragmentation = new Fragmentation()
    val fragMatchesByIonSeries: Map[(FragmentIonSeries.Value, Int), Array[FragmentMatch]] = spectrumMatch.fragMatches.groupBy(fm => (fm.ionSeries, fm.charge))
    fragMatchesByIonSeries.keys.foreach(key => {
      val ionType = new IonType()
      ionType.setCharge(key._2)

      val psiFragIonType = FragmentationIonTypePSIValue.getPsiMsforFragmentationType(key._1.toString)
      if (psiFragIonType != null) {
        val fragMatches = fragMatchesByIonSeries(key)
        val fragArrayMoz = new FragmentArray()
        fragArrayMoz.setMeasure(measureById(measureMZId))
        val fragArrayIntensity = new FragmentArray()
        fragArrayIntensity.setMeasure(measureById(measureIntensityId))
        val fragArrayErr = new FragmentArray()
        fragArrayErr.setMeasure(measureById(measureErrId))

        fragMatches.foreach(fm => {
          fragArrayMoz.getValues.add(fm.moz.toFloat)
          fragArrayIntensity.getValues.add(fm.intensity)
          fragArrayErr.getValues.add((fm.calculatedMoz - fm.moz).toFloat)
          ionType.getIndex.add(fm.aaPosition)
        })

        ionType.setCvParam(PsiCvParam(psiFragIonType))
        ionType.getFragmentArray.add(fragArrayMoz)
        ionType.getFragmentArray.add(fragArrayIntensity)
        ionType.getFragmentArray.add(fragArrayErr)
        fragmentation.getIonType.add(ionType)
      } else {
        logger.warn("Can't FIND  FragmentationIonTypePSIValue. for " + key._1)
      }

    })
    fragmentation
  }

  private def _addPepMatchParam(msQuery: Ms2Query, pepMatch: PeptideMatch, specIdentItem: SpectrumIdentificationItem) = {
    val psmParams = specIdentItem.getCvParam
    psmParams.add(
      PsiCvParam(PsiMs.SpectrumTitle, msQuery.spectrumTitle)
    )

    // Try to retrieve a peptide instance
    pepInstByPepId.get(pepMatch.peptideId).foreach { pepInst =>
      if (pepInst.validatedProteinSetsCount == 1)
        psmParams.add(PsiCvParam(PsiMs.PeptideUniqueToOneProtein))
      else
        psmParams.add(PsiCvParam(PsiMs.PeptideSharedInMultipleProteins))
    }

    if (pepMatch.properties.isDefined) {
      if (pepMatch.properties.get.isotopeOffset.isDefined)
        specIdentItem.getUserParam.add(
          BuildUserParam("IsotopeError", pepMatch.properties.get.isotopeOffset.get.toString, null)
        )
      if (pepMatch.properties.get.mascotProperties.isDefined) {

        val mascotProps = pepMatch.properties.get.mascotProperties.get

        psmParams.add(
          PsiCvParam(PsiMs.MascotScore, pepMatch.score.toString)
        )

        psmParams.add(
          PsiCvParam(
            PsiMs.MascotExpectationValue,
            mascotProps.getExpectationValue.toString
          )
        )

      }
      if (pepMatch.properties.get.omssaProperties.isDefined) {
        val omssaProp = pepMatch.properties.get.omssaProperties.get

        psmParams.add(
          PsiCvParam(PsiMs.OMSSAPvalue, omssaProp.pValue.toString)
        )
      }
      if (pepMatch.properties.get.xtandemProperties.isDefined) {
        val xTandemProp = pepMatch.properties.get.xtandemProperties.get

        psmParams.add(
          PsiCvParam(PsiMs.XTandemExpect, xTandemProp.expectationValue.toString)
        )
      }
    } //End properties defined
  }

  def exportResultSet(filePath: String) {
    throw new RuntimeException("Not Yet Implemented") // Will be possible ?!

  }

  protected def _buildAnalysisCollection(rsmFromValidation: Boolean, analyseProtocolColl: AnalysisProtocolCollection, inputs : Inputs) : AnalysisCollection = {
    val analyseColl = new AnalysisCollection

    //Create fake SpectrumIdentificationList to get ref only
    val spectrumIdentificationList = new SpectrumIdentificationList()
    spectrumIdentificationList.setId(spectrumIdentificationListId)


    msiSearches.foreach(msiSearch =>{

      val spectrumIdentification  = new SpectrumIdentification
      spectrumIdentification.setId(spectrumIdentificationIdPrefix+ msiSearch.id)
      spectrumIdentification.setSpectrumIdentificationList(spectrumIdentificationList)

      //get SpectrumIdentificationProtocol for this msiSearch
      val result = JavaConversions.asScalaIterator(analyseProtocolColl.getSpectrumIdentificationProtocol.iterator()).filter(_.getId.equals(spectrumIdentificationProtocolIdPrefix+msiSearch.id))
      if(result.hasNext)
        spectrumIdentification.setSpectrumIdentificationProtocol(result.next())
      else
        throw new RuntimeException(s"Unable to get SpectrumIdentificationProtocol for Msi Search id ${msiSearch.id}")

      val calDate = Calendar.getInstance()
      calDate.setTime(msiSearch.date)
      spectrumIdentification.setActivityDate(calDate)

      val allSpectras =  JavaConversions.asScalaIterator(inputs.getSpectraData.iterator())
      breakable {
        for( currentSpectraData <- allSpectras){
          if(currentSpectraData.getId.equals(spectraDataIdPrefix+msiSearch.peakList.id)) {
            val inputSpectra = new InputSpectra()
            inputSpectra.setSpectraData(currentSpectraData)
            spectrumIdentification.getInputSpectra.add(inputSpectra)
            break()
          }
        }
      }

      msiSearch.searchSettings.seqDatabases.foreach( seqDb => {
        val searchDbRef = searchDbRefBySeqDbIds(seqDb.id)
        val searchDb = searchDatabases.filter(_.getId.equals(searchDbRef)).head //should be one !
        val ref = new SearchDatabaseRef()
        ref.setSearchDatabase(searchDb)
        spectrumIdentification.getSearchDatabaseRef.add(ref)
      })

      analyseColl.getSpectrumIdentification.add(spectrumIdentification)

    })

    if(rsmFromValidation) {
      val calDate = Calendar.getInstance() //Use first MSI date for ProteinDetection... FIXME
      calDate.setTime(msiSearches(0).date)

      val protDetection = new ProteinDetection
      protDetection.setId(proteinDetectionId)
      protDetection.setProteinDetectionProtocol(analyseProtocolColl.getProteinDetectionProtocol)
      //Create fake ProteinDetectionList to get ref only
      val protDetectionList = new ProteinDetectionList()
      protDetectionList.setId(proteinDetectionListId)
      protDetection.setProteinDetectionList(protDetectionList)
      protDetection.setActivityDate(calDate)
      val inputSpecIdents = new InputSpectrumIdentifications
      inputSpecIdents.setSpectrumIdentificationList(spectrumIdentificationList)
      protDetection.getInputSpectrumIdentifications.add(inputSpecIdents)
      analyseColl.setProteinDetection(protDetection)
    }
    analyseColl
  }

  protected def _buildAnalysisSoftwareList(): AnalysisSoftwareList = {

    val anlSoftList = new AnalysisSoftwareList()
    val anlSoftArray = anlSoftList.getAnalysisSoftware
    anlSoftArray.add(mzidSearchEngine)
    anlSoftArray.add(prolineSuite)
    anlSoftList
  }

  protected def _buildProvider(): Provider = {

    Provider(
      "PROVIDER",
      ContactRole(
        submitterContact,
        Role(PsiCvParam(PsiMs.Researcher))
      )
    )
  }


  protected def getPepByPepId(exportValidated: Boolean): mutable.HashMap[Long, MzIdPeptide] = {

    val tmpMzidPepByPepId = new mutable.HashMap[Long, MzIdPeptide]()
    val peptides = if(!exportValidated) rs.peptides else rsm.peptideInstances.map(_.peptide)

    // TODO: handle substitutions
    for (peptide <- peptides) {

      val mzidPep = new MzIdPeptide()
      mzidPep.setId(peptidePrefix + peptide.id)
      mzidPep.setPeptideSequence(peptide.sequence)

      /* <SubstitutionModification location="6" originalResidue="X" replacementResidue="Q" /> => Can't define for all matches
      mzidPep.getSubstitutionModification */
      val seqLength = peptide.sequence.length

      val modList = mzidPep.getModification
      for (pepPtm <- peptide.ptms) {

        val mod = new Modification()
        mod.setLocation(if (pepPtm.isCTerm) seqLength + 1 else pepPtm.seqPosition)
        mod.setAvgMassDelta(pepPtm.averageMass)
        mod.setMonoisotopicMassDelta(pepPtm.monoMass)

        val residueChar = pepPtm.definition.residue
        if (residueChar != 0) {
          val residueAsStr = residueChar.toString
          mod.getResidues.add(residueAsStr)
        }


        mod.getCvParam.add(
          BuildCvParam("UNIMOD:" + unimodIdByPtmId(pepPtm.definition.ptmId), pepPtm.definition.names.shortName, ControlledVocabulary.unimodCV)
        )
        modList.add(mod)
      }

      tmpMzidPepByPepId += peptide.id -> mzidPep
    }

    tmpMzidPepByPepId
  }


  protected def _buildSequenceCollection(pepEvidencesByPepId: mutable.HashMap[Long, ArrayBuffer[PeptideEvidence]] ,mzidPepByPepId: mutable.HashMap[Long, MzIdPeptide], exportValidated : Boolean ): SequenceCollection = {

    val seqCollec = new SequenceCollection()
    val seqList: util.List[DBSequence] = seqCollec.getDBSequence
    val proteinMatches = if (exportValidated) {
      val ids = rsm.proteinSets.flatMap(_.getSameSetProteinMatchIds)
      ids.map(rsProtMatchesById(_))
    } else {
      rs.proteinMatches
    }

    val bioSeqById = _getBioSequenceById(proteinMatches)
    proteinMatches.foreach( pMatch => {
      val seqDb = rsDbSeqByProtMatchId(pMatch.id)
      val bioSeqId =pMatch.getProteinId()
      if( bioSeqById.contains(bioSeqId)) {
        val protSeq = bioSeqById(bioSeqId).sequence.get
        seqDb.setSeq(protSeq)
        seqDb.setLength(protSeq.length)
      } else {
        logger.warn("No Protein Sequence for Protein Match ID "+pMatch.id+" accession "+pMatch.accession)
      }
      seqList.add(seqDb)
    })


    val pepList = seqCollec.getPeptide
    mzidPepByPepId.values.foreach(pepList.add(_))

    val pepEvList = seqCollec.getPeptideEvidence
    for (pepEvs <- pepEvidencesByPepId.values; pepEv <- pepEvs) {
      pepEvList.add(pepEv)
    }

    seqCollec
  }

  protected def _buildAnalysisProtocolCollection(rsmFromValidation: Boolean): AnalysisProtocolCollection = {
    val anlProtoCollec = new AnalysisProtocolCollection()

    // Set the spectrum identification protocol
    val specIdentProtoList = anlProtoCollec.getSpectrumIdentificationProtocol
    specIdentProtoList.addAll(JavaConversions.asJavaCollection(this._buildSpectrumIdentificationProtocols()))

    // Set the protein detection protocol
    if(rsmFromValidation)
      anlProtoCollec.setProteinDetectionProtocol(this._buildProteinDetectionProtocol())

    anlProtoCollec
  }

  // Return a SpectrumIdentificationProtocol for each SearchSettings
  protected def _buildSpectrumIdentificationProtocols(): Seq[SpectrumIdentificationProtocol] = {

    val spectIdentProtocols = new ArrayBuffer[SpectrumIdentificationProtocol]()
    msiSearches.foreach(msiSearch => {

      val specIdentProto = new SpectrumIdentificationProtocol()

      /* exemple :
      <AdditionalSearchParams>
        <userParam name="Mascot User Comment" value="1807BetaCa QX AutoProline HF1_004789.raw (1807BetaCas_H2-1)" />
        <userParam name="Mascot Instrument Name" value="ESI FTMS HCD" />
        <cvParam accession="MS:1001211" name="parent mass type mono"    cvRef="PSI-MS" />
        <cvParam accession="MS:1001259" name="param: immonium ion" cvRef="PSI-MS" />
        <cvParam accession="MS:1001108" name="param: a ion" cvRef="PSI-MS" />
        <cvParam accession="MS:1001146" name="param: a ion-NH3" cvRef="PSI-MS" />
        <cvParam accession="MS:1001148" name="param: a ion-H2O" cvRef="PSI-MS" />
        <cvParam accession="MS:1001118" name="param: b ion" cvRef="PSI-MS" />
        <cvParam accession="MS:1001149" name="param: b ion-NH3" cvRef="PSI-MS" />
        <cvParam accession="MS:1001150" name="param: b ion-H2O" cvRef="PSI-MS" />
        <cvParam accession="MS:1001262" name="param: y ion" cvRef="PSI-MS" />
        <cvParam accession="MS:1001151" name="param: y ion-NH3" cvRef="PSI-MS" />
        <cvParam accession="MS:1001152" name="param: y ion-H2O" cvRef="PSI-MS" />
        <cvParam accession="MS:1001406" name="param: internal yb ion" cvRef="PSI-MS" />
        <cvParam accession="MS:1001407" name="param: internal ya ion" cvRef="PSI-MS" />
      </AdditionalSearchParams>
       */
      val paramListBuilder = List.newBuilder[AbstractParam]
      if(msiSearch.searchSettings.fragmentationRuleSet.isDefined) {
        paramListBuilder += BuildUserParam("Instrument_Fragmentation_Name", msiSearch.searchSettings.fragmentationRuleSet.get.name, "xsd:string")
        msiSearch.searchSettings.fragmentationRuleSet.get.fragmentationRules.foreach {
          case requirement: FragmentIonRequirement =>
            val psiFragIonParam = FragmentationIonParamPSIValue.getPsiMsforFragmentationType(requirement.ionType.ionSeries.toString)
            paramListBuilder += PsiCvParam(psiFragIonParam)
          case _ =>
        }
      }

      val paramList = paramListBuilder.result()
      if(paramList.nonEmpty)
        specIdentProto.setAdditionalSearchParams(BuildParamList(paramListBuilder.result()))

      //specIdentProto.setAdditionalSearchParams(value)
      specIdentProto.setId(spectrumIdentificationProtocolIdPrefix + msiSearch.id)
      specIdentProto.setAnalysisSoftware(mzidSearchEngine)
      //specIdentProto.setDatabaseFilters(value)
      //specIdentProto.setDatabaseTranslation(value)

      specIdentProto.setEnzymes(this._buildEnzymes(msiSearch))
      val searchSettings = msiSearch.searchSettings
      if (searchSettings.msmsSearchSettings.isDefined) {
        specIdentProto.setFragmentTolerance(this._buildFragmentTolerance(msiSearch).get)
        specIdentProto.setSearchType(
          makeParamGroup(PsiCvParam(PsiMs.MsmsSearch))
        )
      } else {
        specIdentProto.setSearchType(
          makeParamGroup(PsiCvParam(PsiMs.PmfSearch))
        )
      }

      val massTbl = this._buildMassTable()
      massTbl.setId(massTbl.getId+"_"+msiSearch.id)
      specIdentProto.getMassTable.add(massTbl)


      if (searchSettings.fixedPtmDefs.length > 0 || searchSettings.variablePtmDefs.length > 0)
        specIdentProto.setModificationParams(this._buildModificationParams(msiSearch))

      specIdentProto.setParentTolerance(this._buildParentTolerance(msiSearch))
      specIdentProto.setThreshold(
        Threshold(List(PsiCvParam(PsiMs.MascotSigThreshold, "0.05"))) //TODO Change !!
      )

      spectIdentProtocols += specIdentProto
    })
    spectIdentProtocols
  }

  protected def _buildEnzymes(msiSearch: MSISearch): Enzymes = {
    val enzymes = new Enzymes()
    val enzList = enzymes.getEnzyme
    var foundOneEnzyme= false
    var nbMissCleavage = -1

    for (usedEnz <- msiSearch.searchSettings.usedEnzymes) {
      if(!foundOneEnzyme)
        foundOneEnzyme = true
      if (!usedEnz.isIndependant) enzymes.setIndependent(false)

      val enz = new MzIdEnzyme()
      enz.setId(enzymePrefix + msiSearch.id)

      val psiEnzyme =  EnzymePSIValue.getPsiMsforEnzyme(usedEnz.name)
      if(psiEnzyme!=null)
        enz.setEnzymeName(BuildParamList(List(PsiCvParam(psiEnzyme))))
      else {
        enz.setEnzymeName(BuildParamList(List(PsiCvParam(EnzymePSIValue.withName("none").psiMod))))
      }

      enz.setMissedCleavages(msiSearch.searchSettings.maxMissedCleavages)

      enz.setSemiSpecific(usedEnz.isSemiSpecific)
      usedEnz.cleavageRegexp.foreach(r => enz.setSiteRegexp(r))
      //enz.setCTermGain(value)
      //enz.setMinDistance(value)
      //enz.setNTermGain(value)

      enzList.add(enz)
    }

    // FIXME: enzyme should be defined in the MSI-DB
    if (!foundOneEnzyme) {

      val enz = new MzIdEnzyme()
      enz.setId(enzymePrefix+"None")
      enz.setEnzymeName(BuildParamList(List(PsiCvParam(EnzymePSIValue.withName("none").psiMod))))
      enz.setSemiSpecific(false)
      enzList.add(enz)
    }

    enzymes
  }

  protected def _buildFragmentTolerance(msiSearch : MSISearch): Option[Tolerance] = {

    import fr.profi.util.ms.MassTolUnit

    val ms2Settings = msiSearch.searchSettings.msmsSearchSettings
    if (ms2Settings.isEmpty) return None

    val tolUnit = MassTolUnit.string2unit(ms2Settings.get.ms2ErrorTolUnit)
    val tolUnitCv = tolUnit match {
      case MassTolUnit.Da  => UnitTerm.Dalton
      case MassTolUnit.mmu => UnitTerm.Mmu
      case MassTolUnit.PPM => UnitTerm.PartsPerMillion
    }

    val tolValue = ms2Settings.get.ms2ErrorTol.toString
    val tol = new Tolerance()
    tol.getCvParam.add(
      PsiCvParam(
        PsiMs.SearchTolerancePlusValue,
        tolUnitCv,
        tolValue
      )
    )
    tol.getCvParam.add(
      PsiCvParam(
        PsiMs.SearchToleranceMinusValue,
        tolUnitCv,
        tolValue
      )
    )

    Some(tol)
  }

  protected def _buildParentTolerance(msiSearch : MSISearch): Tolerance = {

    val searchSettings = msiSearch.searchSettings

    // TODO: use MassTolUnit enum when also used in MSIdb (check PPM case)
    val tolUnit = searchSettings.ms1ErrorTolUnit match {
      case "ppm" => UnitTerm.PartsPerMillion //CvParamUnit("UO:0000169","parts per million")
      case "Da"  => UnitTerm.Dalton //CvParamUnit("UO:0000221","dalton")
    }

    val tolValue = searchSettings.ms1ErrorTol.toString
    val tol = new Tolerance()
    tol.getCvParam.add(
      PsiCvParam(
        PsiMs.SearchTolerancePlusValue,
        tolUnit,
        tolValue
      )
    )
    tol.getCvParam.add(
      PsiCvParam(
        PsiMs.SearchToleranceMinusValue,
        tolUnit,
        tolValue
      )
    )

    tol
  }

  protected def _buildMassTable(): MassTable = {

    val massTable = new MassTable()
    massTable.setId("MT")
    massTable.getMsLevel.add(1)
    massTable.getMsLevel.add(2)

    val residueList = massTable.getResidue
    val residues = List(
      Residue(code = "A", mass = 71.037114f),
      Residue(code = "C", mass = 103.009185f),
      Residue(code = "D", mass = 115.026943f),
      Residue(code = "E", mass = 129.042593f),
      Residue(code = "F", mass = 147.068414f),
      Residue(code = "G", mass = 57.021464f),
      Residue(code = "H", mass = 137.058912f),
      Residue(code = "I", mass = 113.084064f),
      Residue(code = "K", mass = 128.094963f),
      Residue(code = "L", mass = 113.084064f),
      Residue(code = "M", mass = 131.040485f),
      Residue(code = "N", mass = 114.042927f),
      Residue(code = "P", mass = 97.052764f),
      Residue(code = "Q", mass = 128.058578f),
      Residue(code = "R", mass = 156.101111f),
      Residue(code = "S", mass = 87.032028f),
      Residue(code = "T", mass = 101.047679f),
      Residue(code = "U", mass = 150.95363f),
      Residue(code = "V", mass = 99.068414f),
      Residue(code = "W", mass = 186.079313f),
      Residue(code = "Y", mass = 163.063329f)
    )
    residues.foreach(residueList.add(_))

    val ambiguousResList = massTable.getAmbiguousResidue
    val ambiguousResidues = List(
      AmbiguousResidue("B", List(PsiCvParam(PsiMs.AlternateSingleLetterCodes, "D N"))),
      AmbiguousResidue("Z", List(PsiCvParam(PsiMs.AlternateSingleLetterCodes, "E Q"))),
      AmbiguousResidue("X", List(PsiCvParam(PsiMs.AlternateSingleLetterCodes, "A C D E F G H I K L M N O P Q R S T U V W Y")))
    )
    ambiguousResidues.foreach(ambiguousResList.add(_))

    massTable
  }

  protected def _buildModificationParams(msiSearch : MSISearch): ModificationParams = {

    val modParams = new ModificationParams()
    val modParamsList = modParams.getSearchModification
    val searchSettings = msiSearch.searchSettings

    def ptm2mod(ptmDef: PtmDefinition, isFixed: Boolean): SearchModification = {

      val mod = new SearchModification()
      mod.setFixedMod(isFixed)
      mod.setMassDelta(ptmDef.precursorDelta.monoMass.toFloat) // TODO: add to usedPtm ? ... pourquoi ?

      val modRes = if (ptmDef.residue == '\0') "." else ptmDef.residue.toString
      mod.getResidues.add(modRes)

      mod.getCvParam.add(
        BuildCvParam("UNIMOD:" + unimodIdByPtmId(ptmDef.ptmId), ptmDef.names.shortName, ControlledVocabulary.unimodCV)
      )

      //mod.getSpecificityRules()

      mod
    }

    for (fixedPtm <- searchSettings.fixedPtmDefs) {
      modParamsList.add(ptm2mod(fixedPtm, isFixed = true))
    }
    for (variablePtm <- searchSettings.variablePtmDefs) {
      modParamsList.add(ptm2mod(variablePtm, isFixed = false))
    }

    modParams
  }

   protected def _buildProteinDetectionProtocol(): ProteinDetectionProtocol = {

    var msmsThPsiCVParam = PsiCvParam(PsiMs.MascotMinMSMSThreshold, "0")
    val mascotProp = if(rs.properties.isDefined && rs.properties.get.mascotImportProperties.isDefined) {
      rs.properties.get.mascotImportProperties.get
    } else new MascotImportProperties()

    if (mascotProp.ionsScoreCutoff.isDefined && mascotProp.ionsScoreCutoff.get >0)
        msmsThPsiCVParam = PsiCvParam(PsiMs.MascotMinMSMSThreshold, mascotProp.ionsScoreCutoff.get.toString)

    val scoreType = rsm.peptideSets(0).scoreType
    val protScoreTypePsiCVParam =  if(scoreType.equals(Scoring.Type.MASCOT_STANDARD_SCORE.toString)) {
      PsiCvParam(PsiMs.MascotProteinScoringMethod, "Standard")
    } else {
      PsiCvParam(PsiMs.MascotProteinScoringMethod, "MudPIT")
    }

    val pdp = new ProteinDetectionProtocol()
    pdp.setId(protDetectProtoId)
    pdp.setAnalysisSoftware(prolineSuite)
//    pdp.setThreshold(BuildParamList(List(PsiCvParam("MS:1001494", "no threshold"))))
    pdp.setThreshold(BuildParamList(List(msmsThPsiCVParam)))

    //Get filters used for RSM root of RS Merge
    val appliedPSMFilters = ResultSummaryFilterBuilder.buildPeptideMatchFilters(rsm)
    val appliedProteinSetFilters = ResultSummaryFilterBuilder.buildProteinSetsFilters(rsm)
    val allParams = List.newBuilder[AbstractParam]
    val psmFDR = ResultSummaryFilterBuilder.getPeptideExpectedFDR(rsm)
    val protSetFDR = ResultSummaryFilterBuilder.getProteinSetExpectedFDR(rsm)
    if(psmFDR.isDefined)
      allParams += PsiCvParam(PsiMs.PSMFDRThreshold, psmFDR.get.toString)
    if(protSetFDR.isDefined)
      allParams += PsiCvParam(PsiMs.ProtFDRThreshold, protSetFDR.get.toString)

    allParams += protScoreTypePsiCVParam
    appliedPSMFilters.foreach(elem => {
      val thr = if(elem.getThresholdValue() != null) elem.getThresholdValue().toString else null
      allParams += BuildUserParam("PSM "+elem.filterDescription, thr, null)
    })

    appliedProteinSetFilters.foreach(filter =>{
      val thr = if(filter.getThresholdValue() != null) filter.getThresholdValue().toString else null
      allParams += BuildUserParam("ProteinSet  "+filter.filterDescription,thr, null)
    })

    pdp.setAnalysisParams(BuildParamList(allParams.result()))


    pdp
  }

//  protected def _buildProlineProteinDetectionProtocol(): ProteinDetectionProtocol = {
//
//    val pdp = new ProteinDetectionProtocol()
//    pdp.setId(protDetectProtoId)
//    pdp.setAnalysisSoftware(mzidSearchEngine)
//    pdp.setThreshold(BuildParamList(List(PsiCvParam(PsiMs.NoThreshold))))
//
//    pdp.setAnalysisParams(
//      BuildParamList(
//        List(
//          BuildUserParam("expected_peptides_fdr", "5.0", "xsd:float"),
//          BuildUserParam("expected_protein_sets_fdr", "1.0", "xsd:float")
//        )
//      )
//    )
//
//    pdp
//  }


  protected def _buildInputs(): Inputs = {
    val inputs = new Inputs()

    inputs.getSearchDatabase.addAll(JavaConversions.asJavaCollection(searchDatabases))

    msiSearches.foreach( msiSearch => {
      val sourceFile = new SourceFile()
      sourceFile.setId("SF_" + msiSearch.id)
      sourceFile.setLocation(
        msiSearch.resultFileDirectory + "/" + msiSearch.resultFileName
      )

      val softName = msiSearch.searchSettings.softwareName
      val psiFormatVal = softName.toLowerCase match {
        case "mascot" => PsiMs.MascotDATFormat
        case "xtandem" => PsiMs.XTandemXmlFormat
        case "omssa" => PsiMs.OMSSAXmlFormat
        case _ => PsiMs.MascotDATFormat
      }

      val sourcefileFormat = new FileFormat()
      sourcefileFormat.setCvParam(PsiCvParam(psiFormatVal))
      sourceFile.setFileFormat(sourcefileFormat)

      inputs.getSourceFile.add(sourceFile)

      // FIXME: export peaklist children if they are defined (OM update needed)
      val specData = new SpectraData()
      specData.setId(spectraDataIdPrefix+ msiSearch.peakList.id)
      specData.setLocation(msiSearch.peakList.path)
      //specData.setExternalFormatDocumentation(value)

      // FIXME: retrieve the right file format
      val peaklistSoftName = msiSearch.peakList.peaklistSoftware.name
      val psiPeaklistFormatVal = if(peaklistSoftName.toLowerCase().startsWith("extract_msn")) { PsiMs.DTAFormat }
        else if(peaklistSoftName.toLowerCase().startsWith("Mascot Distiller")) { PsiMs.MascotMGFFormat }
        else  { PsiMs.MascotMGFFormat } // Ask user ?


      val specFileFormat = new FileFormat()
      specFileFormat.setCvParam(PsiCvParam(psiPeaklistFormatVal))
      specData.setFileFormat(specFileFormat)

      // FIXME: retrieve the right format
      val specIDFormat = new SpectrumIDFormat()
      specIDFormat.setCvParam(
        PsiCvParam(PsiMs.MultiplePeakListNativeIDFormat)
      )
      specData.setSpectrumIDFormat(specIDFormat)

      inputs.getSpectraData.add(specData)
    })
    inputs
  }

  protected def _buildFragmentationTable(): FragmentationTable = {

    FragmentationTable(
      List(
        Measure(measureMZId, List(PsiCvParam(PsiMs.ProductIonMz))),
        Measure(measureIntensityId, List(PsiCvParam(PsiMs.ProductIonIntensity))),
        Measure(measureErrId, List(PsiCvParam.withAlternativeUnit(PsiMs.ProductIonMzError, PsiMs.Mz)))
      )
    )
  }

}