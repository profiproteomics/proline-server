package fr.proline.module.exporter.mzidentml

import java.io.FileWriter
import java.io.StringReader
import java.io.ByteArrayInputStream
import javax.xml.bind.JAXBElement
import javax.xml.bind.JAXBContext
import javax.xml.bind.Unmarshaller
import javax.xml.transform.sax.SAXSource
import javax.xml.parsers.SAXParserFactory
import scala.collection.mutable.{ArrayBuffer,HashMap}
import org.xml.sax.InputSource
import com.weiglewilczek.slf4s.Logging

import uk.ac.ebi.jmzidml.model.mzidml._
import uk.ac.ebi.jmzidml.xml.io.MzIdentMLMarshaller
import uk.ac.ebi.jmzidml.xml.jaxb.marshaller.MarshallerFactory
import uk.ac.ebi.jmzidml.xml.jaxb.unmarshaller.UnmarshallerFactory
import uk.ac.ebi.jmzidml.model.utils.ModelConstants

import fr.proline.core.om.model.msi.MSISearch
import fr.proline.core.om.model.msi.ResultSet
import fr.proline.core.om.model.msi.ResultSummary
import fr.proline.core.om.model.msi.Ms2Query
import fr.proline.core.om.model.msi.PtmDefinition
import fr.proline.core.om.model.msi.SequenceMatch

object MzIdExporter {
  
  protected val _xmlReader = {
    // Create a new XML parser
    val saxFactory = SAXParserFactory.newInstance()
    saxFactory.setNamespaceAware(true)
    val saxParser = saxFactory.newSAXParser()
    saxParser.getXMLReader()
  }
  
  protected val _unmarshaller = {
    JAXBContext.newInstance(ModelConstants.PACKAGE).createUnmarshaller()
  }
  
  /** Unmarshall the desired object by providing an XML string */
  def unmarshallXmlString[T]( _class: Class[T], xmlStr: String ): T = {
    //val inputSource = new InputSource(new StringReader(xmlStr))
    //val saxSource = new SAXSource(_xmlReader, inputSource)
    //this._unmarshaller.unmarshal(saxSource, _class).getValue()
    //this._unmarshaller.unmarshal(new StringReader(xmlStr)).asInstanceOf[JAXBElement[T]].getValue()
    
    val input = new ByteArrayInputStream(xmlStr.getBytes())
    val jaxbObject = this._unmarshaller.unmarshal(input)
    jaxbObject.asInstanceOf[JAXBElement[T]].getValue()
  }
  
}

/**
 * @author David Bouyssie
 *
 */
class MzIdExporter(
  rsm: ResultSummary,
  unimodIdByPtmId: Map[Int,Int],
  spectrumNumberById: Map[Int,Int]
) extends ParamMaker with Logging {
  
  // Retrieve some proline objects
  val rs = rsm.resultSet.get
  val msiSearch = rs.msiSearch
  val searchSettings = msiSearch.searchSettings
  val pepById = rs.peptideById
  //println("nb peptides="+pepById.size)
  val protMatchById = rs.proteinMatchById
  //val pepMatchById = rs.peptideMatchById
  val pepInstByPepId = Map() ++ rsm.peptideInstances.map( pi => pi.peptide.id -> pi )
  val validPepMatchIdSet = rsm.peptideInstances.flatMap( _.peptideMatchIds ).toSet
  
  // Set the main mzIdentML Element ids
  val spectraDataId = "SD_" + msiSearch.peakList.id
  val searchDatabaseIds = searchSettings.seqDatabases.map( "SDB_" + _.id )
  val protDetectProtoId = "PDP_MascotParser_1" // "PDP_Proline"
  
  val profiOrganization = {
    val org = new Organization()
    org.setId("ORG_PROFI")
    org.setName("Proteomics French Infrastructure")
    org
  }
  
  val profiContact = {
    
    val proFI = new Person()
    proFI.setId("PERSON_DOC_OWNER")
    
    val affiliation = new Affiliation()
    affiliation.setOrganization(profiOrganization)
    proFI.getAffiliation().add(affiliation)
    
    proFI
  }
  
  lazy val matrixScienceOrganization = {
    val org = new Organization()
    org.setId("ORG_MSL")
    org.setName("Matrix Science Limited")
    org
  }
  
  lazy val matrixScienceContact = {
    val proFI = new Person()
    proFI.setId("Matrix_Science_Limited")
    val affiliation = new Affiliation()
    affiliation.setOrganization(matrixScienceOrganization)
    proFI.getAffiliation().add(affiliation)
    proFI
  }
  
  val auditCollection = {
    /*
    <AuditCollection>
      <Person id="">
        <Affiliation organization_ref="ORG_MSL"/>
      </Person>
      <Person id="PERSON_DOC_OWNER" firstName="" lastName="" >
        <Affiliation organization_ref="ORG_DOC_OWNER"/>
      </Person>
      <Organization id="ORG_MSL" name=""  />
      <Organization id="ORG_DOC_OWNER" />
    </AuditCollection>"""
      
    */
    
    val auditCollec = new AuditCollection()
    auditCollec.getPerson().add(matrixScienceContact)
    auditCollec.getPerson().add(profiContact)
    auditCollec.getOrganization().add(matrixScienceOrganization)
    auditCollec.getOrganization().add(profiOrganization)
    auditCollec
    
  }
  lazy val searchDatabase: SearchDatabase = {
    
    //TODO: export all seq dbs
    val seqDb = searchSettings.seqDatabases(0)
    val seqDbFile = new java.io.File(seqDb.filePath)
    
    val fileFormat = FileFormat( CvParam("MS:1001348", "FASTA format", psiCV ) )
    
    val sd = new SearchDatabase()
    sd.setId( searchDatabaseIds(0) )
    sd.setName(seqDbFile.getName()) // the file name
    sd.setDatabaseName( makeUserParamGroup(seqDb.name) )
    sd.setNumDatabaseSequences(seqDb.sequencesCount)
    //sd.setNumResidues(value)
    seqDb.releaseDate.foreach( relDate => {
      val releaseDateCal = java.util.Calendar.getInstance()
      releaseDateCal.setTime(relDate)
      sd.setReleaseDate(releaseDateCal)
    })
    sd.setVersion(seqDb.version)
    //sd.setExternalFormatDocumentation(value)
    sd.setFileFormat(fileFormat)
    sd.setLocation(seqDb.filePath)
    
    sd
  }
  
  val cvList = new CvList()
  cvList.getCv().add(psiCV)
  cvList.getCv().add(unimodCV)
  cvList.getCv().add(unitCV)
  
  def exportResultSummary( filePath: String ) {
    
    /*int cvCount = -1;
    int analysisSoftwareCount = -1;
    int auditCount = -1;
    int dbSequenceCount = -1;
    int peptideEvidencePeptideCount = -1;*/
    
    //URL xmlFileURL = MzIdentMLMarshallerTest.class.getClassLoader().getResource("Mascot_MSMS_example.mzid");
    //assertNotNull(xmlFileURL);
    //MzIdentMLUnmarshaller unmarshaller = new MzIdentMLUnmarshaller(xmlFileURL);
    //assertNotNull(unmarshaller);

    val mzIdMarshaller = new MzIdentMLMarshaller()
    var writer = new FileWriter(filePath)
    
    // mzIdentML
    //     cvList
    //     AnalysisSoftwareList
    //     Provider
    //     AuditCollection
    //     AnalysisSampleCollection
    //     SequenceCollection
    //     AnalysisCollection
    //     AnalysisProtocolCollection
    //     DataCollection
    //         Inputs
    //         AnalysisData
    //             SpectrumIdentificationList
    //             ProteinDetectionList
    //         /AnalysisData
    //     /DataCollection
    //     BibliographicReference
    // /mzIdentML


    // Note: writing of '\n' characters is optional and only for readability of the produced XML document
    // Also note: since the XML is produced in individual parts, the overall formatting of the document
    //            is not as nice as it would be when marshalling the whole structure at once.

    // XML header
    writer.write(mzIdMarshaller.createXmlHeader() + "\n")
    
    // mzIdentML start tag
    writer.write(mzIdMarshaller.createMzIdentMLStartTag("12345") + "\n")

    /*val cvListAsStr =
    """<cvList>
      <cv id="PSI-MS" fullName="Proteomics Standards Initiative Mass Spectrometry Vocabularies"  uri="http://psidev.cvs.sourceforge.net/viewvc/*checkout*/psidev/psi/psi-ms/mzML/controlledVocabulary/psi-ms.obo" version="2.25.0"></cv>
      <cv id="UNIMOD" fullName="UNIMOD"        uri="http://www.unimod.org/obo/unimod.obo"></cv>
      <cv id="UO"     fullName="UNIT-ONTOLOGY" uri="http://obo.cvs.sourceforge.net/*checkout*/obo/obo/ontology/phenotype/unit.obo"></cv>
    </cvList>"""
    writer.write(cvListAsStr + "\n")*/
    
    mzIdMarshaller.marshal(this.cvList, writer)
    writer.write("\n")

    mzIdMarshaller.marshal( this._buildAnalysisSoftwareList(), writer)
    writer.write("\n")

    // TODO: ask for this information
    mzIdMarshaller.marshal(this._buildProvider(), writer)
    writer.write("\n")
    
    mzIdMarshaller.marshal(auditCollection, writer)
    writer.write("\n")

    // TODO: export the samples
    //AnalysisSampleCollection analysisSampleCollection = unmarshaller.unmarshal(MzIdentMLElement.AnalysisSampleCollection.getXpath());
    //mzIdMarshaller.marshal(analysisSampleCollection, writer);
    //writer.write("\n");
    
    mzIdMarshaller.marshal(this._buildSequenceCollection(), writer)
    writer.write("\n");

    //val analysisCollection = this._buildAnalysisCollection()
    //mzIdMarshaller.marshal(analysisCollection, writer)
    
    // TODO: export all seq dbs
    // TODO: set the dates
    // Note: the jmzidentml API doesn't allows to set the refs of the spectrumIdentificationProtocol/List
    //       but need the real objects which will be problematic for large datasets
    val analysisCollecNode =
    <AnalysisCollection>
      <SpectrumIdentification id="SI" spectrumIdentificationProtocol_ref="SIP"  spectrumIdentificationList_ref="SIL_1" activityDate="2008-06-23T19:39:34">
        <InputSpectra spectraData_ref={ spectraDataId } />
        <SearchDatabaseRef searchDatabase_ref={ searchDatabaseIds(0) } />
      </SpectrumIdentification>
      <ProteinDetection id="PD_1" proteinDetectionProtocol_ref={protDetectProtoId} proteinDetectionList_ref="PDL_1" activityDate="2009-08-18T18:03:11">
        <InputSpectrumIdentifications spectrumIdentificationList_ref="SIL_1"/>
      </ProteinDetection>
    </AnalysisCollection>
      
    writer.write(analysisCollecNode + "\n")

    mzIdMarshaller.marshal(this._buildAnalysisProtocolCollection(), writer)
    writer.write("\n")
    
    writer.write(mzIdMarshaller.createDataCollectionStartTag() + "\n")

    val inputs = this._buildInputs()
    mzIdMarshaller.marshal(inputs, writer)
    writer.write("\n")

    writer.write(mzIdMarshaller.createAnalysisDataStartTag() + "\n")

    writer.write(
      mzIdMarshaller.createSpectrumIdentificationListStartTag(
        "SIL_1", null, msiSearch.searchedSequencesCount.toLong
      ) + "\n"
    )

    //FragmentationTable table = unmarshaller.unmarshal(MzIdentMLElement.FragmentationTable.getXpath());
    mzIdMarshaller.marshal(this._buildFragmentationTable(), writer)
    writer.write("\n")
    
    val spectraData = inputs.getSpectraData().get(0)
    
    val pepMatchesByMsQueryId = rs.peptideMatches.groupBy( _.getMs2Query )    
    for( (msQuery,pepMatches) <- pepMatchesByMsQueryId ) {
      
      // TODO: find what to do whith PMF data
      val ms2Query = msQuery.asInstanceOf[Ms2Query]
      val specIndex = spectrumNumberById(ms2Query.spectrumId) - 1
      
      val specIdentRes = new SpectrumIdentificationResult()
      specIdentRes.setId("SIR_"+ms2Query.id)
      specIdentRes.setSpectraData(spectraData)
      specIdentRes.setSpectrumID("index="+specIndex)
      
      val specIdentItemList = specIdentRes.getSpectrumIdentificationItem()
      
      val charge = msQuery.charge
      val expMoz = msQuery.moz
      
      for( pepMatch <- pepMatches ) {
        
        val pepId = pepMatch.peptide.id
        
        val specIdentItem = new SpectrumIdentificationItem()
        specIdentItem.setId( "SII_"+ pepMatch.id )
        specIdentItem.setCalculatedMassToCharge(
          expMoz - pepMatch.deltaMoz
        )
        //specIdentItem.setCalculatedPI(value) // for PMF ???
        specIdentItem.setChargeState(charge)
        specIdentItem.setExperimentalMassToCharge(expMoz)
        //specIdentItem.setFragmentation(value)
        //specIdentItem.setMassTable(massTable) // when is it needed ?
        specIdentItem.setPassThreshold(validPepMatchIdSet.contains(pepMatch.id))
        specIdentItem.setPeptide(mzidPepByPepId(pepId))
        specIdentItem.setRank(pepMatch.rank)
        //specIdentItem.setSample(sample)
        
        val psmParams = specIdentItem.getCvParam()
        // TODO: export spectum title
        
/*
<cvParam accession="MS:1001171" name="mascot:score" cvRef="PSI-MS" value="13.49"/>
<cvParam accession="MS:1001172" name="mascot:expectation value" cvRef="PSI-MS"
<cvParam accession="MS:1001363" name="peptide unique to one protein" cvRef="PSI-MS"/>
<cvParam accession="MS:1001371" name="mascot:identity threshold" cvRef="PSI-MS" value="43"/>
<cvParam accession="MS:1001370" name="mascot:homology threshold" cvRef="PSI-MS" value="26"/>
<cvParam accession="MS:1001030" name="number of peptide seqs compared to each spectrum"
<cvParam accession="MS:1000796" name="spectrum title" cvRef="PSI-MS"
*/
        // Try to retrieve a peptide instance
        pepInstByPepId.get(pepId).foreach { pepInst =>
          if( pepInst.proteinMatchesCount == 1 )
            psmParams.add(CvParam("MS:1001363", "peptide unique to one protein", psiCV )) 
          else
            psmParams.add(CvParam("MS:1001175", "peptide shared in multiple proteins", psiCV )) 
        }
        
        val mascotPropsOpt = pepMatch.properties.get.mascotProperties
        if( mascotPropsOpt != None ) { 
          
          val mascotProps = mascotPropsOpt.get
          
          psmParams.add(
            CvParam("MS:1001171", "mascot:score", psiCV, pepMatch.score.toString )
          )
          
          psmParams.add(
            CvParam(
              "MS:1001172","mascot:expectation value", psiCV, 
              mascotProps.getExpectationValue.toString
            )
          )          
          // TODO: export other thresholds
          
          val pepEvList = specIdentItem.getPeptideEvidenceRef()
          for( pepEv <- pepEvidencesByPepId(pepId) ) {
            val pepEvRef = new PeptideEvidenceRef()
            pepEvRef.setPeptideEvidence(pepEv)
            pepEvList.add(pepEvRef)
          }
          
        }
        
        specIdentItemList.add(specIdentItem)
      }
      
      mzIdMarshaller.marshal(specIdentRes, writer)
      writer.write("\n")
    }

    // End of spectrum identification list
    writer.write(mzIdMarshaller.createSpectrumIdentificationListClosingTag() + "\n")
    
    // Start of protein detection list
    writer.write(mzIdMarshaller.createProteinDetectionListStartTag("PDL_1", null) + "\n")
    
    for( protSet <- rsm.proteinSets ) {
      
      //val pepInstances = protSet.peptideSet.getPeptideInstances
      val confidentPepCount = protSet.peptideSet.items.length
      
      val protAmbGroup = new ProteinAmbiguityGroup()
      protAmbGroup.setId("PAG_"+protSet.id)
      
      val protDetectHypoList = protAmbGroup.getProteinDetectionHypothesis()
      
      for( protMatchId <- protSet.proteinMatchIds ) {
        
        val protMatch = protMatchById(protMatchId)
        
        val protDetectHypo = new ProteinDetectionHypothesis()
        protDetectHypo.setId("PDH_"+ protMatchId)
        protDetectHypo.setDBSequence(dbSeqByProtMatchId(protMatchId))
        protDetectHypo.setPassThreshold(protSet.isValidated)
        
        val pepHypoList = protDetectHypo.getPeptideHypothesis()
        //val pepSeqs = new ArrayBuffer[String](protMatch.sequenceMatches.length)
        val confidentPepSeqs = new ArrayBuffer[String](protMatch.sequenceMatches.length)
        
        for( seqMatch <- protMatch.sequenceMatches ) {
           // pepInst <- protSet.peptideSet.getPeptideInstances; pepEv <- pepEvidencesByPepId(pepInst.peptide.id) ) {
          
          // Retrieve the validated peptide
          pepInstByPepId.get(seqMatch.getPeptideId).foreach { pepInstance =>
            
            //pepSeqs += pepById(seqMatch.getPeptideId).sequence
            
            val pepHypo = new PeptideHypothesis()
            pepHypo.setPeptideEvidence(pepEvidenceBySeqMatch(seqMatch))          
            val specIdentItemRefList = pepHypo.getSpectrumIdentificationItemRef()
            
            // Retrieve peptide matches of the validated peptide
            for( pepMatchId <- pepInstance.getPeptideMatchIds ) {
              val specIdentItemRef = new SpectrumIdentificationItemRef()
              specIdentItemRef.setSpectrumIdentificationItemRef("SII_"+ pepMatchId)
              specIdentItemRefList.add( specIdentItemRef )
            }
            
            confidentPepSeqs += pepInstance.peptide.sequence
            
            pepHypoList.add(pepHypo)
          }
        }
        
        val protHypoCvParams = protDetectHypo.getCvParam()
        protHypoCvParams.add(
          CvParam("MS:1001171", "mascot:score", psiCV, protMatch.score.toString)
        )
        protHypoCvParams.add(
          CvParam("MS:1001093", "sequence coverage", psiCV, protMatch.coverage.toString)
        )
        /*protHypoCvParams.add(
          makeCvParam("MS:1001097", "distinct peptide sequences", psiCV, pepSeqs.distinct.length.toString)
        )*/
        protHypoCvParams.add(
          CvParam("MS:1001098", "confident distinct peptide sequences", psiCV, confidentPepSeqs.distinct.length.toString)
        )
        
        protDetectHypoList.add(protDetectHypo)
      }
      
      mzIdMarshaller.marshal(protAmbGroup, writer)
      writer.write("\n")
    }

    // End of protein detection list
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
    
    /*File outputFile = new File("output.xml");
    
    unmarshaller = new MzIdentMLUnmarshaller(outputFile);
    MzIdentML mzIdentMl = unmarshaller.unmarshal(MzIdentMLElement.MzIdentML);    

    assertTrue(mzIdentMl.getId().equals("12345"));
    assertTrue(cvCount >= 0);
    assertTrue(mzIdentMl.getCvList().getCv().size() == cvCount);
    assertTrue(analysisSoftwareCount >= 0);
    assertTrue(mzIdentMl.getAnalysisSoftwareList().getAnalysisSoftware().size() == analysisSoftwareCount);
    assertTrue(mzIdentMl.getProvider().getId().equals("PROVIDER"));
    assertTrue(auditCount >= 0);
    assertTrue(mzIdentMl.getAuditCollection().getPersonOrOrganization().size() == auditCount);
    assertTrue(dbSequenceCount >= 0);
    assertTrue(mzIdentMl.getSequenceCollection().getDBSequence().size() == dbSequenceCount);
    assertTrue(peptideEvidencePeptideCount >= 0);
    assertTrue(mzIdentMl.getSequenceCollection().getPeptideEvidence().size() == peptideEvidencePeptideCount);
            */

  }
  
  def exportResultSet( filePath: String ) {
    
  }
  
  
  val mzidSearchEngine = {
    val softName = searchSettings.softwareName
    
    val searchEngine = new AnalysisSoftware()
    searchEngine.setId("AS_"+softName)
    searchEngine.setName(softName)
    searchEngine.setVersion(searchSettings.softwareVersion)
    
    val softCvParamByName = Map( "Mascot" -> CvParam("MS:1001207","Mascot",psiCV),
                                 "OMSSA" -> CvParam("MS:1001475","OMSSA",psiCV)
                               )
    val softCvParamOpt = softCvParamByName.get(softName)
    require( softCvParamOpt != None, "unsupported search engine '"+softName+"'")
    
    searchEngine.setSoftwareName( makeParamGroup(softCvParamOpt.get) )
      
    val contactRole = ContactRole(
      matrixScienceContact, // TODO: retrieve the right one
      Role( CvParam("MS:1001267", "software vendor", psiCV) )
    )
    contactRole.setContact(matrixScienceContact) // TODO: retrieve the right one
    
    searchEngine.setContactRole( contactRole )
    
    
    searchEngine
  }
  
  protected def _buildAnalysisSoftwareList(): AnalysisSoftwareList = {
    
    val anlSoftList = new AnalysisSoftwareList()
    val anlSoftArray = anlSoftList.getAnalysisSoftware()
    anlSoftArray.add(mzidSearchEngine)
    
    // TODO: add Proline AnalysisSoftware
    
    anlSoftList
  }
  
  // TODO: query for this value in the exporter constructor
  protected def _buildProvider(): Provider = {
    Provider(
      "PROVIDER",
      ContactRole(
        profiContact,
        Role(CvParam("MS:1001268","programmer",psiCV))
      )
    )
  }
  
  protected lazy val dbSeqByProtMatchId = {
    
    val tmpDbSeqByProtMatchId = new HashMap[Int,DBSequence]()
    for( protMatch <- rs.proteinMatches ) {
      
      val seq = new DBSequence()
      seq.setId(protMatch.accession)
      seq.setAccession(protMatch.accession)
      //seq.setName(value) // protein identifier
      //seq.setSeq(value)
      //seq.setLength(value)
      seq.setSearchDatabase(searchDatabase) // TODO: retrieve the right one
      
      val descParam = CvParam("MS:1001088", "protein description", psiCV, protMatch.description)
      seq.getCvParam().add(descParam)
      
      tmpDbSeqByProtMatchId += protMatch.id -> seq
    }
    
    tmpDbSeqByProtMatchId
  }
  
  protected lazy val mzidPepByPepId = {
    
    val tmpMzidPepByPepId = new HashMap[Int,Peptide]()
    
    // TODO: handle substitutions
    for( peptide <- rs.peptides ) {
      
      val mzidPep = new Peptide()
      mzidPep.setId("peptide_"+peptide.id)
      mzidPep.setPeptideSequence(peptide.sequence)
      
      val seqLength = peptide.sequence.length
      
      val modList = mzidPep.getModification()
      for( pepPtm <- peptide.ptms ) {
        
        val mod = new Modification()
        mod.setLocation( if( pepPtm.isCTerm ) seqLength + 1 else pepPtm.seqPosition )
        mod.setAvgMassDelta(pepPtm.averageMass)
        mod.setMonoisotopicMassDelta(pepPtm.monoMass)
        mod.getResidues().add(pepPtm.definition.residue.toString)
        
        modList.add(mod)
      }
      
      tmpMzidPepByPepId += peptide.id -> mzidPep
    }
    
    tmpMzidPepByPepId
  }
 
  lazy val pepEvidencesByPepId =  {
    if( _pepEvidencesByPepId.size == 0 ) _initPepEvidencesMaps
    _pepEvidencesByPepId
  }
  lazy val pepEvidenceBySeqMatch =  {
    if( _pepEvidenceBySeqMatch.size == 0 ) _initPepEvidencesMaps
    _pepEvidenceBySeqMatch
  }
  
  private val _pepEvidencesByPepId = new HashMap[Int,ArrayBuffer[PeptideEvidence]]
  private val _pepEvidenceBySeqMatch = new HashMap[SequenceMatch,PeptideEvidence]
  
  /*private def _makeSeqMatchUniqueKey( protMatchId: Int, seqMatch: SequenceMatch ) = {
    List(protMatchId,seqMatch.getPeptideId,seqMatch.start,seqMatch.end).mkString("_")
  }*/
  private def _initPepEvidencesMaps = {
    
    for( protMatch <- rs.proteinMatches; seqMatch <- protMatch.sequenceMatches ) {
      val pepId = seqMatch.getPeptideId
      
      if( mzidPepByPepId.contains(pepId) ) {
        
        val peptide = mzidPepByPepId(pepId)
      
        val pepEv = new PeptideEvidence()
        pepEv.setId( List("PE",protMatch.id,pepId,seqMatch.start,seqMatch.end).mkString("_") )
        pepEv.setDBSequence(dbSeqByProtMatchId(protMatch.id))
        pepEv.setEnd(seqMatch.end)
        //pepEv.setFrame(value)
        pepEv.setIsDecoy(seqMatch.isDecoy)
        pepEv.setPeptide(peptide)
        pepEv.setPost(seqMatch.residueAfter.toString) // TODO: check the syntax when Cterm
        pepEv.setPre(seqMatch.residueBefore.toString) // TODO: check the syntax when Nterm
        pepEv.setStart(seqMatch.start)
        //pepEv.setTranslationTable(translationTable)
  
        _pepEvidencesByPepId.getOrElseUpdate(pepId, new ArrayBuffer[PeptideEvidence] ) += pepEv
        _pepEvidenceBySeqMatch += seqMatch -> pepEv
      }
    }
    
  }
  
  protected def _buildSequenceCollection(): SequenceCollection = {
    
    val seqCollec = new SequenceCollection()
    val seqList = seqCollec.getDBSequence()
    
    dbSeqByProtMatchId.values.foreach( seqList.add(_) )
    
    val pepList = seqCollec.getPeptide()
    mzidPepByPepId.values.foreach( pepList.add(_) )
    
    val pepEvList = seqCollec.getPeptideEvidence()
    for( pepEvs <- pepEvidencesByPepId.values; pepEv <- pepEvs ) {
      pepEvList.add(pepEv)
    }
    
    seqCollec
    
  }
  
  protected def _buildAnalysisProtocolCollection(): AnalysisProtocolCollection = {
    val anlProtoCollec = new AnalysisProtocolCollection()
    
    // Set the spectrum identification protocol
    val specIdentProtoList = anlProtoCollec.getSpectrumIdentificationProtocol()
    specIdentProtoList.add(this._buildSpectrumIdentificationProtocol)
    
    // Set the protein detection protocol
    anlProtoCollec.setProteinDetectionProtocol(this._buildMascotProteinDetectionProtocol)
    
    anlProtoCollec
  }
  
  protected def _buildSpectrumIdentificationProtocol(): SpectrumIdentificationProtocol = {
    
    val specIdentProto = new SpectrumIdentificationProtocol()
    // TODO: load the fragmentation rules from the UDS-DB
    //specIdentProto.setAdditionalSearchParams(value)
    specIdentProto.setId("SIP")
    specIdentProto.setAnalysisSoftware(mzidSearchEngine)
    //specIdentProto.setDatabaseFilters(value)
    //specIdentProto.setDatabaseTranslation(value)
    specIdentProto.setEnzymes(this._buildEnzymes())
    
    if( searchSettings.msmsSearchSettings != None ) {
      specIdentProto.setFragmentTolerance(this._buildFragmentTolerance().get)
      specIdentProto.setSearchType(
        makeParamGroup(CvParam("MS:1001083", "ms-ms search", psiCV))
      )
    } else {
      specIdentProto.setSearchType(
        makeParamGroup(CvParam("MS:1001081", "pmf search", psiCV))
      )
    }
    
    specIdentProto.getMassTable().add(this._buildMassTable)
    
    if( searchSettings.fixedPtmDefs.length > 0 || searchSettings.variablePtmDefs.length > 0 )
      specIdentProto.setModificationParams(this._buildModificationParams())
    
    specIdentProto.setParentTolerance(this._buildParentTolerance())    
    specIdentProto.setThreshold(
      Threshold(List(CvParam("MS:1001316","mascot:SigThreshold",psiCV,"0.05")))
    )
    
    specIdentProto
  }
  
  protected def _buildEnzymes(): Enzymes = {
    val enzymes = new Enzymes()
    val enzList = enzymes.getEnzyme()
    
    for( usedEnz <- searchSettings.usedEnzymes ) {
      if( usedEnz.isIndependant == false ) enzymes.setIndependent(false)
      
      val enz = new Enzyme()
      enz.setId("ENZ_"+usedEnz.id)
      //enz.setCTermGain(value)
      // FIXME: create an enzyme name mapping
      enz.setEnzymeName(ParamList(List(CvParam("MS:1001251", "Trypsin", psiCV))))
      //enz.setMinDistance(value)
      enz.setMissedCleavages(searchSettings.maxMissedCleavages) // TODO: put column in used enzyme
      //enz.setNTermGain(value)
      enz.setSemiSpecific(usedEnz.isSemiSpecific)
      usedEnz.cleavageRegexp.foreach( r => enz.setSiteRegexp(r) )
      
      enzList.add(enz)
    }
    
    // FIXME: enzyme should be defined in the MSI-DB
    if( searchSettings.usedEnzymes.length == 0 ) {
      
      val enz = new Enzyme()
      enz.setId("ENZ_0")
      enz.setCTermGain("OH")
      enz.setEnzymeName(ParamList(List(CvParam("MS:1001251", "Trypsin", psiCV))))
      //enz.setMinDistance(value)
      //enz.setMissedCleavages(value)
      enz.setNTermGain("H")
      enz.setSemiSpecific(false)
      enz.setSiteRegexp("""(?<=[KR])(?!P)""")
      
      enzList.add( enz )
    }
    
    enzymes
  }
  
  protected def _buildFragmentTolerance(): Option[Tolerance] = {
    
    val ms2Settings = searchSettings.msmsSearchSettings
    if( ms2Settings == None) return None
    
    // TODO: use MassTolUnit enum when also used in MSIdb (check PPM case)
    val tolUnit = ms2Settings.get.ms2ErrorTolUnit match {
      case "ppm" => CvParamUnit("UO:0000169","parts per million")
      case "Da" => CvParamUnit("UO:0000221","dalton")
    }
    
    val tolValue = ms2Settings.get.ms2ErrorTol.toString
    val tol = new Tolerance()
    tol.getCvParam().add(
      CvParam(
        "MS:1001412", "search tolerance plus value", psiCV,
        tolUnit.accession, tolUnit.name, value = tolValue
      )
    )
    tol.getCvParam().add(
      CvParam(
        "MS:1001413", "search tolerance minus value", psiCV,
        tolUnit.accession, tolUnit.name, value = tolValue
      )
    )
    
    Some(tol)
  }
  
  protected def _buildParentTolerance(): Tolerance = {
    
    // TODO: use MassTolUnit enum when also used in MSIdb (check PPM case)
    val tolUnit = searchSettings.ms1ErrorTolUnit match {
      case "ppm" => CvParamUnit("UO:0000169","parts per million")
      case "Da" => CvParamUnit("UO:0000221","dalton")
    }
    
    val tolValue = searchSettings.ms1ErrorTol.toString
    val tol = new Tolerance()
    tol.getCvParam().add(
      CvParam(
        "MS:1001412", "search tolerance plus value", psiCV,
        tolUnit.accession, tolUnit.name, value = tolValue
      )
    )
    tol.getCvParam().add(
      CvParam(
        "MS:1001413", "search tolerance minus value", psiCV,
        tolUnit.accession, tolUnit.name, value = tolValue
      )
    )
    
    tol
  }
  
  protected def _buildMassTable(): MassTable = {
    
    val massTable = new MassTable()
    massTable.setId("MT")
    massTable.getMsLevel().add(1)
    massTable.getMsLevel().add(2)
    
    val residueList = massTable.getResidue()
    val residues = List(
      Residue( code="A", mass=71.037114f ),
      Residue( code="C", mass=103.009185f ),
      Residue( code="D", mass=115.026943f ),
      Residue( code="E", mass=129.042593f ),
      Residue( code="F", mass=147.068414f ),
      Residue( code="G", mass=57.021464f ),
      Residue( code="H", mass=137.058912f ),
      Residue( code="I", mass=113.084064f ),
      Residue( code="K", mass=128.094963f ),
      Residue( code="L", mass=113.084064f ),
      Residue( code="M", mass=131.040485f ),
      Residue( code="N", mass=114.042927f ),
      Residue( code="P", mass=97.052764f ),
      Residue( code="Q", mass=128.058578f ),
      Residue( code="R", mass=156.101111f ),
      Residue( code="S", mass=87.032028f ),
      Residue( code="T", mass=101.047679f ),
      Residue( code="U", mass=150.95363f ),
      Residue( code="V", mass=99.068414f ),
      Residue( code="W", mass=186.079313f ),
      Residue( code="Y", mass=163.063329f )
    )
    residues.foreach( residueList.add(_) )
    
    val ambiguousResList = massTable.getAmbiguousResidue()
    val ambiguousResidues = List(
      AmbiguousResidue("B",List(CvParam("MS:1001360","alternate single letter codes",psiCV,"D N"))),
      AmbiguousResidue("Z",List(CvParam("MS:1001360","alternate single letter codes",psiCV,"E Q"))),
      AmbiguousResidue("X",List(CvParam("MS:1001360","alternate single letter codes",
                                        psiCV,"A C D E F G H I K L M N O P Q R S T U V W Y")))
    )
    ambiguousResidues.foreach( ambiguousResList.add(_) )
    
    massTable
  }
  
  protected def _buildModificationParams(): ModificationParams = {
    
    val modParams = new ModificationParams()
    val modParamsList = modParams.getSearchModification()
    
    def ptm2mod(ptmDef: PtmDefinition, isFixed: Boolean): SearchModification = {
      
      val mod = new SearchModification()
      mod.setFixedMod(isFixed)
      mod.setMassDelta(ptmDef.precursorDelta.monoMass.toFloat) // TODO: add to usedPtm ?
      
      val modRes = if( ptmDef.residue == '\0' ) "." else ptmDef.residue.toString
      mod.getResidues().add(modRes)
      
      mod.getCvParam().add(
        CvParam("UNIMOD:"+unimodIdByPtmId(ptmDef.ptmId), ptmDef.names.shortName, unimodCV )
      )
      
      //mod.getSpecificityRules()
      
      mod
    }
    
    // FIXME: do we need to group ptmDefs by PTM ???
    
    for( fixedPtm <- searchSettings.fixedPtmDefs ) {
      modParamsList.add(ptm2mod(fixedPtm,true))
    }
    for( variablePtm <- searchSettings.variablePtmDefs ) {
      modParamsList.add(ptm2mod(variablePtm,false))
    }
    
    modParams
  }
  
  
  // FIXME: put correct values there
  protected def _buildMascotProteinDetectionProtocol(): ProteinDetectionProtocol = {
    
    val pdp = new ProteinDetectionProtocol()
    pdp.setId(protDetectProtoId)
    pdp.setAnalysisSoftware(mzidSearchEngine)
    pdp.setThreshold(ParamList(List(CvParam("MS:1001494","no threshold",psiCV))))
    
    pdp.setAnalysisParams(
      ParamList(
        List(
          CvParam( "MS:1001316", "mascot:SigThreshold", psiCV, "0.05" ),
          CvParam( "MS:1001317", "mascot:MaxProteinHits", psiCV, "Auto" ),
          CvParam( "MS:1001318", "mascot:ProteinScoringMethod", psiCV, "MudPIT" ),
          CvParam( "MS:1001319", "mascot:MinMSMSThreshold", psiCV, "0" ),
          CvParam( "MS:1001320", "mascot:ShowHomologousProteinsWithSamePeptides", psiCV, "1" ),
          CvParam( "MS:1001321", "mascot:ShowHomologousProteinsWithSubsetOfPeptides", psiCV, "1" ),
          CvParam( "MS:1001322", "mascot:RequireBoldRed", psiCV, "0" ),
          CvParam( "MS:1001323", "mascot:UseUnigeneClustering", psiCV, "false" ),
          CvParam( "MS:1001324", "mascot:IncludeErrorTolerantMatches", psiCV, "1" ),
          CvParam( "MS:1001325", "mascot:ShowDecoyMatches", psiCV, "0" )
        )
      )
    )
    
    pdp
  }
  
  protected def _buildProlineProteinDetectionProtocol(): ProteinDetectionProtocol = {
    
    val pdp = new ProteinDetectionProtocol()
    pdp.setId(protDetectProtoId)
    pdp.setAnalysisSoftware(mzidSearchEngine)
    pdp.setThreshold(ParamList(List(CvParam("MS:1001494","no threshold",psiCV))))
    
    pdp.setAnalysisParams(
      ParamList(
        List(
          UserParam( "expected_peptides_fdr", "5.0","xsd:float" ),
          UserParam( "expected_protein_sets_fdr", "1.0","xsd:float" )
        )
      )
    )
    
    pdp
  }
  
  protected def _buildInputs(): Inputs = {
    val inputs = new Inputs()
    
    // FXIME: add all search databases
    inputs.getSearchDatabase().add(searchDatabase)
    
    val sourceFile = new SourceFile()
    sourceFile.setId("SF_"+msiSearch.id)
    sourceFile.setLocation(
      msiSearch.resultFileDirectory + "/" + msiSearch.resultFileName
    )
    //sourceFile.setExternalFormatDocumentation(value)
    
    // FIXME: retrieve the right file format
    val sourcefileFormat = new FileFormat()
    sourcefileFormat.setCvParam(CvParam("MS:1001199", "Mascot DAT file", psiCV))
    sourceFile.setFileFormat(sourcefileFormat)
    
    inputs.getSourceFile().add(sourceFile)
    
    // FIXME: export peaklist children if they are defined (OM update needed)
    val specData = new SpectraData()
    specData.setId("SD_"+msiSearch.peakList.id)
    specData.setLocation(msiSearch.peakList.path)
    //specData.setExternalFormatDocumentation(value)
    
    // FIXME: retrieve the right file format
    val specFileFormat = new FileFormat()
    specFileFormat.setCvParam(CvParam("MS:1001062", "Mascot MGF file", psiCV))
    specData.setFileFormat(specFileFormat)
    
    // FIXME: retrieve the right format
    val specIDFormat = new SpectrumIDFormat()
    specIDFormat.setCvParam(
      CvParam("MS:1000774", "multiple peak list nativeID format", psiCV)
    )
    specData.setSpectrumIDFormat(specIDFormat)
    
    inputs.getSpectraData().add(specData)
    
    inputs
  }
  
  protected def _buildFragmentationTable(): FragmentationTable = {

    FragmentationTable(
      List(
        Measure("m_mz", List(CvParam("MS:1001225","product ion m/z",psiCV)) ),
        Measure("m_intensity", List(CvParam("MS:1001226","product ion intensity",psiCV)) ),
        Measure("m_error", List(CvParam("MS:1001227","product ion m/z error",psiCV,"MS:1000040","m/z")) )
      )
    )
    
  }

  
}