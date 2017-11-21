package fr.proline.module.exporter.pridexml

import java.io.FileWriter
import java.math.BigInteger
import java.nio.ByteBuffer
import java.nio.ByteOrder
import scala.Array.canBuildFrom
import scala.annotation.migration
import scala.collection.mutable.HashMap
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.bytes.doublesToBytes
import fr.profi.util.serialization.CustomDoubleJacksonSerializer
import fr.profi.util.serialization.ProfiJSMSerialization
import fr.proline.context.IExecutionContext
import fr.proline.core.dal.DoJDBCReturningWork
import fr.proline.core.dal.DoJDBCWork
import fr.proline.core.dal.helper.MsiDbHelper
import fr.proline.core.dal.tables.SelectQueryBuilder.any2ClauseAdd
import fr.proline.core.dal.tables.SelectQueryBuilder2
import fr.proline.core.dal.tables.msi.MsiDbObjectTreeTable
import fr.proline.core.dal.tables.msi.MsiDbPeptideMatchObjectTreeMapTable
import fr.proline.core.om.model.msi.LocatedPtm
import fr.proline.core.om.model.msi.ProteinMatch
import fr.proline.core.om.model.msi.ResultSummary
import fr.proline.core.om.model.msi.Spectrum
import fr.proline.core.om.model.msi.SpectrumMatch
import fr.proline.core.om.provider.msi.IMSISearchProvider
import fr.proline.core.om.provider.msi.impl.SQLMsiSearchProvider
import fr.proline.core.om.provider.msi.impl.SQLProteinMatchProvider
import fr.proline.core.om.provider.msi.impl.SQLSpectrumProvider
import uk.ac.ebi.pride.jaxb.model.Admin
import uk.ac.ebi.pride.jaxb.model.Data
import uk.ac.ebi.pride.jaxb.model.Description
import uk.ac.ebi.pride.jaxb.model.GelFreeIdentification
import uk.ac.ebi.pride.jaxb.model.IntenArrayBinary
import uk.ac.ebi.pride.jaxb.model.ModificationItem
import uk.ac.ebi.pride.jaxb.model.MzArrayBinary
import uk.ac.ebi.pride.jaxb.model.Param
import uk.ac.ebi.pride.jaxb.model.PeptideItem
import uk.ac.ebi.pride.jaxb.model.Precursor
import uk.ac.ebi.pride.jaxb.model.PrecursorList
import uk.ac.ebi.pride.jaxb.model.SpectrumDesc
import uk.ac.ebi.pride.jaxb.model.SpectrumInstrument
import uk.ac.ebi.pride.jaxb.model.SpectrumSettings
import uk.ac.ebi.pride.jaxb.utils.PrideModelUtils
import uk.ac.ebi.pride.jaxb.xml.marshaller.PrideXmlMarshaller
import uk.ac.ebi.pride.jaxb.xml.marshaller.PrideXmlMarshallerFactory
import org.apache.commons.lang3.StringUtils
import uk.ac.ebi.pride.jaxb.model.FragmentIon
import fr.proline.core.om.model.msi.FragmentMatch
import uk.ac.ebi.pride.jaxb.model.SampleDescription
import java.io.IOException
import uk.ac.ebi.pride.jaxb.model.Protocol
import uk.ac.ebi.pride.jaxb.model.ProtocolSteps


object PrideExporter {

  protected val marshaller = {
    val pof = PrideXmlMarshallerFactory.getInstance();
    val marshaller = pof.initializeMarshaller();
    marshaller
  }

}

/**
 * @author Christophe Bruley
 *
 */
class PrideExporter(
  rsm: ResultSummary,
  unimodIdByPtmId: Map[Long, Long],
  execCtx: IExecutionContext) extends LazyLogging {

  // inner object def
  object CustomSerializer extends ProfiJSMSerialization with CustomDoubleJacksonSerializer
  private class SeqDatabaseRecord(val id: Long, val name: String, val version: String, val filePath: String)

  // -- variables constructions

  val msiSQLCtx = execCtx.getMSIDbConnectionContext()

  // Retrieve some proline objects
  val rs = rsm.resultSet.get
  val rsId = rs.id

  // --- Get MSISearch info
  val optionalMsiSearch = rs.msiSearch
  val assMsiSearch = if ((optionalMsiSearch != null) && optionalMsiSearch.isDefined) optionalMsiSearch else {
    val msiHelper = new MsiDbHelper(msiSQLCtx)
    val childMsiSearches = msiHelper.getResultSetsMsiSearchIds(Seq(rsId), true)
    if (childMsiSearches.length > 0) {
      getMISearchProvider(execCtx).getMSISearch(childMsiSearches(0))
    } else
      None
  }
  require((assMsiSearch != null) && assMsiSearch.isDefined, "ResultSet #" + rsId + " has no associated MSI Search")
  val metadataBuilder = PrideMetadataBuilder(assMsiSearch.get)

  // Get validated data to export
  private val validPeptideInstance = rsm.peptideInstances.filter(_.validatedProteinSetsCount > 0)
  private val pepInstByPepId = Map() ++ validPeptideInstance.map(pi => pi.peptide.id -> pi)
  private val validPepMatchIdSet = validPeptideInstance.map(_.bestPeptideMatchId).toSet

  //Spectrum - SpectrumMatches informations
  private val spectrumProvider = new SQLSpectrumProvider(msiSQLCtx)

  private val spectrumIdByPepMatchId = {
    val map = new HashMap[Long, Long]
    DoJDBCWork.withEzDBC(msiSQLCtx, { ezDBC =>
      ezDBC.selectAndProcess("SELECT peptide_match.id, ms_query.spectrum_id FROM peptide_match, ms_query WHERE peptide_match.ms_query_id = ms_query.id and peptide_match.id IN (" + validPepMatchIdSet.mkString(",") + ")") { r =>
        val pepMatchId: Long = r.nextLong
        val spectrumId: Long = r.nextLong
        map += (pepMatchId -> spectrumId)
      }
    })
    Map() ++ map
  }

  private val spectrumMatchByPeptideMatchId = {
    val spectrumMatchesByPeptMatchId = new HashMap[Long, SpectrumMatch]
    DoJDBCReturningWork.withEzDBC(msiSQLCtx, { msiEzDBC =>
      val pepMatches = validPepMatchIdSet.mkString(",")
      val pepMatchSpectrumMatchQuery = new SelectQueryBuilder2(MsiDbPeptideMatchObjectTreeMapTable, MsiDbObjectTreeTable).mkSelectQuery((pmT, pmC, otT, otC) =>
        List(pmT.PEPTIDE_MATCH_ID, otT.CLOB_DATA) -> " WHERE " ~ pmT.OBJECT_TREE_ID ~ "=" ~ otT.ID ~ " AND " ~ pmT.SCHEMA_NAME ~ "= 'peptide_match.spectrum_match' AND " ~
          pmT.PEPTIDE_MATCH_ID ~ " IN (" ~ validPepMatchIdSet.mkString(",") ~ ")")

      msiEzDBC.selectAndProcess(pepMatchSpectrumMatchQuery) { r =>
        val id = r.nextLong
        val spectrumMatch = CustomSerializer.deserialize[SpectrumMatch](r.nextString)
        spectrumMatchesByPeptMatchId += (id -> spectrumMatch)
      }
    })
    Map() ++ spectrumMatchesByPeptMatchId
  }

  private val proteinMatchesById = {
    val provider = new SQLProteinMatchProvider(msiSQLCtx)
    val proteinMatchesById = new HashMap[Long, ProteinMatch]
    Map() ++ provider.getResultSummariesProteinMatches(Array(rsm.id)).map(pm => (pm.id -> pm))
  }

  private val seqDbById = {
    val seqDbById = new HashMap[Long, SeqDatabaseRecord]
    DoJDBCReturningWork.withEzDBC(msiSQLCtx, { msiEzDBC =>
      msiEzDBC.selectAndProcess("SELECT id, name, version, fasta_file_path FROM seq_database") { r =>
        val id = r.nextLong
        seqDbById += (id -> new SeqDatabaseRecord(id, r.nextString, r.nextString, r.nextString))
      }
    })
    Map() ++ seqDbById
  }

  private def getMISearchProvider(execContext: IExecutionContext): IMSISearchProvider = {

    new SQLMsiSearchProvider(udsDbCtx = execContext.getUDSDbConnectionContext, msiDbCtx = execContext.getMSIDbConnectionContext,
      psDbCtx = execContext.getPSDbConnectionContext)
  }

  /**
   *  Export a RSM to specified file in Pride format.
   *  Extra information could be specified using extraDataMap parameter.
   *  
   *  Accepted parameters are :
   *  - exp_title : Title of the experiment described in the pride XML file
   *  - exp_short_label : Short label of the experiment described in the pride XML file
   * ---- 
   *  - protocol_description : String representation of the full Protocol PRIDE Section (! mandatory in final doc). The string should be something like 
   *        <Protocol>
   *    	   <ProtocolName>TO BE REPLACED !!!! </ProtocolName>
   *    	   <ProtocolSteps>
   *             <StepDescription><cvParam cvLabel="PRIDE" accession="PRIDE:0000025" name="Reduction" value="DTT" /></StepDescription>
   *              ...
   *           </ProtocolSteps>
   *    	</Protocol>
   * OR
   *  - protocol_name : Name of the described protocol
   *  - protocol_steps : List of CVParam (as XML String: <cvparam .../>) corresponding to each protocol steps (Reduction, ...) 
   * ----
   *  - contact_name : Name of the contact for the data exported to the Pride XML file   (! mandatory in final doc)
   *  - contact_institution : Institution to which belongs the Contact (! mandatory in final doc)
   *  - sample_name : Name of the sample analysed (! mandatory in final doc)
   *  - sample_desc : Description of the sample 
   *  - sample_additional : List of CVParam (as XML String: <cvparam .../>) for sample definition : Species, Tissues, Cell Localization 
   *  - project_name 
   *  - instrument_name : or will be read from MSISearch
   *  
   *  
   */ 
  def exportResultSummary(filePath: String, extraDataMap: Map[String, Object]) {

    val marshaller = PrideExporter.marshaller

    //export experimentalData
    exportExperimentalData(filePath,  marshaller, extraDataMap)

    exportMzData(filePath, marshaller, extraDataMap)

    exportIdentification(filePath, marshaller, extraDataMap)

    exportFooter(filePath, marshaller, extraDataMap)

    logger.info("Pride XML done")
  }

  protected def exportFooter(filePath: String, marshaller: PrideXmlMarshaller, extraDataMap: Map[String, Object]) {
    var writer : FileWriter = null
    try {      
	    writer = new FileWriter(filePath,true)
	
	    writer.write("\n<" + PrideSchemaConstants.ADDITIONAL_NODE + ">\n")
	    val version = new  fr.proline.module.exporter.api.Version()
	    marshaller.marshall(CvParam(PrideSchemaConstants.PRIDE_CV_SOFT_ACC, PrideSchemaConstants.PRIDE_CV_SOFT_NAME, "Proline (Pride Exporter "+version.getVersion+")"), writer)
	    // TODO ? Get project 
	    if(extraDataMap.get("project_name").isDefined) {
	       writer.write("\n")
	      marshaller.marshall(CvParam(PrideSchemaConstants.PRIDE_CV_PROJECT_ACC, PrideSchemaConstants.PRIDE_CV_PROJECT_NAME,  extraDataMap("project_name").toString()), writer)	      
       }
    	    
	    writer.write("\n</" + PrideSchemaConstants.ADDITIONAL_NODE + ">")
	    writer.write("\n</" + PrideSchemaConstants.EXP_NODE + ">")
	    writer.write("\n</" + PrideSchemaConstants.EXP_COLL_NODE + ">")
	    writer.flush()
	    writer.close()
    } catch {
      case ioe : IOException =>{
    	  try {
    		  if(writer != null){
	        	writer.flush()
	        	writer.close()
	      	}
    	  }catch { case e: Exception =>
    	    
    	  }  finally {
    	    throw new Exception(" Error writing Pride Footer "+ioe.getMessage()) 
    	  }
      } 
    }
  }
  
  /**
   * Export to specified file header and experiments information such as project name and protocol description
   * if defined in extraDataMap (entry protocol_description).
   */
  protected def exportExperimentalData(filePath: String, marshaller: PrideXmlMarshaller, extraDataMap: Map[String, Object]) {
   

    val title : String = if(extraDataMap.get("exp_title").isDefined) {extraDataMap("exp_title").toString() } else rs.name
    val shortLabel : String = if(extraDataMap.get("exp_short_label").isDefined) {extraDataMap("exp_short_label").toString() } else rs.name
    require(!StringUtils.isEmpty(title) && !StringUtils.isEmpty(shortLabel), " Did not found title and short Label or search result name !")
 
    var writer : FileWriter  = null
    try {
	    writer = new FileWriter(filePath)
	    
	    // XML header
	    writer.write("""<?xml version="1.0" encoding="utf-8" ?>""")
	    writer.write('\n')
	    writer.write("<" + PrideSchemaConstants.EXP_COLL_NODE + " " + PrideSchemaConstants.SCHEMA_VERSION_ATTR + "= \"" + PrideSchemaConstants.PRIDE_SCHEMA_VERSION + "\" >")
	    writer.write('\n')
	    writer.write("<" + PrideSchemaConstants.EXP_NODE + ">")
	    writer.write('\n')
	    writer.write("<" + PrideSchemaConstants.EXP_TITLE_NODE + ">" + title + "</" + PrideSchemaConstants.EXP_TITLE_NODE + ">")
	    writer.write('\n')
	    writer.write("<" + PrideSchemaConstants.EXP_SHORT_LABEL_NODE + ">" + shortLabel + "</" + PrideSchemaConstants.EXP_SHORT_LABEL_NODE + ">")
	    writer.write('\n')
	
	    if (extraDataMap.get("protocol_description").isDefined){
	      writer.write(extraDataMap("protocol_description").toString())
	      writer.write('\n')
	
	    } else if(extraDataMap.get("protocol_name").isDefined){
      
	      val protocol = new Protocol()
	      protocol.setProtocolName(extraDataMap("protocol_name").asInstanceOf[String])
	      val steps = new ProtocolSteps
	      steps.getStepDescription().add(0, new Param())
	      if(extraDataMap.get("protocol_steps").isDefined){
	        
	         val addValues : List[String] = extraDataMap("protocol_steps").asInstanceOf[List[String]]
    		 addValues.foreach( nextEntry => {
    			 steps.getStepDescription().get(0).getCvParam().add(0,CvParam(nextEntry))
    		 })
	      }
	      protocol.setProtocolSteps(steps)
	      marshaller.marshall(protocol, writer)
                 
         
	    } else {
	      val protocolSample = 
	        <Protocol>
    		  <ProtocolName>TO BE REPLACED !!!! </ProtocolName>
    		  <ProtocolSteps></ProtocolSteps>
    	</Protocol>
	      writer.write(protocolSample + "\n")
	    }
	    writer.flush()
	    writer.close()
    } catch {
      case ioe : IOException =>{
    	  try {
    		  if(writer != null){
	        	writer.flush()
	        	writer.close()
	      	}
    	  }catch { case e: Exception =>
    	    
    	  }  finally {
    	    throw new Exception(" Error writing Pride Experimental Data  "+ioe.getMessage()) 
    	  }
      } 
    }
  }

  protected def exportMzData(filePath: String, marshaller: PrideXmlMarshaller, extraDataMap: Map[String, Object]) {
    var writer : FileWriter  = null
    try {

	    writer = new FileWriter(filePath,true)
	
	    writer.write("""<mzData version="1.05" accessionNumber="0">""")
	    writer.write('\n')
	    marshaller.marshall(CvLookup.msLookup, writer)
	    writer.write('\n')
	    marshaller.marshall(CvLookup.prideLookup, writer)
	    writer.write('\n')
	    marshaller.marshall(_buildMzDataDescription(extraDataMap), writer)
	    
	    val spectra = spectrumProvider.getSpectra(spectrumIdByPepMatchId.values.toSeq)
	
	    var sb = new StringBuilder
	    sb.append("\n<spectrumList count=\"").append(spectra.length).append("\">\n")
	    writer.write(sb.toString)
	
	    for (spectrum <- spectra) {
	      marshaller.marshall(_buildSpectrum(spectrum), writer)
	      writer.write('\n')
	    }
	    writer.write("\n</spectrumList>")
	    logger.debug("SpectrumList writing done")
	    writer.write("\n</mzData>\n")
	    writer.flush()
	    writer.close()
    } catch {
    	case ioe : IOException =>{
    		try {
    		  if(writer != null){
	        	writer.flush()
	        	writer.close()
	      	}
    	  }catch { case e: Exception =>
    	    
    	  }  finally {
    	    throw new Exception(" Error writing Pride MzData  "+ioe.getMessage()) 
    	  }
      } 
    }
  }

  private def _buildMzDataDescription(extraDataMap: Map[String, Object]): Description = {    
    
    require(extraDataMap.contains("sample_name"),"Sample name should be specified for Pride XML file")
    
    val contact = metadataBuilder.getContact        
    require( ( (!StringUtils.isEmpty(contact.getInstitution())) || extraDataMap.contains("contact_institution")),"Contact instituition should be specified for Pride XML file")
    require( ( (!StringUtils.isEmpty(contact.getName())) || extraDataMap.contains("contact_name")),"Contact name should be specified for Pride XML file")
    
    val instrum = metadataBuilder.getInstrument
    require( ( (!StringUtils.isEmpty(instrum.getInstrumentName())) || extraDataMap.contains("instrument_name")),"Instrument name should be specified for Pride XML file")
    require( ( (instrum.getSource() != null && !instrum.getSource().getCvParam().isEmpty()) || (extraDataMap.contains("source_acc") && extraDataMap.contains("source_name")) ),"Instrument source should be specified for Pride XML file")

    //TOFO Get and verify instrument analyser and detector
    
    val description = new Description();
    //--- Generate admin part : "sampleName", "sampleDescription", "sourceFile","contact"    
    val admin = new Admin()

    if(extraDataMap.contains("contact_institution"))
      contact.setInstitution(extraDataMap("contact_institution").toString())
    if(extraDataMap.contains("contact_name"))
      contact.setName(extraDataMap("contact_name").toString())
     
    admin.getContact().add(contact)
    
    admin.setSampleName(extraDataMap("sample_name").toString)
      
    if(extraDataMap.contains("sample_desc") || extraDataMap.contains("sample_additional")) {
      val splDesc = new SampleDescription()
      
      if(extraDataMap.contains("sample_desc"))
    	  splDesc.setComment(extraDataMap("sample_desc").toString())
      
	  if(extraDataMap.contains("sample_additional")){
        val addValues : List[String] = extraDataMap("sample_additional").asInstanceOf[List[String]]
        addValues.foreach( nextEntry => {
        	splDesc.getCvParam().add(0, CvParam(nextEntry))
        })    	
      }
      
      admin.setSampleDescription(splDesc)
    }
    // TODO add Source File info, name, path... Not mandatory and not applicable for merge RSM
    description.setAdmin(admin)

    //--- Generate Instrument part :  "instrumentName", "source", "analyzerList", "detector", "additional"    
    description.setInstrument(metadataBuilder.getInstrument)
    
        //--- Generate DataProcessing  part :  processingMethod
    description.setDataProcessing(metadataBuilder.getDataProcessing)
    description
  }

  protected def _buildSpectrum(spectrum: Spectrum): uk.ac.ebi.pride.jaxb.model.Spectrum = {
    val prideSpectrum = new uk.ac.ebi.pride.jaxb.model.Spectrum()
    prideSpectrum.setId(spectrum.id.toInt)
    
    val spectrumDesc = new SpectrumDesc()
    
    val settings = new SpectrumSettings()    
    val instrument = new SpectrumInstrument()
    instrument.setMsLevel(2)
    //TODO Get correct value or ignore  
    //instrument.setMzRangeStart(0.0f)
    //instrument.setMzRangeStop(0.0f)    
    settings.setSpectrumInstrument(instrument)
    spectrumDesc.setSpectrumSettings(settings)
    
    val precursor = new Precursor()
    precursor.setMsLevel(1)
    val ionSelection = new Param();
    ionSelection.getCvParam().add(CvParam(PrideSchemaConstants.MS_CV_MASS_2_CHARGE_ACC, PrideSchemaConstants.MS_CV_MASS_2_CHARGE_NAME, PrideSchemaConstants.MS_CV_NAME, spectrum.precursorMoz.toString))
    ionSelection.getCvParam().add(CvParam(PrideSchemaConstants.MS_CV_CHARGE_STATE_ACC, PrideSchemaConstants.MS_CV_CHARGE_STATE_NAME, PrideSchemaConstants.MS_CV_NAME, spectrum.precursorCharge.toString))
    if (!spectrum.precursorIntensity.isNaN())
      ionSelection.getCvParam().add(CvParam(PrideSchemaConstants.MS_CV_INTENSITY_ACC, PrideSchemaConstants.MS_CV_INTENSITY_NAME, PrideSchemaConstants.MS_CV_NAME, spectrum.precursorIntensity.toString))
    precursor.setIonSelection(ionSelection)
     val activation = new Param();
    precursor.setActivation(activation)
    precursor.setSpectrum(PrideModelUtils.createSpectrum(0))
    
    val precursorList = new PrecursorList()
    precursorList.setCount(1)
    precursorList.getPrecursor().add(precursor)
    spectrumDesc.setPrecursorList(precursorList)
    
    val (mz, intensities) = _encodeSpectrum(spectrum)
    val mzBinary = new MzArrayBinary()
    var data = new Data()
    data.setEndian("little")
    data.setPrecision("64")
    data.setLength(spectrum.mozList.get.length)
    data.setValue(mz)
    mzBinary.setData(data)

    val intensitiesBinary = new IntenArrayBinary()
    data = new Data()
    data.setEndian("little")
    data.setPrecision("64")
    data.setLength(spectrum.intensityList.get.length)
    data.setValue(intensities)
    intensitiesBinary.setData(data)

    prideSpectrum.setSpectrumDesc(spectrumDesc)
    prideSpectrum.setMzArrayBinary(mzBinary)
    prideSpectrum.setIntenArrayBinary(intensitiesBinary)
    prideSpectrum
  }

  private def _encodeSpectrum(spectrum: Spectrum): (Array[Byte], Array[Byte]) = {
    //build byte arrays from the peak data
    val numPoints = spectrum.mozList.get.length
    val mzBytes = doublesToBytes(spectrum.mozList.get, true)

    // Convert doubles to a byte buffer
    val intensityBuff = ByteBuffer.allocate(numPoints * 8).order(ByteOrder.LITTLE_ENDIAN)
    spectrum.intensityList.get.foreach { intensityBuff.putDouble(_) }

    //	//make the base64 strings from the bytes
    //	val encodedMz = DatatypeConverter.printBase64Binary(mzBytes)
    //	val encodedIntensity = DatatypeConverter.printBase64Binary(intensityBuff.array())
    (mzBytes, intensityBuff.array())
  }

  protected def exportIdentification(filePath: String, marshaller: PrideXmlMarshaller, extraDataMap: Map[String, Object]) {
    var writer : FileWriter  = null
    try {
	    writer = new FileWriter(filePath,true)
	
	    val proteinSets = rsm.proteinSets.filter(_.isValidated).sortBy { p => proteinMatchesById(p.getRepresentativeProteinMatchId).score }.reverse
	    for (protSet <- proteinSets) {
	
	      val typicalProteinMatch = proteinMatchesById(protSet.getRepresentativeProteinMatchId)
	      val idf = new GelFreeIdentification()
	      val seqDb = seqDbById(typicalProteinMatch.seqDatabaseIds(0))
	      
	      idf.setAccession(typicalProteinMatch.accession)      
	      idf.setDatabase(seqDb.name)
	      var dbVersion =seqDb.version
	      // Try to get version from fasta path : after DB name... 
	      if(StringUtils.isEmpty(dbVersion) ) {
	    	  val filePath = seqDb.filePath
	    	  if(filePath.lastIndexOf(seqDb.name) != -1)
	    		  dbVersion = filePath.substring(filePath.lastIndexOf(seqDb.name)+seqDb.name.length())
    		  else {
    		    val sepIndex = Math.max(filePath.lastIndexOf("/"), filePath.lastIndexOf("\\"))
    		    if(sepIndex != -1)
    		      dbVersion = filePath.substring(sepIndex+1)
    		  }
	    	  if(dbVersion.startsWith("_"))
	    		  dbVersion = dbVersion.substring(1)
			  if(dbVersion.lastIndexOf('.') != -1)
				  dbVersion = dbVersion.substring(0,dbVersion.lastIndexOf('.'))
	      }
	      idf.setDatabaseVersion(dbVersion)
	      
	      val seqMatches = typicalProteinMatch.sequenceMatches.groupBy(_.getPeptideId)      
	      var nbrSpectrumNotFound = 0
	      for (pepInstance <- protSet.peptideSet.getPeptideInstances) {
	        val seqMatch = seqMatches(pepInstance.peptideId)(0)
	        val peptideMatch = rsm.resultSet.get.getPeptideMatchById.get(pepInstance.bestPeptideMatchId).get
	        
	        val peptideItem = new PeptideItem()
          // Replace X, B and Z  with corresponding AA used for matching
          val finalSeq =  if(peptideMatch.properties.isDefined && peptideMatch.properties.get.getMascotProperties.isDefined && peptideMatch.properties.get.getMascotProperties.get.ambiguityString.isDefined)
            {
              val originalSeq = pepInstance.peptide.sequence
              logger.trace(" Will Change seq for "+originalSeq)
              try {
                val ambiguityString = peptideMatch.properties.get.getMascotProperties.get.ambiguityString.get //formatted as 14,X,E for SIYGLTTDEAVVAXEEAK
                val parsedAmbiguity = ambiguityString.split(",")
                var tempFinalSeq = originalSeq
                var i = 0
                while(i < parsedAmbiguity.length) {
                  val genericCharIndex = Integer.parseInt(parsedAmbiguity(i))
                  val char2ReplaceWith: String = parsedAmbiguity(i+2)
                  val sb = new StringBuilder(tempFinalSeq.substring(0,genericCharIndex-1)).append(char2ReplaceWith).append(tempFinalSeq.substring(genericCharIndex))
                  tempFinalSeq = sb.toString()
                  i = i+3
                }
                logger.trace(" Change seq to "+tempFinalSeq)
                tempFinalSeq

              } catch {
                case e: Exception => originalSeq
              }

            } else { // finalSeq equals to original sequence
              pepInstance.peptide.sequence
            }
	        peptideItem.setSequence(finalSeq)
	        peptideItem.setStart(BigInteger.valueOf(seqMatch.start))
	        peptideItem.setEnd(BigInteger.valueOf(seqMatch.end))
	        //TODO getBest peptideMatch then Query then Spectrum.Id
	        peptideItem.setSpectrum(PrideModelUtils.createSpectrum(spectrumIdByPepMatchId(pepInstance.bestPeptideMatchId).toInt))
	        
	        //Add extra parameters
	        val additional = new Param()        
	        additional.getCvParam().add(CvParam(PrideSchemaConstants.PRIDE_CV_MASCOT_SCORE_ACC, PrideSchemaConstants.PRIDE_CV_MASCOT_SCORE_NAME, peptideMatch.score.toString))
	        additional.getCvParam().add(CvParam(PrideSchemaConstants.MS_CV_CHARGE_STATE_ACC, PrideSchemaConstants.MS_CV_CHARGE_STATE_NAME, peptideMatch.charge.toString))
	        additional.getCvParam().add(CvParam(PrideSchemaConstants.PRIDE_CV_RESIDUE_BEFORE_ACC, PrideSchemaConstants.PRIDE_CV_RESIDUE_BEFORE_NAME, seqMatch.residueBefore.toString))
	        additional.getCvParam().add(CvParam(PrideSchemaConstants.PRIDE_CV_RESIDUE_AFTER_ACC, PrideSchemaConstants.PRIDE_CV_RESIDUE_AFTER_NAME, seqMatch.residueAfter.toString))
	        peptideItem.setAdditional(additional)
	        
	        for (ptm <- pepInstance.peptide.ptms) {
	          peptideItem.getModificationItem().add(_buildModification(ptm))
	        }
	        
	        if (spectrumMatchByPeptideMatchId.contains(pepInstance.bestPeptideMatchId)) {
	        	val spectrumMatch =  spectrumMatchByPeptideMatchId(pepInstance.bestPeptideMatchId)
	        	_buildFragmentMatches(peptideItem,spectrumMatch)
	        } else {
	          nbrSpectrumNotFound += 1
	          logger.trace("Unable to get Spectrum for peptide {} ",pepInstance.peptide.sequence )
	        }
	        idf.getPeptideItem().add(peptideItem)
	      } // End go through Peptide Matches
	      if(nbrSpectrumNotFound >0)
	        logger.warn("Unable to found " +nbrSpectrumNotFound+" Spectra")
	        
	      idf.setScore(typicalProteinMatch.score)
	      //Add extra parameters
	      val protAdditional = new Param()
	      protAdditional.getCvParam().add(CvParam(PrideSchemaConstants.PRIDE_CV_PROT_DESCRIPTION_ACC, PrideSchemaConstants.PRIDE_CV_PROT_DESCRIPTION_NAME, typicalProteinMatch.description))
	      protAdditional.getCvParam().add(CvParam(PrideSchemaConstants.PRIDE_CV_PROT_IDENTIFIED_PEP_FRAG_ACC, PrideSchemaConstants.PRIDE_CV_PROT_IDENTIFIED_PEP_FRAG_NAME, ""))
	      protSet.getSameSetProteinMatchIds.foreach( ssId => {
	        if(!ssId.equals(protSet.getRepresentativeProteinMatchId)){
	        	val ssProteinMatch = proteinMatchesById(ssId)
	        	protAdditional.getCvParam().add(CvParam(PrideSchemaConstants.PRIDE_CV_PROT_SAMSET_ACC, PrideSchemaConstants.PRIDE_CV_PROT_SAMSET_NAME,ssProteinMatch.accession))
	        }         
	      })
	      idf.setAdditional(protAdditional)
	      marshaller.marshall(idf, writer)
	    } //End go through Protein Sets
	    
	    writer.flush()
	    writer.close()
    } catch {
    	case ioe : IOException =>{
    		try {
    		  if(writer != null){
	        	writer.flush()
	        	writer.close()
	      	}
    	  }catch { case e: Exception =>
    	    
    	  }  finally {
    	    throw new Exception(" Error writing Pride Identification Data  "+ioe.getMessage()) 
    	  }
      } 
    }

  }

  private def _buildFragmentMatches(pepItem: PeptideItem, spectrumMatch: SpectrumMatch) = {
    val fragMatches = spectrumMatch.fragMatches

    if(fragMatches != null) {
      fragMatches.foreach(fragMatch => {

        val fragmentIon = new FragmentIon()
        fragmentIon.getCvParam().add(CvParam(PrideSchemaConstants.PRIDE_CV_PROD_ION_CHARGE_ACC, PrideSchemaConstants.PRIDE_CV_PROD_ION_CHARGE_NAME, fragMatch.charge.toString))
        fragmentIon.getCvParam().add(CvParam(PrideSchemaConstants.PRIDE_CV_PROD_ION_MZ_ACC, PrideSchemaConstants.PRIDE_CV_PROD_ION_MZ_NAME, fragMatch.moz.toString))

        if (fragMatch.intensity != Float.NaN)
          fragmentIon.getCvParam().add(CvParam(PrideSchemaConstants.PRIDE_CV_PROD_ION_I_ACC, PrideSchemaConstants.PRIDE_CV_PROD_ION_I_NAME, fragMatch.intensity.toString))

        val serie = fragMatch.ionSeries
          
      val (cvIonAcc, cvIonName)  = FragmentMatchMapper.getPrideCVforIonSerie(serie.toString)
        fragmentIon.getCvParam().add(CvParam(cvIonAcc, cvIonName, fragMatch.aaPosition.toString))           
        fragmentIon.getCvParam().add(CvParam(PrideSchemaConstants.PRIDE_CV_PROD_ION_MASS_ERR_ACC, PrideSchemaConstants.PRIDE_CV_PROD_ION_MASS_ERR_NAME,(fragMatch.calculatedMoz-fragMatch.moz).toString))
        
        
        pepItem.getFragmentIon().add(fragmentIon)
      }) //END Go trough fragment matches
    }

  }
  
  protected def _buildModification(ptm: LocatedPtm): ModificationItem = {
    val mod = new ModificationItem()
    mod.setModLocation(BigInteger.valueOf(ptm.seqPosition))
    mod.setModDatabase("MOD")
    mod.setModAccession(UnimodToPSIPtmMap.map(ptm.definition.unimodId).getAccession())
    mod.getModMonoDelta().add(ptm.monoMass.toString)
    mod.getModAvgDelta().add(ptm.averageMass.toString)
    mod
  }

}