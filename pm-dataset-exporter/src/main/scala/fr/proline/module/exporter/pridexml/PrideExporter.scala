package fr.proline.module.exporter.pridexml

import java.io.FileWriter
import java.io.StringReader
import scala.collection.mutable.{ ArrayBuffer, HashMap }
import com.typesafe.scalalogging.slf4j.Logging
import uk.ac.ebi.pride.jaxb.xml.marshaller.PrideXmlMarshallerFactory
import fr.proline.core.om.model.msi.MSISearch
import fr.proline.core.om.model.msi.ResultSet
import fr.proline.core.om.model.msi.ResultSummary
import fr.proline.core.om.model.msi.Ms2Query
import fr.proline.core.om.model.msi.PtmDefinition
import fr.proline.core.om.model.msi.SequenceMatch
import uk.ac.ebi.pride.jaxb.model.Description
import uk.ac.ebi.pride.jaxb.model.Admin
import uk.ac.ebi.pride.jaxb.model.Contact
import fr.proline.core.om.model.msi.Spectrum
import uk.ac.ebi.pride.jaxb.model.SpectrumDesc
import uk.ac.ebi.pride.jaxb.model.SpectrumSettings
import uk.ac.ebi.pride.jaxb.model.SpectrumInstrument
import uk.ac.ebi.pride.jaxb.model.Precursor
import uk.ac.ebi.pride.jaxb.model.Param
import uk.ac.ebi.pride.jaxb.model.PrecursorList
import uk.ac.ebi.pride.jaxb.model.MzArrayBinary
import fr.profi.util.bytes._
import java.nio.ByteOrder
import java.nio.ByteBuffer
import javax.xml.bind.DatatypeConverter
import uk.ac.ebi.pride.jaxb.model.Data
import uk.ac.ebi.pride.jaxb.model.IntenArrayBinary
import uk.ac.ebi.pride.jaxb.model.GelFreeIdentification
import fr.proline.core.dal.DoJDBCReturningWork
import fr.proline.context.DatabaseConnectionContext
import uk.ac.ebi.pride.jaxb.model.PeptideItem
import java.math.BigInteger
import uk.ac.ebi.pride.jaxb.utils.PrideModelUtils
import fr.proline.core.om.provider.msi.impl.SQLProteinMatchProvider
import fr.proline.core.om.model.msi.ProteinMatch
import fr.proline.core.om.provider.msi.impl.SQLSpectrumProvider
import fr.proline.core.dal.DoJDBCWork
import uk.ac.ebi.pride.jaxb.model.Instrument
import uk.ac.ebi.pride.jaxb.model.Protocol
import uk.ac.ebi.pride.jaxb.model.ModificationItem
import fr.proline.core.om.model.msi.LocatedPtm
import fr.proline.core.om.model.msi.SpectrumMatch
import fr.proline.core.dal.tables.msi.MsiDbPeptideMatchObjectTreeMapTable
import fr.proline.core.dal.tables.msi.MsiDbObjectTreeTable
import fr.proline.core.dal.tables.SelectQueryBuilder2
import fr.proline.core.dal.tables.SelectQueryBuilder._
import fr.profi.util.serialization._

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
  msiSQLCtx: DatabaseConnectionContext) extends Logging {

  object CustomSerializer extends ProfiJSMSerialization with CustomDoubleJacksonSerializer 
  
  // Retrieve some proline objects
  val rs = rsm.resultSet.get
  val rsId = rs.id
  val optionalMsiSearch = rs.msiSearch

  // TODO LMN : May not work for merged ResultSet (refs #7486) - use childMsiSearches and build a consensus MSISearch
  require((optionalMsiSearch != null) && optionalMsiSearch.isDefined, "ResultSet #" + rsId + " has no associated MSI Search")

  val metadataBuilder = PrideMetadataBuilder(optionalMsiSearch.get)

  private val validPeptideInstance = rsm.peptideInstances.filter(_.validatedProteinSetsCount > 0)
  private val pepInstByPepId = Map() ++ validPeptideInstance.map(pi => pi.peptide.id -> pi)
  private val validPepMatchIdSet = validPeptideInstance.map(_.bestPeptideMatchId).toSet

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

  private val proteinMatchesById = {
    val provider = new SQLProteinMatchProvider(msiSQLCtx)
    val proteinMatchesById = new HashMap[Long, ProteinMatch]
    Map() ++ provider.getResultSummariesProteinMatches(Array(rsm.id)).map(pm => (pm.id -> pm))
  }

  private class SeqDatabaseRecord(val id: Long, val name: String, val version: String)

  private val seqDbById = {
    val seqDbById = new HashMap[Long, SeqDatabaseRecord]
    DoJDBCReturningWork.withEzDBC(msiSQLCtx, { msiEzDBC =>
      msiEzDBC.selectAndProcess("SELECT id, name, version FROM seq_database") { r =>
        val id = r.nextLong
        seqDbById += (id -> new SeqDatabaseRecord(id, r.nextString, r.nextString))
      }
    })
    Map() ++ seqDbById
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

  def exportResultSummary(filePath: String) {

    val marshaller = PrideExporter.marshaller
    var writer = new FileWriter(filePath)
    // XML header
    writer.write("""<?xml version="1.0" encoding="utf-8" ?>""")
    writer.write('\n')
    writer.write("""<ExperimentCollection version="2.1">""")
    writer.write('\n')
    writer.write("<Experiment>")
    writer.write('\n')
    writer.write("<Title>" + rs.name + "</Title>")
    writer.write('\n')
    writer.write("<ShortLabel>" + rs.name + "</ShortLabel>")
    writer.write('\n')

    val protocolSample = <Protocol>
                           <ProtocolName>In Gel Protein Digestion</ProtocolName>
                           <ProtocolSteps>
                             <StepDescription>
                               <cvParam cvLabel="PRIDE" accession="PRIDE:0000025" name="Reduction" value="DTT"/>
                             </StepDescription>
                             <StepDescription>
                               <cvParam cvLabel="PRIDE" accession="PRIDE:0000026" name="Alkylation" value="iodoacetamide"/>
                             </StepDescription>
                             <StepDescription>
                               <cvParam cvLabel="PRIDE" accession="PRIDE:0000160" name="Enzyme" value="Trypsin"/>
                             </StepDescription>
                           </ProtocolSteps>
                         </Protocol>

    writer.write(protocolSample + "\n")

    writer.write("""<mzData version="1.05" accessionNumber="0">""")
    writer.write('\n')
    marshaller.marshall(_buildMzDataDescription, writer)
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
    writer.write("\n</mzData>")
    val proteinSets = rsm.proteinSets.filter(_.isValidated).sortBy { p => proteinMatchesById(p.getTypicalProteinMatchId).score }.reverse
    for (protSet <- proteinSets) {
      val typicalProteinMatch = proteinMatchesById(protSet.getTypicalProteinMatchId)
      val idf = new GelFreeIdentification()
      idf.setAccession(typicalProteinMatch.accession)
      idf.setDatabase(seqDbById(typicalProteinMatch.seqDatabaseIds(0)).name)
      idf.setDatabaseVersion(seqDbById(typicalProteinMatch.seqDatabaseIds(0)).version)
      val seqMatches = typicalProteinMatch.sequenceMatches.groupBy(_.getPeptideId)
      for (pepInstance <- protSet.peptideSet.getPeptideInstances) {
        val peptideItem = new PeptideItem()
        peptideItem.setSequence(pepInstance.peptide.sequence)
        peptideItem.setStart(BigInteger.valueOf(seqMatches(pepInstance.peptideId)(0).start))
        peptideItem.setEnd(BigInteger.valueOf(seqMatches(pepInstance.peptideId)(0).end))
        //TODO getBest peptideMatch then Query then Spectrum.Id
        peptideItem.setSpectrum(PrideModelUtils.createSpectrum(spectrumIdByPepMatchId(pepInstance.bestPeptideMatchId).toInt))
        val additional = new Param()
        val peptideMatch = rsm.resultSet.get.getPeptideMatchById.get(pepInstance.bestPeptideMatchId).get
        additional.getCvParam().add(CvParam("PRIDE:0000069", "Mascot score", peptideMatch.score.toString))
        peptideItem.setAdditional(additional)
        for (ptm <- pepInstance.peptide.ptms) {
          peptideItem.getModificationItem().add(_buildModification(ptm))
        }
        if (spectrumIdByPepMatchId.contains(pepInstance.bestPeptideMatchId)) {
          logger.info("TODO :: GENERATE FRAGMENT MATCHES HERE ...... ")
        }
        idf.getPeptideItem().add(peptideItem)
      }
      idf.setScore(typicalProteinMatch.score)
      marshaller.marshall(idf, writer)
    }
    writer.write("\n<additional>\n")
    marshaller.marshall(CvParam("PRIDE:0000175", "XML generation software", "Proline"), writer)
    writer.write("\n</additional>")
    writer.write("\n</Experiment>")
    writer.write("\n</ExperimentCollection>")
    writer.flush()
    writer.close()
    logger.info("Pride XML done")
  }

  protected def _buildSpectrum(spectrum: Spectrum): uk.ac.ebi.pride.jaxb.model.Spectrum = {
    val prideSpectrum = new uk.ac.ebi.pride.jaxb.model.Spectrum()
    prideSpectrum.setId(spectrum.id.toInt)
    val spectrumDesc = new SpectrumDesc()
    val settings = new SpectrumSettings()
    val instrument = new SpectrumInstrument()
    instrument.setMsLevel(2)
    instrument.setMzRangeStart(0.0f)
    instrument.setMzRangeStop(0.0f)
    settings.setSpectrumInstrument(instrument)
    spectrumDesc.setSpectrumSettings(settings)
    val precursor = new Precursor()
    precursor.setMsLevel(1)
    val ionSelection = new Param();
    ionSelection.getCvParam().add(CvParam("PSI:1000040", "MassToChargeRatio", "PSI", spectrum.precursorMoz.toString))
    ionSelection.getCvParam().add(CvParam("PSI:1000041", "ChargeState", "PSI", spectrum.precursorCharge.toString))
    if (!spectrum.precursorIntensity.isNaN())
      ionSelection.getCvParam().add(CvParam("PSI:1000042", "Intensity", "PSI", spectrum.precursorIntensity.toString))
    precursor.setIonSelection(ionSelection)
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

  protected def _buildModification(ptm: LocatedPtm): ModificationItem = {
    val mod = new ModificationItem()
    mod.setModLocation(BigInteger.valueOf(ptm.seqPosition))
    mod.setModDatabase("MOD")
    mod.setModAccession(UnimodToPSIPtmMap.map(ptm.definition.unimodId).getAccession())
    mod.getModMonoDelta().add(ptm.monoMass.toString)
    mod.getModAvgDelta().add(ptm.averageMass.toString)
    mod
  }

  private def _buildMzDataDescription(): Description = {
    val description = new Description();
    val admin = new Admin()
    admin.getContact().add(metadataBuilder.getContact)
    description.setAdmin(admin)
    description.setInstrument(metadataBuilder.getInstrument)
    description.setDataProcessing(metadataBuilder.getDataProcessing)
    description
  }

}