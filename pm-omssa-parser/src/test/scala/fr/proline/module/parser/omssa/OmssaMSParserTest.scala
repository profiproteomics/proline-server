package fr.proline.module.parser.omssa

import com.weiglewilczek.slf4s.Logging
import java.io.File
import org.junit.Assert._
import org.junit.{ Before, Test, After }
import scala.collection.mutable.ArrayBuffer
import fr.proline.core.om.model.msi.{ MsQuery, Ms2Query, ResultSet, SearchSettings, MSISearch, SpectrumMatch, Spectrum }
import fr.proline.repository.utils.DatabaseTestCase
import fr.proline.repository.ProlineDatabaseType
import fr.proline.context.{ DatabaseConnectionContext, BasicExecutionContext }
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.{ IProteinProvider, ISeqDatabaseProvider, IPeptideProvider, IPTMProvider }
import fr.proline.module.parser.provider.fake.{ ProteinFakeProvider, SeqDbFakeProvider, PeptideFakeProvider, PTMFakeProvider }
import net.liftweb.json.{ pretty, compact }
import net.liftweb.json.JsonDSL._
import net.liftweb.json.JsonAST._
import java.io.FileNotFoundException
import fr.proline.module.fragment_match.FragmentTable
import fr.proline.util.MathUtils

@Test
class OmssaMSParserTest extends Logging {

  var psDBTestCase: DatabaseTestCase = null
  var executionContext: BasicExecutionContext = null
  var parserContext: ProviderDecoratedExecutionContext = null
  // configuration files
  val omssaConfigFolder: String = "/omssa_config"
  //  val xsdFileName: String = omssaConfigFolder + "/OMSSA.xsd"
  //  val modsFileName: String = omssaConfigFolder + "/mods.xml"
  val userModsFileName: String = omssaConfigFolder + "/usermods.xml"
  val omssaSampleFolder: String = "omx_samples"
  val defaultDatasetsFolder: String = "/default_datasets"

  class PSDatabaseTestCase extends DatabaseTestCase {

    override def getProlineDatabaseType() = ProlineDatabaseType.PS

    override def getPropertiesFileName() = "db_settings/h2/db_ps.properties"

  }

  class PDIDatabaseTestCase extends DatabaseTestCase {

    override def getProlineDatabaseType() = ProlineDatabaseType.PDI

    override def getPropertiesFileName() = "db_settings/h2/db_pdi.properties"

  }

  val propertiesBuilder = Map.newBuilder[String, Any]
  propertiesBuilder += (OmssaParseParams.OMSSA_VERSION.toString -> "2.1.9")
  //  propertiesBuilder += (OmssaParseParams.OMSSA_XSD_FILE.toString -> new File(this.getClass().getResource(xsdFileName).toURI()))
  //  propertiesBuilder += (OmssaParseParams.MOD_XML_FILE.toString -> new File(this.getClass().getResource(modsFileName).toURI()))
  propertiesBuilder += (OmssaParseParams.USERMOD_XML_FILE.toString -> new File(this.getClass().getResource(userModsFileName).toURI()))
  propertiesBuilder += (OmssaParseParams.FASTA_CONTAINS_TARGET.toString -> true)
  propertiesBuilder += (OmssaParseParams.FASTA_CONTAINS_DECOY.toString -> true)
  propertiesBuilder += (OmssaParseParams.FASTA_FILE_PATH.toString -> "")
  propertiesBuilder += (OmssaParseParams.FASTA_TAXONOMIES.toString -> "")
  propertiesBuilder += (OmssaParseParams.PEAK_LIST_FILE_PATH.toString -> "")
  propertiesBuilder += (OmssaParseParams.RAW_FILE_PATH.toString -> "")

  @Before
  def init() {
    try {
      logger.info("Start Logging ")
      logger.debug("Start Logging Debug ")
      // Init PS db connexion
      psDBTestCase = new PSDatabaseTestCase()
      psDBTestCase.initDatabase()
      psDBTestCase.loadDataSet(defaultDatasetsFolder + "/Unimod_Dataset.xml")

      val psDbCtx = new DatabaseConnectionContext(psDBTestCase.getConnector)
      executionContext = new BasicExecutionContext(null, null, psDbCtx, null, null)
      parserContext = new ProviderDecoratedExecutionContext(executionContext)
      parserContext.putProvider(classOf[IProteinProvider], ProteinFakeProvider)
      parserContext.putProvider(classOf[ISeqDatabaseProvider], SeqDbFakeProvider)
    } catch {
      case e: Exception => logger.error("test init failed", e)
    }
  }

  @After
  def closeResources() {
    //    executionContext.closeAll()
    psDBTestCase.tearDown()
  }

  private def getMethod(): String = {
    val st = Thread.currentThread.getStackTrace()
    st(2).getMethodName()
  }
  private def getCallingMethod(): String = {
    val st = Thread.currentThread.getStackTrace()
    st(3).getMethodName()
  }

  /**
   * @param file is the name of the file to parse
   * @param parserContext is the context of execution
   * @return the OmssaResultFile object that contains the parse of the omx file
   */
  private def parseOmxFile(file: String): OmssaResultFile = {
    assert(file != null, "parseOmxFile() file is null")

    val filePath = omssaSampleFolder + "/" + file

    logger.debug("Parsing [" + filePath + ']')

    var omx: OmssaResultFile = null

    try {
      val cl = Thread.currentThread.getContextClassLoader

      omx = new OmssaResultFile(
        fileLocation = new File(cl.getResource(filePath).toURI()),
        parserContext = parserContext,
        importProperties = propertiesBuilder.result)

    } catch {

      case ex: Exception => {
        logger.error("Error parsing [" + filePath + ']', ex)

        throw ex // Re-throw without changing type of the Exception
      }

    }

    return omx
  }

  /*
     * main test with a small correct file
     */
  @Test
  def testMgfInputFile {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")
    // this file is a simple search that should be correct (it can be used to verify most of the algorithm)
    val omssaOmxFile = parseOmxFile("STG_NCSpiste1_OTD_mgfInputFile.omx")
    val rs = omssaOmxFile.getResultSet(wantDecoy = false)
    assertEquals(2, rs.proteinMatches.length)
    val ionSeries: ArrayBuffer[String] = new ArrayBuffer[String]()
    def eachSpectrumMatches(spectrumMatch: SpectrumMatch) = {
      for (fm <- spectrumMatch.fragMatches) {
        if (fm.label == "y(7)+") assert(fm.ionSeries == "y" && fm.aaPosition == 7 && fm.charge == 1)
        else if (fm.label == "b(10)++") assert(fm.ionSeries == "b" && fm.aaPosition == 10 && fm.charge == 2)
        else if (fm.label == "y(22)+++") assert(fm.ionSeries == "y" && fm.aaPosition == 22 && fm.charge == 3)
      }
    }
    omssaOmxFile.eachSpectrumMatch(false, eachSpectrumMatches)
    for (ion <- ionSeries) logger.debug(ion)
    logger.debug("TEST [" + method + "] OK: parsing is successful")
  }
  @Test
  def testSpectrumMatches {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")
    try {
      // this file is a simple search that should be correct (it can be used to verify most of the algorithm)
      val omssaOmxFile = parseOmxFile("STG_NCSpiste1_OTD_mgfInputFile.omx")
      val rs = omssaOmxFile.getResultSet(wantDecoy = false)
      assertEquals(2, rs.proteinMatches.length)
      val ionSeries: ArrayBuffer[String] = new ArrayBuffer[String]()
      def eachSpectrumMatches(spectrumMatch: SpectrumMatch) = {
        logger.debug("SpectrumMatch: msQueryInitialId:" + spectrumMatch.msQueryInitialId + " peptideMatchRank:" + spectrumMatch.peptideMatchRank)
        logger.debug("Spectrum match fragment ion table :")
        //        logger.debug(fr.proline.module.fragment_match.FragmentTable.fragmentIonTableAsString(spectrumMatch.fragTable))
        logger.debug(FragmentTable.fragmentIonTableAsString(spectrumMatch.fragTable))
        //        spectrumMatch.fragTable.foreach(ft => logger.debug("Serie:"+ft.fragSeries+" isReverse:"+ft.isReverse+" "+ft.masses.))
        logger.debug("Spectrum match fragment matches :")
        //        for (sm <- spectrumMatch.fragMatches) logger.debug("FragmentMatch " + sm.label + " : type:" + sm.`type`.toString() + " moz:" + sm.moz + " theoMoz:" + sm.calculatedMoz + " intensity:" + sm.intensity + " neutralLossMass:" + sm.neutralLossMass)
        spectrumMatch.fragMatches.foreach(fm => logger.debug("FragmentMatch " + fm.label + " : type:" + fm.`type`.toString() + " moz:" + fm.moz + " theoMoz:" + fm.calculatedMoz + " intensity:" + fm.intensity + " neutralLossMass:" + fm.neutralLossMass))
        logger.debug("")
      }
      omssaOmxFile.eachSpectrumMatch(false, eachSpectrumMatches)
      for (ion <- ionSeries) logger.debug(ion)
      logger.debug("TEST [" + method + "] OK: parsing is successful")
    } catch {
      case e: Exception =>
        logger.error("TEST [" + method + "] in error", e)
      //        throw e
    }
  }

  @Test
  def testResultFileProvider {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")
    val resultFileProvider = new OmssaResultFileProvider

    val cl = Thread.currentThread.getContextClassLoader

    val resultFile = resultFileProvider.getResultFile(
      fileLocation = new File(cl.getResource(omssaSampleFolder + "/STG_NCSpiste1_OTD_mgfInputFile.omx").toURI()),
      importProperties = propertiesBuilder.result,
      parserContext = parserContext)
    val rs = resultFile.getResultSet(wantDecoy = false)
    assertEquals(2, rs.proteinMatches.length)
    logger.debug("TEST [" + method + "] OK: parsing is successful")
  }
  @Test
  def additionalTestsForCobertura {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")
    val omssaOmxFile = parseOmxFile("STG_NCSpiste1_OTD_mgfInputFile.omx")
    val rs = omssaOmxFile.getResultSet(wantDecoy = false)
    // test JSON settings
    logger.debug("TEST [" + method + "] read JSon settings")
    val jsonSettings = omssaOmxFile.omssaSettingsInJsonFormat
    //    logger.debug(pretty(render(jsonSettings)))
    for (setting <- jsonSettings) {
      //      logger.debug("JSON setting: " + setting.values.first._1 + " => " + setting.values.first._2)
      if (setting.values.first._1 == "MSSearchSettings_numisotopes") assert(setting.values.first._2 == "0")
      else if (setting.values.first._1 == "MSSearchSettings_scale") assert(setting.values.first._2 == "1000")
      else if (setting.values.first._1 == "MSSearchSettings_cutoff") assert(setting.values.first._2 == "10")
      else if (setting.values.first._1 == "MSSearchSettings_db") assert(setting.values.first._2 == "db/spHomo_DCpSP_ABU_20121206")
      else if (setting.values.first._1 == "MSSearchSettings_exactmass") assert(setting.values.first._2 == "1446.94")
    }
    // test mandatory files
    logger.debug("TEST [" + method + "] read mandatory files")
    val omssaLoader = omssaOmxFile.getOmssaLoader
    assert(omssaLoader.enzymes.size == 25)
    // test msQueryByInitialId
    logger.debug("TEST [" + method + "] calling msQueryByInitialId")
    assert(omssaOmxFile.msQueryByInitialId.get(0).get.moz == 825.339)
    //    for(msQuery <- omssaOmxFile.msQueryByInitialId) {
    //      logger.debug("id="+msQuery._1+" / moz=" + msQuery._2.moz)
    //    }
    // test an null file
    try {
      logger.debug("TEST [" + method + "] parse an null file")
      val noOmxFile = new OmssaResultFile(fileLocation = null, parserContext = parserContext, importProperties = propertiesBuilder.result)
      // parsing should fail
      logger.debug("TEST [" + method + "] KO: parsing has not failed as expected !!")
    } catch {
      case e: FileNotFoundException => logger.debug("TEST [" + method + "] OK: parsing has failed as expected")
      case e: Exception =>
        logger.debug("TEST [" + method + "] KO: parsing has failed for an unexpected reason : " + e, e)

        throw e
    }
    // test an invalid file path
    try {
      logger.debug("TEST [" + method + "]  parse an non existing file")
      val noOmxFile = new OmssaResultFile(fileLocation = new File("/usr/local/myFile.omx"), parserContext = parserContext, importProperties = propertiesBuilder.result)
      // parsing should fail
      logger.debug("TEST [" + method + "] KO: parsing has not failed as expected !!")
    } catch {
      case e: FileNotFoundException => logger.debug("TEST [" + method + "] OK: parsing has failed as expected")
      case e: Exception =>
        logger.debug("TEST [" + method + "] KO: parsing has failed for an unexpected reason : " + e, e)

        throw e
    }
    logger.debug("TEST [" + method + "] OK : all additional tests are successfull")
  }

  @Test
  def testSearchInNCBI {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")
    val omssaOmxFile = parseOmxFile("STG_NCSpiste1_OTD_ncbiDatabase.omx")
    val rs = omssaOmxFile.getResultSet(wantDecoy = true)
    for (pm <- rs.proteinMatches) {
      pm.accession match {
        case "49614" => assert(pm.description == "Reverse sequence, was rCG48939 [Rattus norvegicus] (originalGI:149067022)")
        case "60727" => assert(pm.description == "Reverse sequence, was neuron navigator 3 [Rattus norvegicus] (originalGI:300796331)")
      }
    }
    logger.debug("TEST [" + method + "] OK: parsing is successful")
  }
  @Test
  def testFileWithoutSpectraNorSettings {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")
    // this file has been generated with the setting MSOutFile_includerequest=false
    // this mean that the file only contains the results, but not the settings or the spectra
    // the parsing should fail
    try {
      val omssaOmxFile = parseOmxFile("STG_NCSpiste1_OTD_noSpectraNoSettings.omx")
      // parsing should fail
      logger.debug("TEST [" + method + "] KO: parsing has not failed as expected !!")
    } catch {
      case e: UnexpectedOmxFormatException => logger.debug("TEST [" + method + "] OK: parsing has failed as expected")
      case e: Exception =>
        logger.debug("TEST [" + method + "] KO: parsing has failed for an unexpected reason : " + e, e)

        throw e
    }
  }
  /* 
     * tests related to the peaklists
     */
  @Test
  def testMsMerge {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")
    // MSMerge is a script made in LSMBO that merges mgf/pkl/dta files into a single mgf file
    // It also adds the name of the original file at the end of the TITLE line, to keep its unicity and to help users to find spectra origin
    // for instance, this title : TITLE= Cmpd 1, +MSn(825.34), 1.4 min
    // would become : TITLE= Cmpd 1, +MSn(825.34), 1.4 min, F037602LSA.mgf
    val omssaOmxFile = parseOmxFile("STG_NCSpiste1_OTD_msmerge.omx")
    def onEachSpectrum(spectrum: Spectrum) = assertEquals(spectrum.title.endsWith(", F037602LSA.mgf"), true)
    omssaOmxFile.eachSpectrum(onEachSpectrum)
    logger.debug("TEST [" + method + "] OK : All spectra ends with the original file name (F037602LSA.mgf)")
  }
  @Test
  def testAbundanceScaleLessThanOne {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")
    // omssa turns abundances float values into integer, and indicates the scale in MSSpectrum_iscale, but sometimes the value is 0.1
    // some softwares such as scaffold (until v3.6.5) crashes with this value
    val omssaOmxFile = parseOmxFile("STG_F034524BA_OTD_scaleLessThanOne.omx")
    // eachSpectra should return correct abundances
    def onEachSpectrum(spectrum: Spectrum) = {
      if (spectrum.title == " Cmpd 807, +MSn(421.09), 8.5 min") {
        val intensities = spectrum.intensityList.get
        assert(intensities(0) == 5533260)
      }
    }
    omssaOmxFile.eachSpectrum(onEachSpectrum)
    logger.debug("TEST [" + method + "] OK : First intensity for compound 807 is correct (5533260)")
  }
  @Test
  def testDtaInputFile {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")
    // omssa accepts dta files
    val omssaOmxFile = parseOmxFile("STG_LTQFT10APR1025_OTD_dtaInputFile.omx")
    // parsing should succeed
    val rs = omssaOmxFile.getResultSet(false)
    assert(omssaOmxFile.getMsQueries.length == 1)
    assert(rs.peptideMatches(0).getMs2Query.spectrumTitle == " Cmpd 1, +MSn(678.859), ? min")
    def onEachSpectrum(spectrum: Spectrum) = assert(spectrum.title == " Cmpd 1, +MSn(678.859), ? min")
    omssaOmxFile.eachSpectrum(onEachSpectrum)
    logger.debug("TEST [" + method + "] OK: parsing is successful")
  }
  @Test
  def testPklInputFile {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")
    // omssa accepts dta files
    val omssaOmxFile = parseOmxFile("STG_W18776LSA_OTD_pklInputFile.omx")
    // parsing should succeed
    val rs = omssaOmxFile.getResultSet(false)
    assert(omssaOmxFile.getMsQueries.length == 15)
    assert(omssaOmxFile.getMsQueries(0).spectrumTitle == " Cmpd 5, +MSn(639.824), ? min")
    assert(rs.peptideMatches.length == 10)
    assert(rs.proteinMatches.length == 29)
    logger.debug("TEST [" + method + "] OK: parsing is successful")
  }

  /* 
         * tests related to specific options in omssa
         */
  @Test
  def testMozTolerance {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")
    // omssa allows M/z tolerances in Dalton or ppm since v2.1.9
    // the user can ask for Product ion M/z tolerance in ppm, but omssa 2.1.9 does not consider this option (it uses Dalton)
    // first file with product ion M/z tolerance at 0.25Da 
    val omssaOmxFileDalton = parseOmxFile("STG_NCSpiste1_OTD_precursorDaltonFragmentsDalton.omx")
    val rsDalton = omssaOmxFileDalton.getResultSet(false)
    // second file with product ion M/z tolerance at 0.25ppm 
    val omssaOmxFilePpm = parseOmxFile("STG_NCSpiste1_OTD_precursorDaltonFragmentsPpm.omx")
    val rsPpm = omssaOmxFilePpm.getResultSet(false)
    // both should return the same number of peptides and the same peptides in the same order
    assert(rsDalton.peptideMatches.length == rsPpm.peptideMatches.length)
    var nbCommonPeptideMatches: Int = 0
    // this syntax is a double loop on rsDalton.peptideMatches, then on rsPpm.peptideMatches
    for (pmd <- rsDalton.peptideMatches; pmp <- rsPpm.peptideMatches) {
      if (pmd.peptide.sequence == pmp.peptide.sequence) nbCommonPeptideMatches += 1
    }
    assert(nbCommonPeptideMatches == rsDalton.peptideMatches.length)
    // both should return the same number of proteins and the same proteins in the same order
    assert(rsDalton.proteinMatches.length == rsPpm.proteinMatches.length)
    var nbCommonProteinMatches: Int = 0
    for (pmd <- rsDalton.proteinMatches; pmp <- rsPpm.proteinMatches) {
      if (pmd.accession == pmp.accession) nbCommonProteinMatches += 1
    }
    assert(nbCommonProteinMatches == rsDalton.proteinMatches.length)
    logger.debug("TEST [" + method + "] OK: parsing is successful")
  }

  /*
           * tests related to the results
           */
  @Test
  def testNoMatch {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")
    try {
      // case when no spectra has match at all
      val omssaOmxFile = parseOmxFile("STG_NCSpiste1_OTD_noMatches.omx")
      // parsing should succeed
      // resultsets should be empty
      val rsTarget = omssaOmxFile.getResultSet(false)
      // parsing should fail
      logger.debug("TEST [" + method + "] KO: parsing has not failed as expected !!")
    } catch {
      case e: Exception =>
        if (e.getMessage() == "No pepToPeptideMatches left") logger.debug("TEST [" + method + "] OK: parsing has failed as expected")
        else {
          logger.debug("TEST [" + method + "] KO: parsing has failed for an unexpected reason : " + e, e)

          throw e
        }
    }
    //    
    //    
    ////    No pepToPeptideMatches left
    //    assertEquals(rsTarget.getProteins().size, 0)
    //    assertEquals(rsTarget.peptides.size, 0)
    //    assertEquals(rsTarget.peptideMatches.size, 0)
    //    assertEquals(rsTarget.proteinMatches.size, 0)
    ////    val rsDecoy = omssaOmxFile.getResultSet(true)
    ////    assertEquals(rsDecoy.getProteins().size, 0)
    ////    assertEquals(rsDecoy.peptides.size, 0)
    ////    assertEquals(rsDecoy.peptideMatches.size, 0)
    ////    assertEquals(rsDecoy.proteinMatches.size, 0)
    //    // eachSpectrumMatch should be empty
    //    val lstSpectra: ArrayBuffer[SpectrumMatch] = new ArrayBuffer[SpectrumMatch]
    //    def onEachSpectrumMatch(spectrumMatch: SpectrumMatch) = lstSpectra += spectrumMatch
    //    omssaOmxFile.eachSpectrumMatch(true, onEachSpectrumMatch)
    //    assertEquals(lstSpectra.length, 0)
    //    logger.debug("TEST [" + method + "] OK: parsing is successful")
  }
  @Test
  def testMergeOfTwoDifferentOmxFiles {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")
    // this file is the result of a merge of two different searches (not the same enzyme used)
    // this should return an exception
    try {
      val omssaOmxFile = parseOmxFile("STG_NCSpiste1_OTD_omssamergeKO.omx")
      // parsing should fail
      logger.debug("TEST [" + method + "] KO: parsing has not failed as expected !!")
    } catch {
      case e: NotMatchingSearchSettingsException => logger.debug("TEST [" + method + "] OK: parsing has failed as expected")
      case e: Exception =>
        logger.debug("TEST [" + method + "] KO: parsing has failed for an unexpected reason : " + e, e)

        throw e
    }
  }
  @Test
  def testMergeOfTwoSimilarOmxFiles {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")
    // this file is the result of a merge of two searches from a same set (same settings, except for input and output files)
    val omssaOmxFile = parseOmxFile("STG_NCSpiste1_OTD_omssamergeOK.omx")
    // parsing should succeed
    val rs = omssaOmxFile.getResultSet(false)
    logger.debug("TEST [" + method + "] OK: parsing is successful")
  }
  @Test
  def testSplitTargetDecoy {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")
    val omssaOmxFile = parseOmxFile("STG_NCSpiste1_OTD_omssamergeOK.omx")
    val rs = omssaOmxFile.getResultSet(false)
    logger.debug("Splitting resultset")
    val acDecoyRegex = """REV_\S+""".r
    val (tRs, dRs) = fr.proline.core.algo.msi.TargetDecoyResultSetSplitter.split(rs, acDecoyRegex)
    logger.debug("number of target protein matches = " + tRs.proteinMatches.length)
    logger.debug("number of decoy protein matches  = " + dRs.proteinMatches.length)
    logger.debug("number of target peptide matches = " + tRs.peptideMatches.length)
    logger.debug("number of decoy peptide matches  = " + dRs.peptideMatches.length)
    logger.debug("number of target peptides        = " + tRs.peptides.length)
    logger.debug("number of decoy peptides         = " + dRs.peptides.length)
    logger.debug("TEST [" + method + "] OK: parsing is successful")
  }
  //  @Test
  //  def testCompareMatches {
  //    val method = getMethod()
  //    logger.debug("TEST [" + method + "] STARTS")
  //    val omssaOmxFile = parseOmxFile("E 040676.omx")
  //    val rs = omssaOmxFile.getResultSet(false)
  //    // get peptides matches
  //    var pepPeptideIds = new scala.collection.mutable.HashMap[Int, Int]()
  //    val peptideMatchesByPepId = rs.peptideMatches.groupBy(_.peptide.id)
  //    for ((peptideId, pepMatchGroup) <- (peptideMatchesByPepId)) {
  //      var bestPepMatch: fr.proline.core.om.model.msi.PeptideMatch = null
  //      if (pepMatchGroup.length == 1) { bestPepMatch = pepMatchGroup(0) }
  //      else { bestPepMatch = pepMatchGroup.toList.reduce { (a, b) => if (a.score > b.score) a else b } }
  //      pepPeptideIds.put(bestPepMatch.peptide.id, 1)
  //    }
  //    logger.debug("pepPeptideIds has " + pepPeptideIds.size + " items")
  //    // get protein matches
  //    var countEmptyProtSeqMatch = 0
  //    var protPeptideIds = new scala.collection.mutable.HashMap[Int, Int]()
  //    for (protMatch <- rs.proteinMatches) {
  //      if (protMatch.sequenceMatches.length == 0) { countEmptyProtSeqMatch += 1 }
  //      for (seqMatch <- protMatch.sequenceMatches) {
  //        protPeptideIds.put(seqMatch.getPeptideId, 1)
  //      }
  //    }
  //    logger.debug("protPeptideIds has " + protPeptideIds.size + " items")
  //    // compare
  //    for (item <- pepPeptideIds.keySet) {
  //      val opt = protPeptideIds.get(item)
  //      if (opt == None) { logger.debug("Peptide " + item + " from pepPeptideIds does not exist in protPeptideIds") }
  //    }
  //    for (item <- protPeptideIds.keySet) {
  //      val opt = pepPeptideIds.get(item)
  //      if (opt == None) { logger.debug("Peptide " + item + " from protPeptideIds does not exist in pepPeptideIds") }
  //    }
  //    logger.debug("rs has " + rs.peptideMatches.length + " peptideMatches and " + rs.proteinMatches.length + " proteinMatches (but " + countEmptyProtSeqMatch + " are empty)")
  //  }
  @Test
  def testCsvFile {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")
    // omssa can generate other formats than omx, such as csv
    // this should return an exception
    try {
      val omssaOmxFile = parseOmxFile("STG_NCSpiste1_OTD_standardFile.csv")
      // parsing should fail
      logger.debug("TEST [" + method + "] KO: parsing has not failed as expected !!")
    } catch {
      case e: IllegalArgumentException => logger.debug("TEST [" + method + "] OK: parsing has failed as expected")
      case e: Exception =>
        logger.debug("TEST [" + method + "] KO: parsing has failed for an unexpected reason : " + e, e)

        throw e
    }
  }
  @Test
  def testOmxFileWithoutExtension {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")
    // this file is the copy of STG_NCSpiste1_OTD_mgfInputFile.omx, but without the .omx extension
    // it should either pass the test, or fail due to the missing extension
    try {
      val omssaOmxFile = parseOmxFile("STG_NCSpiste1_OTD_standardOmxFileWithoutExtension")
      // parsing should fail due to  the missing omx extension
      logger.debug("TEST [" + method + "] KO: parsing has not failed as expected !!")
    } catch {
      case e: IllegalArgumentException => logger.debug("TEST [" + method + "] OK: parsing has failed as expected")
      case e: Exception =>
        logger.debug("TEST [" + method + "] KO: parsing has failed for an unexpected reason : " + e, e)

        throw e
    }
  }
  @Test
  def testShortFormat {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")
    // omssa omx files are actually xml files, where each line is indented by leading spaces
    // if these spaces are removed, an omx file can be 25% smaller (even more sometimes)
    // the parser should be able to read such files
    val omssaOmxFile = parseOmxFile("STG_NCSpiste1_OTD_shortFormat.omx")
    // parsing should succeed
    val rs = omssaOmxFile.getResultSet(false)
    assert(omssaOmxFile.getMsQueries.length == 18)
    assert(rs.peptideMatches.length == 2)
    assert(rs.proteinMatches.length == 2)
    logger.debug("TEST [" + method + "] OK: parsing is successful")
  }

  @Test
  def testUnimodMatch {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")
    // at least one ptm match in this file
    val omssaOmxFile = parseOmxFile("STG_NCSpiste1_OTD_unimodPtmMatches.omx")
    // the first match in the decoy resultset has at least one ptm match, read in the unimod file
    val rs = omssaOmxFile.getResultSet(true)
    for (peptideMatch <- rs.peptideMatches; ptm <- peptideMatch.peptide.ptms) {
      if (ptm.definition.names.fullName == "deamidation of N and Q") assert(ptm.definition.names.shortName == "Deamidated")
    }
    def eachSpectrumMatches(spectrumMatch: SpectrumMatch) = {
      try {

        for (fm <- spectrumMatch.fragMatches) {

          if (fm.label == "y(16)+++") {
            assertEquals("FragMatches.calculatedMoz", 1587.8581046599998, fm.calculatedMoz, MathUtils.EPSILON_LOW_PRECISION)
          }

        }

      } catch {
        case e: Exception =>
          logger.debug("eachSpectrumMatch error : " + e.getMessage())
          e.printStackTrace()
          throw e
      }
    }
    omssaOmxFile.eachSpectrumMatch(true, eachSpectrumMatches)
    logger.debug("TEST [" + method + "] OK: parsing is successful")
  }
  @Test
  def testUsermodMatch {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")
    // at least one user ptm match in this file
    try {
      val omssaOmxFile = parseOmxFile("STG_NCSpiste1_OTD_usermodMatches.omx")
      // parsing should fail because usermods are not in the database
      logger.debug("TEST [" + method + "] KO: parsing has not failed as expected !!")
    } catch {
      case e: UnknownPTMException => logger.debug("TEST [" + method + "] OK: parsing has failed as expected")
      case e: Exception =>
        logger.debug("TEST [" + method + "] KO: parsing has failed for an unexpected reason : " + e, e)

        throw e
    }
    //    val omssaOmxFile = parseOmxFile("STG_NCSpiste1_OTD_usermodMatches.omx")
    //    // all peptideMatches must match to the same ptm
    //    val rs = omssaOmxFile.getResultSet(false)
    //    for (peptideMatch <- rs.peptideMatches; ptm <- peptideMatch.peptide.ptms) { assert(ptm.definition.names.fullName == "BS3 linker + H2O") }
    //    logger.debug("TEST [" + method + "] OK: parsing is successful")
  }
  //
  //  /*
  //   * this file is from LSA and seems to contain at least one redundancy on quadruplet "protein_match_id, peptide_id, start, stop"
  //   * La clé ½ (protein_match_id, peptide_id, start, stop)=(35, 967485, 248, 264) ╗ existe déjà.
  //   * This test is commented because the file containing
  //   */
  //  @Test
  //  def testSequenceMatchRedundancyIssue {
  //    val method = getMethod()
  //    logger.debug("TEST [" + method + "] STARTS")
  //    val omssaOmxFile = parseOmxFile("MsMerge_piste mircroparticules.omx")
  //    val rs = omssaOmxFile.getResultSet(wantDecoy = false)
  //    logger.debug("rs.proteinMatches.length="+rs.proteinMatches.length)
  ////    assertEquals(2, rs.proteinMatches.length)
  //    logger.info("#proteinMatchId#peptideId#start#stop#")
  //    var count = 0
  //    for (pm <- rs.proteinMatches ; sm <- pm.sequenceMatches) {
  //	    if(pm.accession == "sp|REV_A6NJG2" && sm.start == 248 && sm.end == 264) {
  //	    	count += 1
  //	    	logger.info("#"+pm.id+"#"+sm.getPeptideId+"#"+sm.start+"#"+sm.end+"#")
  //	    	logger.info(pm.toString)
  //	    	logger.info(sm.toString)
  //	    }
  //    }
  //    assert(count == 1)
  //
  //    logger.debug("TEST [" + method + "] OK: parsing is successful")
  //  }

  //  @Test
  //  def testExtraHighOmssaScore {
  //    val method = getMethod()
  //    logger.debug("TEST [" + method + "] STARTS")
  //    val omssaOmxFile = parseOmxFile("STG_W11940CC_OTD.omx")
  //    val rs = omssaOmxFile.getResultSet(wantDecoy = false)
  ////    logger.debug("rs.proteinMatches.length="+rs.proteinMatches.length)
  ////    assertEquals(2, rs.proteinMatches.length)
  //    for (pm <- rs.peptideMatches) {
  //      if(pm.score > 40) {
  //        logger.debug("ABU found "+pm.toString)
  //      }
  //    }
  //    logger.debug("TEST [" + method + "] OK: parsing is successful")
  //  }

  @Test
  def testPtmPosition {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")
    val omssaOmxFile = parseOmxFile("STG_W18776LSA_OTD_pklInputFile.omx")
    val rs = omssaOmxFile.getResultSet(wantDecoy = false)
    val expectedResidues = new ArrayBuffer[Char]()

    val optionalMsiSearch = rs.msiSearch

    if ((optionalMsiSearch != null) && optionalMsiSearch.isDefined) {
      val msiSearch = optionalMsiSearch.get

      if (msiSearch.searchSettings.variablePtmDefs.length > 0) {
        logger.debug("PTM(s) expected : ")
        for (ptm <- msiSearch.searchSettings.variablePtmDefs) {
          expectedResidues += ptm.residue
          logger.debug(ptm.names.fullName + "(" + ptm.residue + ")")
        }
      }

    }

    try {
      for (pm <- rs.peptideMatches) {
        if (pm.peptide.ptms.length > 0) {
          logger.debug("PTM in sequence " + pm.peptide.sequence + " on amino acid(s) : ")
          for (ptm <- pm.peptide.ptms) {
            if (ptm.seqPosition == -1) logger.debug("C-term ptm")
            else if (ptm.seqPosition == 0) logger.debug("N-term ptm")
            else if (ptm.seqPosition > 0) {
              logger.debug(pm.peptide.sequence.charAt(ptm.seqPosition - 1).toString + " (" + ptm.seqPosition + ")")
              assert(expectedResidues.contains(pm.peptide.sequence.charAt(ptm.seqPosition - 1)))
            }
          }
        }
      }
      logger.debug("TEST [" + method + "] OK: parsing is successful")
    } catch {
      case e: Exception =>
        logger.error("TEST [" + method + "] FAILED", e)
        throw e
    }
  }

}
