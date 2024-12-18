package fr.proline.module.parser.omssa

import java.io.File
import java.io.FileNotFoundException

import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

import org.junit.After
import org.junit.Assert
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test

import fr.proline.context.BasicExecutionContext
import fr.proline.core.dal.AbstractMultipleDBTestCase
import fr.proline.core.dal.BuildDbConnectionContext
import fr.proline.core.dal.BuildMsiDbConnectionContext
import fr.proline.core.dal.BuildUdsDbConnectionContext
import fr.proline.core.om.model.msi.Ms2Query
import fr.proline.core.om.model.msi.Spectrum
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.IPTMProvider
import fr.proline.core.om.provider.msi.IPeptideProvider
import fr.proline.core.om.provider.msi.ResultFileProviderRegistry
import fr.proline.core.om.provider.msi.impl.SQLPTMProvider
import fr.proline.core.om.provider.msi.impl.SQLPeptideProvider
import fr.proline.core.service.msi.ResultFileCertifier
import fr.proline.repository.DriverType

@Test
class OmssaMSParserTest extends AbstractMultipleDBTestCase{

  val driverType = DriverType.H2

  val propertiesBuilder = Map.newBuilder[String, Any]
  propertiesBuilder += (OmssaParseParams.OMSSA_VERSION.toString -> "2.1.9")
  propertiesBuilder += (OmssaParseParams.USERMOD_XML_FILE.toString -> new File(this.getClass().getResource("/omssa_config/usermods.xml").toURI()))
  propertiesBuilder += (OmssaParseParams.FASTA_FILE_PATH.toString -> "")
  propertiesBuilder += (OmssaParseParams.FASTA_TAXONOMIES.toString -> "")
  propertiesBuilder += (OmssaParseParams.PEAK_LIST_FILE_PATH.toString -> "")
  propertiesBuilder += (OmssaParseParams.RAW_FILE_PATH.toString -> "")
  propertiesBuilder += (OmssaParseParams.PTM_COMPOSITION_FILE.toString -> new File(this.getClass().getResource("/compositions.txt").toURI()))
  val importProperties = propertiesBuilder.result

  var parserContext: ProviderDecoratedExecutionContext = null
  @Before
  def init() {
    logger.debug("Test initialization")
    super.initDBsDBManagement(driverType)

    //Load Data
    logger.info("Initializing Dbs")
    msiDBTestCase.loadDataSet("/default_datasets/Init_Dataset.xml")

    logger.info("MSI db succesfully initialized")

    udsDBTestCase.loadDataSet("/default_datasets/UDS_Simple_Dataset.xml")
    logger.info("UDS db succesfully initialized")

    val udsDbCtx = BuildUdsDbConnectionContext(dsConnectorFactoryForTest.getUdsDbConnector, true) // default: false
    val msiDbCtx = BuildMsiDbConnectionContext(dsConnectorFactoryForTest.getMsiDbConnector(1), true) // default: false

    val executionContext = new BasicExecutionContext(1L, udsDbCtx, msiDbCtx, null)

    parserContext = ProviderDecoratedExecutionContext(executionContext) // Use Object factory

    Assert.assertNotNull(parserContext)
  }

  private def getMethod(): String = {
    val st = Thread.currentThread.getStackTrace()
    st(2).getMethodName()
  }
  private def getCallingMethod(): String = {
    val st = Thread.currentThread.getStackTrace()
    st(3).getMethodName()
  }

  @After
  def closeResources() {
    logger.debug("Closing resources")
    super.tearDown
  }
  private def parseOmxFile(file: String): OmssaResultFile = {
    logger.info(" --- Get File " + file)
    var omxFile = new File(getClass.getResource("/omx_samples/" + file).toURI)
    logger.info(" --- OmssaMSParserTest  " + omxFile.exists)

    val resultFile = new OmssaResultFile(
      fileLocation = omxFile,
      parserContext = parserContext,
      importProperties = importProperties
    )
    import fr.proline.core.om.provider.uds.impl.{ SQLPeaklistSoftwareProvider => UdsSQLPklSoftProvider }
    val udsPklSoftProvider = new UdsSQLPklSoftProvider(parserContext.getUDSDbConnectionContext())
    resultFile.peaklistSoftware = udsPklSoftProvider.getPeaklistSoftware(2)

    resultFile
  }

  @Test
  def testVersion {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")
    val version = new fr.proline.module.parser.omssa.Version()
    logger.debug("Module name : "+version.getModuleName)
    logger.debug("Module version : "+version.getVersion)
    logger.debug("TEST [" + method + "] OK: versionning is successful")
  }
  
  @Test
  def testOmssaFileWithFixedPtm {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")
//    val omssaOmxFile = parseOmxFile("STG_NCSpiste1_OTD_fixedPtm.omx")
    val omssaOmxFile = parseOmxFile("E040XXX.omx.bz2")
    val rs = omssaOmxFile.getResultSet(wantDecoy = false)
//    assertEquals("Carbamidomethyl (C7)", rs.peptides.filter(_.sequence.equals("QLSHQLCVALDK")).head.readablePtmString)
    /* 
     * Selected fixed ptms:
     * Carbamidomethyl : ANYWHERE (C)
     * Dimethyl        : ANY_N_TERM
     * Ammonia-loss    : ANY_N_TERM (M)
     * Methyl          : ANY_C_TERM
     * Met->Hse        : ANY_C_TERM (M)
     * Acetyl          : PROT_N_TERM
     * Met-loss        : PROT_N_TERM (M)
     * 
     * Note that Methyl and Dimethyl will be present for every peptide
     */
    assertTrue(compareReadablePtmString(rs.peptides.filter(_.sequence.equals("TINEEQMRQCYVKITK")).head.readablePtmString, Array("Carbamidomethyl (C10)", "Methyl (Any C-term)", "Dimethyl (Any N-term)")))
    assertTrue(compareReadablePtmString(rs.peptides.filter(_.sequence.equals("NQILIFRIM")).head.readablePtmString, Array("Methyl (Any C-term)", "Dimethyl (Any N-term)", "Met->Hse (Any C-term)")))
    assertTrue(compareReadablePtmString(rs.peptides.filter(_.sequence.equals("YSRIAPPVTACDAENLTK")).head.readablePtmString, Array("Carbamidomethyl (C11)", "Acetyl (Protein N-term)", "Methyl (Any C-term)", "Dimethyl (Any N-term)")))
    assertTrue(compareReadablePtmString(rs.peptides.filter(_.sequence.equals("MTTKNLETK")).head.readablePtmString, Array("Met-loss (Protein N-term)", "Acetyl (Protein N-term)", "Methyl (Any C-term)", "Dimethyl (Any N-term)")))
    assertTrue(compareReadablePtmString(rs.peptides.filter(_.sequence.equals("DPRECVCMSGGICMCGDNCK")).head.readablePtmString, Array("Carbamidomethyl (C5)", "Carbamidomethyl (C7)", "Carbamidomethyl (C13)", "Carbamidomethyl (C15)", "Carbamidomethyl (C19)", "Met-loss (Protein N-term)", "Acetyl (Protein N-term)", "Methyl (Any C-term)", "Dimethyl (Any N-term)")))
    logger.debug("TEST [" + method + "] OK: parsing is successful")
  }
  
  private def compareReadablePtmString(readablePtmString: String, ptms: Array[String]): Boolean = {
    if(readablePtmString.equals(ptms.mkString(") ")))
      true
    else {
      if(readablePtmString.split("; ").sorted.mkString.equals(ptms.sorted.mkString)) {
        logger.warn("ReadablePtmString matches the expected readablePtmString but PTMs are sorted differently")
        true
      } else false
    }
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
//    val ionSeries: ArrayBuffer[String] = new ArrayBuffer[String]()
//    def eachSpectrumMatches(spectrumMatch: SpectrumMatch) = {
//      for (fm <- spectrumMatch.fragMatches) {
//        if (fm.label == "y(7)+") assert(fm.ionSeries == "y" && fm.aaPosition == 7 && fm.charge == 1)
//        else if (fm.label == "b(10)++") assert(fm.ionSeries == "b" && fm.aaPosition == 10 && fm.charge == 2)
//        else if (fm.label == "y(22)+++") assert(fm.ionSeries == "y" && fm.aaPosition == 22 && fm.charge == 3)
//      }
//    }
//    omssaOmxFile.eachSpectrumMatch(false, eachSpectrumMatches)
//    for (ion <- ionSeries) logger.debug(ion)
    logger.debug("TEST [" + method + "] OK: parsing is successful")
  }
  
  @Test
  def testPeaklistValues {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")
    // this file is a simple search that should be correct (it can be used to verify most of the algorithm)
    val omssaOmxFile = parseOmxFile("STG_NCSpiste1_OTD_mgfInputFile.omx")
    val rs = omssaOmxFile.getResultSet(wantDecoy = false)
    assertEquals(rs.msiSearch.get.peakList.fileType, "Mascot generic")
    assertEquals(rs.msiSearch.get.peakList.rawFileIdentifier, "STG_NCSpiste1_OTD_standardFile")
    logger.debug("TEST [" + method + "] OK: parsing is successful")
  }
  
  @Test
  def testBzippedFiles {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")
    // this file is a simple search that should be correct (it can be used to verify most of the algorithm)
    val omssaOmxFile = parseOmxFile("STG_NCSpiste1_OTD_bz2.omx.bz2")
    val rs = omssaOmxFile.getResultSet(wantDecoy = false)
    assertEquals(2, rs.proteinMatches.length)
    assertEquals(18, omssaOmxFile.msQueries.size)
    logger.debug("TEST [" + method + "] OK: parsing is successful")
  }

  /**
   * Temporary test
   * The composition file replaces the interface, it should disappear shortly
   */
  @Test
  def testResultFileVerifier_getIncompletePtmDefinitions {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")

    val storer = fr.proline.core.om.storer.msi.BuildPtmDefinitionStorer(parserContext.getMSIDbConnectionContext)
    val resultFileProvider = new OmssaResultFileProvider
    val verifier = resultFileProvider.getResultFileVerifier

    val ptmFile = new File(this.getClass().getResource("/omssa_config/usermods.xml").toURI())
    // fake properties, removed the composition file to force failure
    val properties = HashMap[String, Any](
      OmssaParseParams.OMSSA_VERSION.toString -> "2.1.9",
      OmssaParseParams.USERMOD_XML_FILE.toString -> ptmFile,
      OmssaParseParams.FASTA_FILE_PATH.toString -> "",
      OmssaParseParams.FASTA_TAXONOMIES.toString -> "",
      OmssaParseParams.PEAK_LIST_FILE_PATH.toString -> "",
      OmssaParseParams.RAW_FILE_PATH.toString -> "",
      OmssaParseParams.PTM_COMPOSITION_FILE.toString -> ""
    )

    val ptms = verifier.getPtmDefinitions(ptmFile, properties.toMap)
    try {
      storer.storePtmDefinitions(ptms.toSeq, parserContext)
      logger.debug("TEST [" + method + "] KO: parsing has not failed as expected !!")
      throw new Exception("Test " + method + " should have failed !!")
    } catch {
//      case e: IllegalArgumentException => logger.debug("TEST [" + method + "] OK: parsing has failed as expected: ", e)
      case e: IllegalArgumentException => logger.debug("TEST [" + method + "] OK: parsing has failed as expected: "+e.getMessage())
      case e: Exception =>
        logger.debug("TEST [" + method + "] KO: parsing has failed for an unexpected reason : " + e, e)
        throw e
    }
  }

  @Test
  def testResultFileVerifier_getPtmDefinitions {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")

    val storer = fr.proline.core.om.storer.msi.BuildPtmDefinitionStorer(parserContext.getMSIDbConnectionContext)
    val resultFileProvider = new OmssaResultFileProvider
    val verifier = resultFileProvider.getResultFileVerifier
    val ptms = verifier.getPtmDefinitions(new File(Thread.currentThread.getContextClassLoader.getResource("omssa_config/usermods.xml").getPath()), propertiesBuilder.result)
    //    try {
    storer.storePtmDefinitions(ptms.toSeq, parserContext)
    //    	logger.debug("TEST [" + method + "] KO: parsing has not failed as expected !!")
    logger.debug("TEST [" + method + "] OK: file is valid")
    //    	throw new Exception("Test "+method+" should have failed !!")
    //    } catch {
    //      case e: IllegalArgumentException => logger.debug("TEST [" + method + "] OK: parsing has failed as expected: ", e)
    //      case e: Exception =>
    //        logger.debug("TEST [" + method + "] KO: parsing has failed for an unexpected reason : " + e, e)
    //        throw e
    //    }
  }

  @Test
  def testFileType {
    val method = getMethod
    logger.debug("TEST [" + method + "] STARTS")
    
    ResultFileProviderRegistry.get(OmssaResultFileProviderType.fileType).get.getResultFile(
        fileLocation = new File(Thread.currentThread.getContextClassLoader.getResource("omx_samples/STG_NCSpiste1_OTD_mgfInputFile.omx").toURI()), 
        importProperties = propertiesBuilder.result, 
        parserContext = parserContext
    )
    logger.debug("TEST [" + method + "] OK: FileType is valid")
  }
  @Test
  def testResultFileVerifierWithProvider {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")
    //    try {
    ResultFileProviderRegistry.register(new OmssaResultFileProvider())
    var rfByFormat = Map("omssa.omx" -> Array(new File(Thread.currentThread.getContextClassLoader.getResource("omx_samples/STG_NCSpiste1_OTD_mgfInputFile.omx").getPath())))
    val certifier = new ResultFileCertifier(
      parserContext,
      resultIdentFilesByFormat = rfByFormat,
      importProperties = propertiesBuilder.result
    )
    logger.debug(" --- run service ")
    val result = certifier.runService()
    Assert.assertTrue(result)
    logger.debug("TEST [" + method + "] OK: file is valid")
  }

  @Test
  def testResultFileVerifier_isValid {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")
    val resultFileProvider = new OmssaResultFileProvider
    val verifier = resultFileProvider.getResultFileVerifier
    val valid = verifier.isValid(new File(Thread.currentThread.getContextClassLoader.getResource("omx_samples/STG_NCSpiste1_OTD_mgfInputFile.omx").toURI()), propertiesBuilder.result)
    assertEquals(true, valid)
    logger.debug("TEST [" + method + "] OK: file is valid")
  }

  @Test
  def testResultFileProvider {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")
    val resultFileProvider = new OmssaResultFileProvider

    val cl = Thread.currentThread.getContextClassLoader

    val resultFile = resultFileProvider.getResultFile(
      fileLocation = new File(cl.getResource("omx_samples/STG_NCSpiste1_OTD_mgfInputFile.omx").toURI()),
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
    // test hashmapped settings
    logger.debug("TEST [" + method + "] read hashmapped settings")
    val settings = omssaOmxFile.omssaSettingsInHashTable
    settings.foreach { case (key, value) => logger.debug("TEST:" + key + " => " + value) }
    assert(settings.get("MSSearchSettings_numisotopes").get == "0")
    assert(settings.get("MSSearchSettings_scale").get == "1000")
    assert(settings.get("MSSearchSettings_cutoff").get == "10")
    assert(settings.get("MSSearchSettings_db").get == "db/spHomo_DCpSP_ABU_20121206")
    assert(settings.get("MSSearchSettings_exactmass").get == "1446.94")
    // test mandatory files
    logger.debug("TEST [" + method + "] read mandatory files")
    val omssaLoader = omssaOmxFile.getOmssaLoader
    assert(omssaLoader.enzymes.size == 25)
    // test msQueryByInitialId
    logger.debug("TEST [" + method + "] calling msQueryByInitialId")
    assert(omssaOmxFile.msQueries.filter(_.initialId == 0).head.moz == 825.339)
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
    val rs = omssaOmxFile.getResultSet(wantDecoy = false)
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
        logger.debug("id = " + spectrum.id)
        logger.debug("title = " + spectrum.title)
        logger.debug("precursorMoz = " + spectrum.precursorMoz)
        logger.debug("precursorCharge = " + spectrum.precursorCharge)
        logger.debug("firstCycle = " + spectrum.firstCycle)
        logger.debug("lastCycle = " + spectrum.lastCycle)
        logger.debug("firstScan = " + spectrum.firstScan)
        logger.debug("lastScan = " + spectrum.lastScan)
        logger.debug("firstTime = " + spectrum.firstTime)
        logger.debug("lastTime = " + spectrum.lastTime)
        logger.debug("mozList = " + spectrum.mozList.get.mkString("#"))
        logger.debug("intensityList = " + spectrum.intensityList.get.mkString("#"))
        logger.debug("peaksCount = " + spectrum.peaksCount)
        logger.debug("fragRuleSetId = " + spectrum.fragmentationRuleSetId)
        logger.debug("peaklistId = " + spectrum.peaklistId)
        val intensities = spectrum.intensityList.get
        assert(intensities(0) == 5533260)
      }
    }
    omssaOmxFile.eachSpectrum(onEachSpectrum)
    logger.debug("TEST [" + method + "] OK : First intensity for compound 807 is correct (5533260)")
  }
//  @Test
  def testDtaInputFile {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")
    // omssa accepts dta files
    val omssaOmxFile = parseOmxFile("STG_LTQFT10APR1025_OTD_dtaInputFile.omx")
    // parsing should succeed
    val rs = omssaOmxFile.getResultSet(false)
    assert(omssaOmxFile.msQueries.length == 1)
    assert(rs.peptideMatches(0).getMs2Query.spectrumTitle == " Cmpd 1, +MSn(678.859), ? min")
    def onEachSpectrum(spectrum: Spectrum) = assert(spectrum.title == " Cmpd 1, +MSn(678.859), ? min")
    omssaOmxFile.eachSpectrum(onEachSpectrum)
    logger.debug("TEST [" + method + "] OK: parsing is successful")
  }
//  @Test
  def testPklInputFile {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")
    // omssa accepts dta files
    val omssaOmxFile = parseOmxFile("STG_W18776LSA_OTD_pklInputFile.omx")
    // parsing should succeed
    val rs = omssaOmxFile.getResultSet(false)
    assertEquals(15,omssaOmxFile.msQueries.length)
    //assertEquals(" Cmpd 5, +MSn(639.824), ? min",omssaOmxFile.getMsQueries(0).spectrumTitle )
//    assertEquals(" Cmpd 8, +MSn(616.806), ? min",omssaOmxFile.msQueries(0).spectrumTitle )
    assertEquals(" Cmpd 8, +MSn(616.806), ? min",omssaOmxFile.msQueries(0).asInstanceOf[Ms2Query].spectrumTitle )
    assertEquals(10,rs.peptideMatches.length)
    assertEquals(29,rs.proteinMatches.length)
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
    // check error tolerance units
    assert(rsDalton.msiSearch.get.searchSettings.ms1ErrorTolUnit.equals("Da"))
    assert(rsDalton.msiSearch.get.searchSettings.msmsSearchSettings.get.ms2ErrorTolUnit.equals("Da"))
    assert(rsPpm.msiSearch.get.searchSettings.ms1ErrorTolUnit.equals("Da"))
    assert(rsPpm.msiSearch.get.searchSettings.msmsSearchSettings.get.ms2ErrorTolUnit.equals("Ppm"))
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
  def testMissedCleavages {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")
    val omssaOmxFile = parseOmxFile("STG_NCSpiste1_OTD_miscleavages.omx")
    // parsing should succeed
    val rs = omssaOmxFile.getResultSet(false)
    val enzymeName = rs.msiSearch.get.searchSettings.usedEnzymes.head.name
    rs.peptideMatches.foreach(pm => {
      if(pm.peptide.sequence == "YPGFLDVRAAPLLADDNKLR") {
        logger.debug(pm.peptide.sequence + " has " + pm.missedCleavage + " missed cleavages with enzyme="+enzymeName)
        assert(pm.missedCleavage == 2)
      } else if(pm.peptide.sequence == "EGQELGTADCSVPGDPEGLETAK") {
        logger.debug(pm.peptide.sequence + " has " + pm.missedCleavage + " missed cleavages with enzyme="+enzymeName)
        assert(pm.missedCleavage == 0)
      }
    })
    logger.debug("TEST [" + method + "] OK: parsing is successful")
  }
  @Test
  def testChymotrypsin {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")
    val omssaOmxFile = parseOmxFile("STG_NCSpiste1_OTD_chymotrypsin.omx")
    // parsing should succeed
    val rs = omssaOmxFile.getResultSet(false)
    val enzymeName = rs.msiSearch.get.searchSettings.usedEnzymes.head.name
    logger.debug("Enzyme="+enzymeName)
    logger.debug("TEST [" + method + "] OK: parsing is successful")
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
    assert(omssaOmxFile.msQueries.length == 18)
    assert(rs.peptideMatches.length == 2)
    assert(rs.proteinMatches.length == 2)
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
  }

  @Test
  def testUnknownEnzyme() = {
    val method = getMethod()
    logger.debug("TEST [" + method + "] STARTS")
    ResultFileProviderRegistry.register(new OmssaResultFileProvider())
    var rfByFormat = Map("omssa.omx" -> Array(new File(Thread.currentThread.getContextClassLoader.getResource("omx_samples/STG_NCSpiste1_OTD_unknownEnzyme.omx").getPath())))
    val certifier = new ResultFileCertifier(
      parserContext,
      resultIdentFilesByFormat = rfByFormat,
      importProperties = propertiesBuilder.result
    )
    logger.debug(" --- run service ")
    val result = certifier.runService()
    
    logger.debug(" --- parse file ")
    val omssaOmxFile = parseOmxFile("STG_NCSpiste1_OTD_unknownEnzyme.omx")
    val rs = omssaOmxFile.getResultSet(wantDecoy = false)
    val e: fr.profi.chemistry.model.Enzyme = rs.msiSearch.get.searchSettings.usedEnzymes.head
    assert(e.name == "gluc-de")
    assert(e.isSemiSpecific == false)
    assert(e.enzymeCleavages.size == 1)
    assert(e.enzymeCleavages.head.site == "C-term")
    assert(e.enzymeCleavages.head.residues == "ED")
    assert(e.enzymeCleavages.head.restrictiveResidues == None)
    logger.debug("TEST [" + method + "] OK: parsing is successful")
  }

}
