package fr.proline.core.service.msi

import java.io.File

import scala.collection.mutable.{HashMap, ArrayBuffer}

import org.junit.{After, Assert, Test, Before}

import com.weiglewilczek.slf4s.Logging

import fr.proline.context.IExecutionContext
import fr.proline.core.algo.msi.filtering.pepmatch.{ScorePSMFilter, RankPSMFilter, _}
import fr.proline.core.algo.msi.filtering.proteinset.{ScoreProtSetFilter, SpecificPeptidesPSFilter}
import fr.proline.core.algo.msi.filtering.{IPeptideMatchFilter, FilterPropertyKeys, _}
import fr.proline.core.algo.msi.validation.pepmatch.TDPepMatchValidatorWithFDROptimization
import fr.proline.core.algo.msi.validation.proteinset.ProtSetRulesValidatorWithFDROptimization
import fr.proline.core.algo.msi.validation.{BasicTDAnalyzer, _}
import fr.proline.core.algo.msi.InferenceMethods
import fr.proline.core.algo.msi.scoring.ProtSetScoring
import fr.proline.core.dal.{SQLQueryHelper, SQLConnectionContext}
import fr.proline.core.om.model.msi.{ResultSet, PeptideMatch, FilterDescriptor}
import fr.proline.core.om.provider.msi.impl.{SQLResultSetProvider, ORMResultSetProvider}
import fr.proline.core.om.provider.msi.IResultSetProvider
import fr.proline.core.om.storer.msi.impl.StorerContext
import fr.proline.repository.DriverType

class ResultSetValidatorsTest extends AbstractRFImporterTest_ with Logging {

  val driverType = DriverType.H2
  var executionContext: IExecutionContext = null
  var rsIDWork: Int = 0
  var rsProvider: IResultSetProvider = null

  // TODO: load the file only once (BeforeClass ?)
  // TODO: do we need really to use the storer ?
  @Before
  @throws(classOf[Exception])
  override def setUp() = {
    super.setUp()

    _datFileName = "/dat_samples/STR_F136482_CTD.dat"
    udsDBTestCase.loadDataSet("/fr/proline/module/parser/mascot/UDS_Simple_Dataset.xml")
    logger.info("UDS db succesfully initialized")

    val (execContext, rsProv) = buildJPAContext() //SQLContext()    
    executionContext = execContext
    rsProvider = rsProv

  }

  private def importDatFile(datFileClassPath: String, decoyRegExp : String): Unit = {
    logger.debug(" --- Load Mascot File " + datFileClassPath)

    var datFile: File = new File(getClass.getResource(datFileClassPath).toURI)

    val propertiedBuilder = Map.newBuilder[String, Any]
    propertiedBuilder += ("ion.score.cutoff" -> 0.0)
    propertiedBuilder += ("subset.threshold" -> 1.0)

    val importer: ResultFileImporterJPAStorer = new ResultFileImporterJPAStorer(
      executionContext,
      resultIdentFile = datFile,
      fileType = "MascotMSParser",
      instrumentConfigId = 1,
      peaklistSoftwareId = 1, // TODO : provide the right value
      importerProperties = propertiedBuilder.result,
      acDecoyRegex = if(decoyRegExp == null) None else Some(decoyRegExp.r))

    val result = importer.runService()
    rsIDWork = importer.getTargetResultSetId

    logger.debug(" ResultFile  loaded (" + result + ") from " + datFile.getAbsolutePath() + " with target resultID " + rsIDWork)
  }

  @After
  override def tearDown() {
    if (executionContext != null)
      executionContext.closeAll()
    super.tearDown()
  }
  
 @Test
  def testScoreValidationOnNoneDecoy() = {
   importDatFile("/dat_samples/STR_F122817_Mascot_v2.3.dat",null)
   val nbrPepProteo = 1
    val scoreTh = 22.0f
    val pepFilters = Seq(new ScorePSMFilter(scoreThreshold = scoreTh))
    val protProteoTypiqueFilters = Seq()
    
    val rsValidation = ResultSetValidator(
      execContext = executionContext,
      targetRsId = rsIDWork,
      tdAnalyzer = Some(new BasicTDAnalyzer(TargetDecoyModes.CONCATENATED)),
      pepMatchPreFilters = Some(pepFilters),
      pepMatchValidator = None,
      protSetFilters = Some(protProteoTypiqueFilters),
      storeResultSummary = false)

    val result = rsValidation.runService
    Assert.assertTrue("ResultSet validation result", result)
    logger.info(" End Run ResultSetValidator Service with Score Filter, in Test ")

    val tRSM = rsValidation.validatedTargetRsm
    val dRSM = rsValidation.validatedDecoyRsm
    logger.info(" rsValidation.validatedTargetRsm "+tRSM.id+" rsValidation.validatedDecoyRsm "+dRSM.isDefined)
    Assert.assertNotNull(tRSM)
    Assert.assertNotNull(dRSM)
    
//    Assert.assertFalse(dRSM.isDefined)

  }

 
  @Test
  def testScoreValidation() = {
    importDatFile(_datFileName,"""sp\|REV_\S+""")

    val scoreTh = 22.0f
    val pepFilters = Seq(new ScorePSMFilter(scoreThreshold = scoreTh))

    val rsValidation = ResultSetValidator(
      execContext = executionContext,
      targetRsId = rsIDWork,
      tdAnalyzer = Some(new BasicTDAnalyzer(TargetDecoyModes.CONCATENATED)),
      pepMatchPreFilters = Some(pepFilters),
      pepMatchValidator = None,
      protSetFilters = None,
      storeResultSummary = false)

    val result = rsValidation.runService
    Assert.assertTrue("ResultSet validation result", result)
    logger.info(" End Run ResultSetValidator Service with Score Filter, in Test ")

    val tRSM = rsValidation.validatedTargetRsm
    val dRSM = rsValidation.validatedDecoyRsm

    Assert.assertNotNull(tRSM)
    Assert.assertNotNull(dRSM)
    Assert.assertTrue(dRSM.isDefined)

    Assert.assertTrue(tRSM.properties.isDefined)
    Assert.assertTrue(tRSM.properties.get.getValidationProperties.get.getParams.getPeptideFilters.isDefined)

    val pepFilterProps = tRSM.properties.get.getValidationProperties.get.getParams.getPeptideFilters.get
    Assert.assertEquals(1, pepFilterProps.size)

    val fPrp: FilterDescriptor = pepFilterProps(0)
    val props = fPrp.getProperties.get
    Assert.assertEquals(1, props.size)
    Assert.assertEquals(new ScorePSMFilter().filterDescription, fPrp.getDescription.get)
    Assert.assertEquals("ScoreTh float from properties Map",props(FilterPropertyKeys.THRESHOLD_VALUE), scoreTh)

    val pepValResultsOpt = rsValidation.validatedTargetRsm.properties.get.getValidationProperties.get.getResults.getPeptideResults
    Assert.assertTrue(pepValResultsOpt.isDefined)
    val pepValResults = pepValResultsOpt.get
    Assert.assertEquals(438, pepValResults.getTargetMatchesCount)
    Assert.assertEquals(251, pepValResults.getDecoyMatchesCount.get)
    Assert.assertEquals(72.86, pepValResults.getFdr.get, 0.01)

    logger.debug(" Verify Result IN RSM ")
    val allTarPepMatc = rsValidation.validatedTargetRsm.peptideInstances.flatMap(pi => pi.peptideMatches)
    val allDecPepMatc = rsValidation.validatedDecoyRsm.get.peptideInstances.flatMap(pi => pi.peptideMatches)
    Assert.assertEquals("AllTarPepMatc length", 438, allTarPepMatc.length)
    Assert.assertEquals("AllTarPepMatc length", 251, allDecPepMatc.length)

    rsValidation.validatedTargetRsm.peptideInstances.foreach(pepInst => {
      pepInst.peptideMatches.foreach(peptideM => {
        Assert.assertTrue("PeptideMatch is validated", peptideM.isValidated)
        Assert.assertTrue("PeptideMatch.score > scoreTh",peptideM.score > scoreTh)
      })
    })
  }
  
   @Test
  def testRankValidation() = {
    importDatFile(_datFileName,"""sp\|REV_\S+""")

    val readRS = rsProvider.getResultSet(rsIDWork).get
    val seqBuilder = Seq.newBuilder[IPeptideMatchFilter]
    val rank = 1
    seqBuilder += new RankPSMFilter(pepMatchMaxRank = 1)
    val rsValidation = new ResultSetValidator(
      execContext = executionContext,
      targetRs = readRS,
      tdAnalyzer = Some(new BasicTDAnalyzer(TargetDecoyModes.CONCATENATED)),
      pepMatchPreFilters = Some(seqBuilder.result()),
      pepMatchValidator = None,
      protSetFilters = None, 
      protSetValidator = None,
      storeResultSummary = false
      )

    val result = rsValidation.runService
    Assert.assertTrue(result)
    logger.info(" End Run ResultSetValidator Service with Rank filter, in Test ")

    Assert.assertNotNull(rsValidation.validatedTargetRsm)
    Assert.assertTrue(rsValidation.validatedDecoyRsm.isDefined)

    logger.debug(" Verify Result IN RSM ")
    val allTarPepMatc = rsValidation.validatedTargetRsm.peptideInstances.flatMap(pi => pi.peptideMatches)
    val allDecPepMatc = rsValidation.validatedDecoyRsm.get.peptideInstances.flatMap(pi => pi.peptideMatches)
    Assert.assertEquals("AllTarPepMatc length", 774, allTarPepMatc.length)
    Assert.assertEquals("AllDecPepMatc length",638, allDecPepMatc.length)

    val pepMatchByQuId = new HashMap[Int, ArrayBuffer[PeptideMatch]]()
    allTarPepMatc.foreach(peptideM => {
      val pepMatches = pepMatchByQuId.get(peptideM.msQueryId).getOrElse(new ArrayBuffer[PeptideMatch]())
      //             System.out.println(peptideM.msQueryId+"\t"+peptideM.peptide.sequence+"\t"+peptideM.peptide.ptmString+"\t"+peptideM.score)
      pepMatches + peptideM
      Assert.assertTrue(peptideM.isValidated)
      pepMatchByQuId.put(peptideM.msQueryId, pepMatches)
    })

    allDecPepMatc.foreach(peptideM => {
      val pepMatches = pepMatchByQuId.get(peptideM.msQueryId).getOrElse(new ArrayBuffer[PeptideMatch]())
      pepMatches + peptideM
      //             System.out.println(peptideM.msQueryId+"\t"+peptideM.peptide.sequence+"\t"+peptideM.peptide.ptmString)
      Assert.assertTrue(peptideM.isValidated)
      pepMatchByQuId.put(peptideM.msQueryId, pepMatches)
    })

    pepMatchByQuId.foreach(entry => {
      var validatedEntry = false
      if (entry._2.length.equals(1)) {
        validatedEntry = true
      } else {
        var index = 0
        while (index < entry._2.length - 1) {
          validatedEntry = (entry._2(index).score - entry._2(index + 1).score).abs < 0.1
          index += 1
        }
      }

      Assert.assertTrue(validatedEntry)
    })

    logger.debug(" ResultSetValidator testRankValidation test properties")
    val rsmPropTargetCount = rsValidation.validatedTargetRsm.properties.get.getValidationProperties.get.getResults.getPeptideResults.get.getTargetMatchesCount
    val rsmPropDecoyCount = rsValidation.validatedTargetRsm.properties.get.getValidationProperties.get.getResults.getPeptideResults.get.getDecoyMatchesCount

    Assert.assertEquals(" RSM validation properties target count ",allTarPepMatc.length,rsmPropTargetCount)
    Assert.assertEquals(" RSM validation properties target count ", allDecPepMatc.length, rsmPropDecoyCount.get)

    val rsPepMatchByQuId = new HashMap[Int, ArrayBuffer[PeptideMatch]]()
    val rsPsm = readRS.peptideMatches ++ readRS.decoyResultSet.get.peptideMatches
    rsPsm.foreach(peptideM => {
      val pepMatches = rsPepMatchByQuId.get(peptideM.msQueryId).getOrElse(new ArrayBuffer[PeptideMatch]())
      pepMatches += (peptideM)
      rsPepMatchByQuId.put(peptideM.msQueryId, pepMatches)
    })

    rsPepMatchByQuId.foreach(entry => {
      val psmEntry = entry._2.sortWith((a, b) => a.score > b.score)
      var firstPSMScore = psmEntry(0).score
      entry._2.foreach(psm => {
        //logger.debug(" -- QID "+entry._1+" PSM "+psm.peptide.sequence+" firstPSMScore "+firstPSMScore+" <> "+psm.score+"  " +psm.isValidated+" ( "+(firstPSMScore - psm.score).abs+" )")
        if ((firstPSMScore - psm.score).abs >= 0.1)
          Assert.assertFalse(psm.isValidated)
        else
          Assert.assertTrue(psm.isValidated)
      })
    })

  }
    
  //@Test
  def testRankValidationWithCompetitionFDR() = {
    importDatFile(_datFileName,"""sp\|REV_\S+""")

    val readRS = rsProvider.getResultSet(rsIDWork).get
    val seqBuilder = Seq.newBuilder[IPeptideMatchFilter]
    val rank = 1
    seqBuilder += new RankPSMFilter(pepMatchMaxRank = 1)
    val seqFilters = seqBuilder.result
    val rsValidation = new ResultSetValidator(
      execContext = executionContext,
      targetRs = readRS,
      tdAnalyzer = Some(new CompetitionBasedTDAnalyzer(new ScorePSMFilter())), //sort PSM using score
      pepMatchPreFilters = Some(seqFilters),
      pepMatchValidator = None,
      protSetFilters = None,
      protSetValidator = None,
      storeResultSummary = false
      )

    val result = rsValidation.runService
    Assert.assertTrue(result)
    logger.info(" End Run ResultSetValidator Service with Rank filter, in Test ")

    Assert.assertNotNull(rsValidation.validatedTargetRsm)
    Assert.assertTrue(rsValidation.validatedDecoyRsm.isDefined)

    logger.debug(" Verify Result IN RSM ")
    val allTarPepMatc = rsValidation.validatedTargetRsm.peptideInstances.flatMap(pi => pi.peptideMatches)
    val allDecPepMatc = rsValidation.validatedDecoyRsm.get.peptideInstances.flatMap(pi => pi.peptideMatches)
    val allPepMatc  = allTarPepMatc++allDecPepMatc
    Assert.assertEquals("AllTarPepMatc length", 774, allTarPepMatc.length)
    Assert.assertEquals("AllDecPepMatc length",638, allDecPepMatc.length)
    Assert.assertEquals("All PepMatc length",638+774, allPepMatc.length)
    
    val pepMatchByQuId = new HashMap[Int, ArrayBuffer[PeptideMatch]]()
    allPepMatc.foreach(peptideM => {
      val pepMatches = pepMatchByQuId.get(peptideM.msQueryId).getOrElse(new ArrayBuffer[PeptideMatch]())
      //             System.out.println(peptideM.msQueryId+"\t"+peptideM.peptide.sequence+"\t"+peptideM.peptide.ptmString+"\t"+peptideM.score)
      pepMatches + peptideM
      Assert.assertTrue(peptideM.isValidated)
      pepMatchByQuId.put(peptideM.msQueryId, pepMatches)
    })

    pepMatchByQuId.foreach(entry => {
      var validatedEntry = false
      if (entry._2.length.equals(1)) {
        validatedEntry = true
      } else {
        var index = 0
        while (index < entry._2.length - 1) {
          validatedEntry = (entry._2(index).score - entry._2(index + 1).score).abs < 0.1
          index += 1
        }
      }
      Assert.assertTrue(validatedEntry)
    })
    
    logger.debug(" ResultSetValidator testRankValidationWithCompetitionFDR test properties")
    val rsmPropTargetCount = rsValidation.validatedTargetRsm.properties.get.getValidationProperties.get.getResults.getPeptideResults.get.getTargetMatchesCount
    val rsmPropDecoyCount = rsValidation.validatedTargetRsm.properties.get.getValidationProperties.get.getResults.getPeptideResults.get.getDecoyMatchesCount

    Assert.assertEquals(" RSM validation properties target count ",allTarPepMatc.length,rsmPropTargetCount)
    Assert.assertEquals(" RSM validation properties decoy count ", allDecPepMatc.length, rsmPropDecoyCount.get)

  }

  @Test
  def testScoreFDRValidation() = {

    importDatFile(_datFileName,"""sp\|REV_\S+""")

    val testTDAnalyzer = Some(new BasicTDAnalyzer(TargetDecoyModes.CONCATENATED))
    val fdrValidator = new TDPepMatchValidatorWithFDROptimization(
      validationFilter = new ScorePSMFilter(),
      expectedFdr = Some(7.0f),
      tdAnalyzer = testTDAnalyzer
      )
    //    ComputedFDRPeptideMatchFilter( 1.0F, new ScorePSMFilter() )
    logger.info(" ResultSetValidator testScoreFDRValidation Create service")
    val rsValidation = ResultSetValidator(
      execContext = executionContext,
      targetRsId = rsIDWork,
      tdAnalyzer = testTDAnalyzer,
      pepMatchPreFilters = None,
      pepMatchValidator = Some(fdrValidator),
      protSetFilters = None,
      storeResultSummary = true)

    logger.debug(" ResultSetValidator testScoreFDRValidation RUN  service")
    val result = rsValidation.runService
    Assert.assertTrue(result)
    val rsmID = rsValidation.validatedTargetRsm.id
    Assert.assertTrue(" ResultSummary was saved (positive id) ",rsmID>1)
    logger.debug(" End Run ResultSetValidator Service with FDR filter using Score, in Test ")

    Assert.assertNotNull(rsValidation.validatedTargetRsm)
    Assert.assertTrue(rsValidation.validatedDecoyRsm.isDefined)
    Assert.assertTrue(rsValidation.validatedTargetRsm.properties.isDefined)

    val pepFilterPropsOpt = rsValidation.validatedTargetRsm.properties.get.getValidationProperties.get.getParams.getPeptideFilters
    Assert.assertTrue(pepFilterPropsOpt.isDefined)
    val pepFilterProps = pepFilterPropsOpt.get
    Assert.assertEquals(1, pepFilterProps.size)
    val fPrp: FilterDescriptor = pepFilterProps(0)
    val props = fPrp.getProperties.get
    Assert.assertEquals(1, props.size)
    Assert.assertEquals(new ScorePSMFilter().filterDescription, fPrp.getDescription.get)

    val scoreThresh = props(FilterPropertyKeys.THRESHOLD_VALUE).asInstanceOf[Float]
    Assert.assertEquals("ScoreThresh float compare",52.89, scoreThresh, 0.01)

    Assert.assertEquals(7.01f, rsValidation.validatedTargetRsm.properties.get.getValidationProperties.get.getResults.getPeptideResults.get.getFdr.get, 0.01)

    logger.debug(" Verify Result IN RSM ")
    val allTarPepMatc = rsValidation.validatedTargetRsm.peptideInstances.flatMap(pi => pi.peptideMatches)
    val allDecPepMatc = rsValidation.validatedDecoyRsm.get.peptideInstances.flatMap(pi => pi.peptideMatches)
    Assert.assertEquals(55, allTarPepMatc.length)
    Assert.assertEquals(2, allDecPepMatc.length)

    allTarPepMatc.foreach(peptideM => {
      //             System.out.println(peptideM.msQueryId+"\t"+peptideM.peptide.sequence+"\t"+peptideM.peptide.ptmString+"\t"+peptideM.score)
      Assert.assertTrue(peptideM.isValidated)
      Assert.assertTrue(peptideM.score > scoreThresh)
    })

    allDecPepMatc.foreach(peptideM => {
      //             System.out.println(peptideM.msQueryId+"\t"+peptideM.peptide.sequence+"\t"+peptideM.peptide.ptmString+"\t"+peptideM.score)
      Assert.assertTrue(peptideM.isValidated)
      Assert.assertTrue(peptideM.score > scoreThresh)
    })

  }
  
  @Test
  def testProtPrototypiquePSMValidation() = {

  importDatFile(_datFileName,"""sp\|REV_\S+""")
  //   importDatFile("/dat_samples/F067920.dat","""###REV###\S+""")
     
    val testTDAnalyzer = Some(new BasicTDAnalyzer(TargetDecoyModes.CONCATENATED))
    val scoreTh = 22.0f
    val nbrPepProteo = 1
    val pepFilters = Seq(new ScorePSMFilter(scoreThreshold = scoreTh))
    val protProteoTypiqueFilters = Seq(new SpecificPeptidesPSFilter(nbrPepProteo))
    
    logger.info(" ResultSetValidator testProtPrototypiquePSMValidation Create service")
    val rsValidation = ResultSetValidator(
      execContext = executionContext,
      targetRsId = rsIDWork,
      tdAnalyzer = Some(new BasicTDAnalyzer(TargetDecoyModes.CONCATENATED)),
      pepMatchPreFilters =  Some(pepFilters),
      pepMatchValidator = None,
      protSetFilters = Some(protProteoTypiqueFilters),
      storeResultSummary = false)
      
      logger.debug(" ResultSetValidator testProtPrototypiquePSMValidation RUN  service")
      val result = rsValidation.runService
      Assert.assertTrue(result)      
      logger.debug(" End Run ResultSetValidator Service with FDR filter using Score, in Test ")

    Assert.assertNotNull(rsValidation.validatedTargetRsm)
    Assert.assertTrue(rsValidation.validatedDecoyRsm.isDefined)
    Assert.assertTrue(rsValidation.validatedTargetRsm.properties.isDefined)

    val protFilterPropsOpt = rsValidation.validatedTargetRsm.properties.get.getValidationProperties.get.getParams.getProteinFilters
    Assert.assertTrue(protFilterPropsOpt.isDefined)
    val protFilterProps = protFilterPropsOpt.get
    Assert.assertEquals(1, protFilterProps.size)
    val fPrp: FilterDescriptor = protFilterProps(0)
    val props = fPrp.getProperties.get
    Assert.assertEquals(1, props.size)
    Assert.assertEquals(ProtSetFilterParams.SPECIFIC_PEP.toString, fPrp.getParameter)

    val nbrPep = props(FilterPropertyKeys.THRESHOLD_VALUE).asInstanceOf[Int]
    Assert.assertEquals("Proteotypique peptide # compare",nbrPepProteo, nbrPep)

    logger.debug(" Verify Result IN RSM ")
    val removedTarProtSet2 = rsValidation.validatedTargetRsm.proteinSets.filter(!_.isValidated)
    val removedDecProtSet2= rsValidation.validatedDecoyRsm.get.proteinSets.filter(!_.isValidated)
    
    val validatedTarProtSet = rsValidation.validatedTargetRsm.proteinSets.filter(_.isValidated)
    val validatedDecProtSet = rsValidation.validatedDecoyRsm.get.proteinSets.filter(_.isValidated)
    
    val allTarProtSet = rsValidation.validatedTargetRsm.proteinSets
    val allDecProtSet = rsValidation.validatedDecoyRsm.get.proteinSets
    logger.debug(" All Target ProtSet (even not validated) "+allTarProtSet.length+" <>  removed Target ProtSet "+  removedTarProtSet2.length)
    logger.debug(" All Decoy ProtSet (even not validated)  "+allDecProtSet.length+" <>  removed Decoy ProtSet "+  removedDecProtSet2.length)
//    Assert.assertEquals("allTarProtSet length", 4, allTarProtSet.length) //VDS DEBUG Pour test final, threshold 2... 
//    Assert.assertEquals("allDecProtSet length", 1, allDecProtSet.length)
    removedTarProtSet2.foreach(protSet => {
      //DEBUG ONLY 
       logger.debug("---- Removed Protein Set ------ ")
	val firstPrtMatch =  rsValidation.validatedTargetRsm.resultSet.get.proteinMatches.filter(_.id == protSet.proteinMatchIds(0))(0)
        System.out.println(firstPrtMatch.accession+"\t"+protSet.peptideSet.peptideMatchesCount+"\t"+protSet.isValidated)
        protSet.peptideSet.getPeptideInstances.foreach(pepIns =>{
	  System.out.println("\t"+"\t"+pepIns.peptide.sequence+"\t"+pepIns.peptide.ptmString+"\t"+pepIns.proteinSetsCount)  
        })    
        logger.debug(" Removed Protein Set - unique pep # "+protSet.peptideSet.getPeptideInstances.filter(_.proteinSetsCount == 1).length)
//        Assert.assertTrue("Protein Set more than 1 unique pep", protSet.peptideSet.getPeptideInstances.filter(_.proteinSetsCount == 1).length >= 2 )
    })
    
    validatedTarProtSet.foreach(protSet => {
      //DEBUG ONLY 
       logger.debug("---- validatedTarProtSet  ------ ")
	val firstPrtMatch =  rsValidation.validatedTargetRsm.resultSet.get.proteinMatches.filter(_.id == protSet.proteinMatchIds(0))(0)
        System.out.println(firstPrtMatch.accession+"\t"+protSet.peptideSet.peptideMatchesCount+"\t"+protSet.isValidated)
        protSet.peptideSet.getPeptideInstances.foreach(pepIns =>{
	  System.out.println("\t"+"\t"+pepIns.peptide.sequence+"\t"+pepIns.peptide.ptmString+"\t"+pepIns.proteinSetsCount)  
        })    
        logger.debug(" Protein Set - unique pep # "+protSet.peptideSet.getPeptideInstances.filter(_.proteinSetsCount == 1).length)
//        Assert.assertTrue("Protein Set more than 1 unique pep", protSet.peptideSet.getPeptideInstances.filter(_.proteinSetsCount == 1).length >= 2 )
    })
    
    validatedDecProtSet.foreach(protSet => {
      //DEBUG ONLY 
             logger.debug("---- validatedDecProtSet  ------ ")
	val firstPrtMatch =  rsValidation.validatedDecoyRsm.get.resultSet.get.proteinMatches.filter(_.id == protSet.proteinMatchIds(0))(0)
        System.out.println(firstPrtMatch.accession+"\t"+protSet.peptideSet.peptideMatchesCount+"\t"+protSet.isValidated)
        protSet.peptideSet.getPeptideInstances.foreach(pepIns =>{
	  System.out.println("\t"+"\t"+pepIns.peptide.sequence+"\t"+pepIns.peptide.ptmString+"\t"+pepIns.proteinSetsCount)  
        })    
        logger.debug(" Protein Set - unique pep # "+protSet.peptideSet.getPeptideInstances.filter(_.proteinSetsCount == 1).length)
//        Assert.assertTrue("Protein Set more than 1 unique pep", protSet.peptideSet.getPeptideInstances.filter(_.proteinSetsCount == 1).length >= 2 )
    })
  }

  @Test
  def testRankAndScoreFDRValidation() = {

    importDatFile(_datFileName,"""sp\|REV_\S+""")

    val firstRankFilter = new RankPSMFilter(1)
    val valFilter = new ScorePSMFilter()
    val testTDAnalyzer = Some(new CompetitionBasedTDAnalyzer(valFilter))
//    val testTDAnalyzer = Some(new BasicTDAnalyzer(TargetDecoyModes.CONCATENATED))
    val fdrValidator = new TDPepMatchValidatorWithFDROptimization(
      validationFilter = valFilter,
      expectedFdr = Some(7.0f),
      tdAnalyzer = testTDAnalyzer)

    logger.info("ResultSetValidator testRankAndScoreFDRValidation Create service")
    val rsValidation = ResultSetValidator(
      execContext = executionContext,
      targetRsId = rsIDWork,
      tdAnalyzer = testTDAnalyzer,
      pepMatchPreFilters = Some(Seq(firstRankFilter)),
      pepMatchValidator = Some(fdrValidator),
      protSetFilters = None,
      protSetValidator = None,
      storeResultSummary = false
      )

    logger.debug("ResultSetValidator testRankAndScoreFDRValidation RUN service")
    val result = rsValidation.runService
    Assert.assertTrue(result)
    logger.debug("End Run ResultSetValidator Service with FDR filter using Rank and Score, in Test ")

    logger.debug("Verify Result IN RS")
    val rsTarPepMatches = rsValidation.validatedTargetRsm.resultSet.get.peptideMatches
    val rsDecPepMatches = rsValidation.validatedDecoyRsm.get.resultSet.get.peptideMatches
    Assert.assertEquals("RsTarPepMatches validated count", 102, rsTarPepMatches.count(_.isValidated))
    Assert.assertEquals("RsDecPepMatches validated count", 16, rsDecPepMatches.count(_.isValidated))

    logger.debug("Verify Result IN RSM")
    val allTarPepMatc = rsValidation.validatedTargetRsm.peptideInstances.flatMap(pi => pi.peptideMatches)
    val allDecPepMatc = rsValidation.validatedDecoyRsm.get.peptideInstances.flatMap(pi => pi.peptideMatches)
    Assert.assertEquals(102, allTarPepMatc.length)
    Assert.assertEquals(16, allDecPepMatc.length)

  }

  @Test
  def testProtSetFDRValidation() = {
    importDatFile(_datFileName,"""sp\|REV_\S+""")

    val testTDAnalyzer = Some(new CompetitionBasedTDAnalyzer(new ScorePSMFilter()))

    // Create protein set validator
    val protSetValidator = new ProtSetRulesValidatorWithFDROptimization(
      //protSetScoreUpdater = Some(new MascotProteinSetScoreUpdater(-20f)),
      protSetFilterRule1 = new ScoreProtSetFilter,
      protSetFilterRule2 = new ScoreProtSetFilter,
      expectedFdr = Some(1.0f))

    logger.info("ResultSetValidator testProtSetFDRValidation Create service")
    val rsValidation = ResultSetValidator(
      execContext = executionContext,
      targetRsId = rsIDWork,
      tdAnalyzer = testTDAnalyzer,
      pepMatchPreFilters = None,
      pepMatchValidator = None,
      protSetFilters = None,
      protSetValidator = Some(protSetValidator),
      storeResultSummary = false)

    logger.debug("ResultSetValidator testProtSetFDRValidation RUN service")
    val result = rsValidation.runService
    Assert.assertTrue(result)
    logger.debug("End Run ResultSetValidator Service for testProtSetFDRValidation")

    logger.debug("Verify Result IN RSM")
    val allTarProtSets = rsValidation.validatedTargetRsm.proteinSets
    val allDecProtSets = rsValidation.validatedDecoyRsm.get.proteinSets
    Assert.assertEquals("AllTarProtSets validated count", 13, allTarProtSets.count(_.isValidated))
    Assert.assertEquals("AllDecProtSets validated count", 0, allDecProtSets.count(_.isValidated))
  }
    
  @Test
  def testPepMatchAndProtSetFDRValidation() = {
    importDatFile(_datFileName,"""sp\|REV_\S+""")

    val firstRankFilter = new RankPSMFilter(1)
    val pepMatchValFilter = new ScorePSMFilter()
    val testTDAnalyzer = Some(new CompetitionBasedTDAnalyzer(pepMatchValFilter))

    // Create peptide match validator
    val pepMatchValidator = new TDPepMatchValidatorWithFDROptimization(
      validationFilter = pepMatchValFilter,
      expectedFdr = Some(7.0f),
      tdAnalyzer = testTDAnalyzer
      )

    // Create protein set validator
    val protSetValidator = new ProtSetRulesValidatorWithFDROptimization(
      //protSetScoreUpdater = Some(new MascotProteinSetScoreUpdater(-20f)),
      protSetFilterRule1 = new ScoreProtSetFilter,
      protSetFilterRule2 = new ScoreProtSetFilter,
      expectedFdr = Some(1.0f))

    logger.info("ResultSetValidator testPepMatchAndProtSetFDRValidation Create service")
    val rsValidation = ResultSetValidator(
      execContext = executionContext,
      targetRsId = rsIDWork,
      tdAnalyzer = testTDAnalyzer,
      pepMatchPreFilters = Some(Seq(firstRankFilter)),
      pepMatchValidator = Some(pepMatchValidator),
      protSetFilters = None,
      protSetValidator = Some(protSetValidator),
      inferenceMethod = Some(InferenceMethods.parsimonious),
      proteinSetScoring = Some(ProtSetScoring.MASCOT_PROTEIN_SET_SCORE),
      storeResultSummary = false)

    logger.debug("ResultSetValidator testPepMatchAndProtSetFDRValidation RUN service")
    val result = rsValidation.runService
    Assert.assertTrue(result)
    logger.debug("End Run ResultSetValidator Service for testPepMatchAndProtSetFDRValidation")

    logger.debug("Verify Result IN RSM")
    val allTarProtSets = rsValidation.validatedTargetRsm.proteinSets
    val allDecProtSets = rsValidation.validatedDecoyRsm.get.proteinSets
    Assert.assertEquals("AllTarProtSets validated count", 7, allTarProtSets.count(_.isValidated))
    Assert.assertEquals("AllDecProtSets validated count", 0, allDecProtSets.count(_.isValidated))
  }

  @Test
  def testOtherMascotPValueValidation() = {

    importDatFile("/dat_samples/GRE_F068213_M2.4_TD_EColi.dat","""sp\|REV_\S+""") //"""###REV###\S+""")

    val pValTh = 0.1f
    val pepFilters = Seq(new MascotPValuePSMFilter(pValue = pValTh, useHomologyThreshold = false))

    val rsValidation = ResultSetValidator(
      execContext = executionContext,
      targetRsId = rsIDWork,
      tdAnalyzer = Some(new BasicTDAnalyzer(TargetDecoyModes.CONCATENATED)),
      pepMatchPreFilters = Some(pepFilters),
      pepMatchValidator = None,
      protSetFilters = None,
      storeResultSummary = false)

    val result = rsValidation.runService
    Assert.assertTrue(result)
    logger.info(" End Run ResultSetValidator Service with Identity Threshold Filter, in Test ")

    val tRSM = rsValidation.validatedTargetRsm
    val dRSM = rsValidation.validatedDecoyRsm

    Assert.assertNotNull(tRSM)
    Assert.assertNotNull(dRSM)
    Assert.assertTrue(dRSM.isDefined)

    logger.debug(" Verify Result IN RSM ")
    val allTarPepMatc = rsValidation.validatedTargetRsm.peptideInstances.flatMap(pi => pi.peptideMatches)
    val allDecPepMatc = rsValidation.validatedDecoyRsm.get.peptideInstances.flatMap(pi => pi.peptideMatches)

    logger.debug("  - allTarPepMatc " + allTarPepMatc.length + " allDecPepMatc " + allDecPepMatc.length)
    Assert.assertEquals(996, allTarPepMatc.length + allDecPepMatc.length) //IRMa -> 1000! 

    rsValidation.validatedTargetRsm.peptideInstances.foreach(pepInst => {
      pepInst.peptideMatches.foreach(peptideM => {
        Assert.assertTrue(peptideM.isValidated)
      })
    })

    Assert.assertTrue(tRSM.properties.isDefined)
    Assert.assertTrue(tRSM.properties.get.getValidationProperties.get.getParams.getPeptideFilters.isDefined)

    val pepFilterProps = tRSM.properties.get.getValidationProperties.get.getParams.getPeptideFilters.get
    Assert.assertEquals(1, pepFilterProps.size)

    val fPrp: FilterDescriptor = pepFilterProps(0)
    val props = fPrp.getProperties.get
    Assert.assertEquals(1, props.size)
    Assert.assertEquals(new MascotPValuePSMFilter().filterDescription, fPrp.getDescription.get)
    Assert.assertEquals(props(FilterPropertyKeys.THRESHOLD_VALUE), pValTh)

    val pepValResultsOpt = rsValidation.validatedTargetRsm.properties.get.getValidationProperties.get.getResults.getPeptideResults
    Assert.assertTrue(pepValResultsOpt.isDefined)
    val pepValResults = pepValResultsOpt.get
    Assert.assertEquals(996, pepValResults.getTargetMatchesCount) // IRMa 997
    Assert.assertEquals(0, pepValResults.getDecoyMatchesCount.get) //IRMa 3
  }

  @Test
  def testMascotPValueValidation() = {
    importDatFile(_datFileName,"""sp\|REV_\S+""")
    val pValTh = 0.01f
    val pepFilters = Seq(new MascotPValuePSMFilter(pValue = pValTh, useHomologyThreshold = false))

    val rsValidation = ResultSetValidator(
      execContext = executionContext,
      targetRsId = rsIDWork,
      tdAnalyzer = Some(new BasicTDAnalyzer(TargetDecoyModes.CONCATENATED)),
      pepMatchPreFilters = Some(pepFilters),
      pepMatchValidator = None,
      protSetFilters = None,
      storeResultSummary = false)

    val result = rsValidation.runService
    Assert.assertTrue(result)
    logger.info(" End Run ResultSetValidator Service with Score Filter, in Test ")

    val tRSM = rsValidation.validatedTargetRsm
    val dRSM = rsValidation.validatedDecoyRsm

    Assert.assertNotNull(tRSM)
    Assert.assertNotNull(dRSM)
    Assert.assertTrue(dRSM.isDefined)

    logger.debug(" Verify Result IN RSM ")
    val allTarPepMatc = rsValidation.validatedTargetRsm.peptideInstances.flatMap(pi => pi.peptideMatches)
    val allDecPepMatc = rsValidation.validatedDecoyRsm.get.peptideInstances.flatMap(pi => pi.peptideMatches)

    //    val allPepMatches = allTarPepMatc++allDecPepMatc
    //    allPepMatches.foreach( pepM  => {
    //	logger.debug(pepM.msQueryId+"\t"+pepM.peptide.sequence+"\t"+pepM.peptide.ptmString)
    //    } )

    Assert.assertEquals(57, allTarPepMatc.length + allDecPepMatc.length)

    rsValidation.validatedTargetRsm.peptideInstances.foreach(pepInst => {
      pepInst.peptideMatches.foreach(peptideM => {
        Assert.assertTrue(peptideM.isValidated)
      })
    })

    Assert.assertTrue(tRSM.properties.isDefined)
    Assert.assertTrue(tRSM.properties.get.getValidationProperties.get.getParams.getPeptideFilters.isDefined)

    val pepFilterProps = tRSM.properties.get.getValidationProperties.get.getParams.getPeptideFilters.get
    Assert.assertEquals(1, pepFilterProps.size)

    val fPrp: FilterDescriptor = pepFilterProps(0)
    val props = fPrp.getProperties.get
    Assert.assertEquals(1, props.size)
    Assert.assertEquals(new MascotPValuePSMFilter().filterDescription, fPrp.getDescription.get)
    Assert.assertEquals(props(FilterPropertyKeys.THRESHOLD_VALUE), pValTh)

    val pepValResultsOpt = rsValidation.validatedTargetRsm.properties.get.getValidationProperties.get.getResults.getPeptideResults
    Assert.assertTrue(pepValResultsOpt.isDefined)
    val pepValResults = pepValResultsOpt.get
    Assert.assertEquals(55, pepValResults.getTargetMatchesCount)
    Assert.assertEquals(2, pepValResults.getDecoyMatchesCount.get)
  }

  @Test
  def testMascotHomologyPValueValidation() = {
//    importDatFile("/dat_samples/F067920.dat","""###REV###_\S+""")
    importDatFile(_datFileName,"""sp\|REV_\S+""")
    val pValTh = 0.01f
    val pepFilters = Seq(new MascotPValuePSMFilter(pValue = pValTh, useHomologyThreshold = true))

    val rsValidation = ResultSetValidator(
      execContext = executionContext,
      targetRsId = rsIDWork,
      tdAnalyzer = Some(new BasicTDAnalyzer(TargetDecoyModes.CONCATENATED)),
      pepMatchPreFilters = Some(pepFilters),
      pepMatchValidator = None,
      protSetFilters = None,
      storeResultSummary = false)

    val result = rsValidation.runService
    Assert.assertTrue(result)
    logger.info(" End Run ResultSetValidator Service with Score Filter, in Test ")

    val tRSM = rsValidation.validatedTargetRsm
    val dRSM = rsValidation.validatedDecoyRsm

    Assert.assertNotNull(tRSM)
    Assert.assertNotNull(dRSM)
    Assert.assertTrue(dRSM.isDefined)

    logger.debug(" Verify Result IN RSM ")
    val allTarPepMatc = rsValidation.validatedTargetRsm.peptideInstances.flatMap(pi => pi.peptideMatches)
    val allDecPepMatc = rsValidation.validatedDecoyRsm.get.peptideInstances.flatMap(pi => pi.peptideMatches)

    //    val allPepMatches = allTarPepMatc++allDecPepMatc
    //    allPepMatches.foreach( pepM  => {
    //	logger.debug(pepM.msQueryId+"\t"+pepM.peptide.sequence+"\t"+pepM.peptide.ptmString)
    //    } )
    logger.debug(" allTarPepMatc " + allTarPepMatc.length) //IRMa 81 + 0 duplicated between target and decoy 

    logger.debug(" allDecPepMatc  " + allDecPepMatc.length) //IRMa 3 + 0 duplicated between target and decoy 
    Assert.assertEquals(84, allTarPepMatc.length + allDecPepMatc.length)

    rsValidation.validatedTargetRsm.peptideInstances.foreach(pepInst => {
      pepInst.peptideMatches.foreach(peptideM => {
        Assert.assertTrue(peptideM.isValidated)
      })
    })

    Assert.assertTrue(tRSM.properties.isDefined)
    Assert.assertTrue(tRSM.properties.get.getValidationProperties.get.getParams.getPeptideFilters.isDefined)

    val pepFilterProps = tRSM.properties.get.getValidationProperties.get.getParams.getPeptideFilters.get
    Assert.assertEquals(1, pepFilterProps.size)

    val fPrp: FilterDescriptor = pepFilterProps(0)
    val props = fPrp.getProperties.get
    Assert.assertEquals(1, props.size)
    Assert.assertEquals(new MascotPValuePSMFilter(useHomologyThreshold = true).filterDescription, fPrp.getDescription.get)
    Assert.assertEquals(props(FilterPropertyKeys.THRESHOLD_VALUE), pValTh)

    val pepValResultsOpt = rsValidation.validatedTargetRsm.properties.get.getValidationProperties.get.getResults.getPeptideResults
    Assert.assertTrue(pepValResultsOpt.isDefined)
    val pepValResults = pepValResultsOpt.get
    Assert.assertEquals(81, pepValResults.getTargetMatchesCount)
    Assert.assertEquals(3, pepValResults.getDecoyMatchesCount.get)
  }

}