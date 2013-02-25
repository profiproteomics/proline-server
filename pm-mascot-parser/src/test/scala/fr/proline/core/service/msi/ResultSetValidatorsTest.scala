package fr.proline.core.service.msi

import org.junit.{ After, AfterClass, Assert, Test, Before, BeforeClass }
import com.weiglewilczek.slf4s.Logging
import fr.proline.context.IExecutionContext
import fr.proline.repository.DriverType
import java.io.File
import fr.proline.core.om.model.msi.ResultSet
import fr.proline.core.om.provider.msi.impl.ORMResultSetProvider
import fr.proline.core.om.provider.msi.IResultSetProvider
import fr.proline.core.om.storer.msi.impl.StorerContext
import fr.proline.core.dal.SQLQueryHelper
import fr.proline.core.dal.SQLConnectionContext
import fr.proline.core.om.provider.msi.impl.SQLResultSetProvider
import fr.proline.core.dal.ContextFactory
import fr.proline.core.algo.msi.filtering.pepmatch._
import scala.collection.mutable.HashMap
import fr.proline.core.om.model.msi.PeptideMatch
import scala.collection.mutable.ArrayBuffer
import fr.proline.core.algo.msi.validation._
import fr.proline.core.om.model.msi.FilterDescriptor
import fr.proline.core.algo.msi.filtering._
import fr.proline.core.algo.msi.validation.BasicTDAnalyzer
import fr.proline.core.algo.msi.filtering.pepmatch.RankPSMFilter
import fr.proline.core.algo.msi.filtering.pepmatch.ScorePSMFilter
import fr.proline.core.algo.msi.filtering.IPeptideMatchFilter
import fr.proline.core.algo.msi.validation.pepmatch.TDPepMatchValidatorWithFDROptimization
import fr.proline.core.algo.msi.filtering.FilterPropertyKeys
import fr.proline.core.algo.msi.validation.proteinset.ProtSetRulesValidatorWithFDROptimization
import fr.proline.core.algo.msi.filtering.proteinset.ScoreProtSetFilter
import fr.proline.core.algo.msi.scoring.MascotProteinSetScoreUpdater

class ResultSetValidatorsTest extends AbstractRFImporterTest_ with Logging {

  val driverType = DriverType.H2
  var executionContext: IExecutionContext = null
  var rsIDWork: Int = 0
  var rsProvider: IResultSetProvider = null

  // TODO: load the file only once (BeforeClass ?)
  // TODO: do we need really to use the storer ?
  @Before
  @throws( classOf[Exception] )
  override def setUp() = {
    super.setUp()
    _datFileName = "/dat_samples/STR_F136482_CTD.dat"
    udsDBTestCase.loadDataSet( "/fr/proline/module/parser/mascot/UDS_Simple_Dataset.xml" )
    logger.info( "UDS db succesfully initialized" )

    val ( execContext, rsProv ) = buildJPAContext() //SQLContext()    
    executionContext = execContext
    rsProvider = rsProv

    logger.debug( " --- Load Mascot File " + _datFileName )

    var datFile: File = new File( ResultSetValidatorsTest.this.getClass.getResource( _datFileName ).toURI )

    val propertiedBuilder = Map.newBuilder[String, Any]
    propertiedBuilder += ( "ion.score.cutoff" -> 0.0 )
    propertiedBuilder += ( "subset.threshold" -> 1.0 )

    val importer: ResultFileImporterJPAStorer = new ResultFileImporterJPAStorer(
      executionContext,
      resultIdentFile = datFile,
      fileType = "MascotMSParser",
      instrumentConfigId = 1,
      peaklistSoftwareId = 1, // TODO : provide the right value
      importerProperties = propertiedBuilder.result,
      acDecoyRegex = Some( """sp\|REV_\S+""".r ) )

    val result = importer.runService()
    rsIDWork = importer.getTargetResultSetId

    logger.debug( " ResultFile  loaded (" + result + ") from " + datFile.getAbsolutePath() + " with target resultID " + rsIDWork )
  }

  @After
  override def tearDown() {
    if ( executionContext != null )
      executionContext.closeAll()
    super.tearDown()
  }

  @Test
  def testScoreValidation() = {

    val scoreTh = 22.0f
    val pepFilters = Seq(new ScorePSMFilter( scoreThreshold = scoreTh ))
    
    val rsValidation = ResultSetValidator(
      execContext = executionContext,
      targetRsId = rsIDWork,
      tdAnalyzer = Some(new BasicTDAnalyzer(TargetDecoyModes.CONCATENATED)),
      pepMatchPreFilters = Some( pepFilters ),
      pepMatchValidator = None,
      protSetFilters = None,
      storeResultSummary = false )

    val result = rsValidation.runService
    Assert.assertTrue( result )
    logger.info( " End Run ResultSetValidator Service with Score Filter, in Test " )

    val tRSM = rsValidation.validatedTargetRsm
    val dRSM = rsValidation.validatedDecoyRsm
    
    Assert.assertNotNull( tRSM )
    Assert.assertNotNull( dRSM )
    Assert.assertTrue( dRSM.isDefined )
    
    Assert.assertTrue( tRSM.properties.isDefined )    
    Assert.assertTrue( tRSM.properties.get.getValidationProperties.get.getParams.getPeptideFilters.isDefined )
    
    val pepFilterProps = tRSM.properties.get.getValidationProperties.get.getParams.getPeptideFilters.get
    Assert.assertEquals( 1, pepFilterProps.size )
    
    val fPrp: FilterDescriptor = pepFilterProps( 0 )
    val props = fPrp.getProperties.get
    Assert.assertEquals( 1, props.size )
    Assert.assertEquals( new ScorePSMFilter().filterDescription, fPrp.getDescription.get )
    Assert.assertEquals( props( FilterPropertyKeys.THRESHOLD_VALUE), scoreTh )
    
    val pepValResultsOpt = rsValidation.validatedTargetRsm.properties.get.getValidationProperties.get.getResults.getPeptideResults
    Assert.assertTrue( pepValResultsOpt.isDefined )
    val pepValResults = pepValResultsOpt.get
    Assert.assertEquals( 438, pepValResults.getTargetMatchesCount)
    Assert.assertEquals( 251, pepValResults.getDecoyMatchesCount.get)
    Assert.assertEquals(72.86, pepValResults.getFdr.get,0.01)

    
    logger.debug( " Verify Result IN RSM " )
    val allTarPepMatc = rsValidation.validatedTargetRsm.peptideInstances.flatMap( pi => pi.peptideMatches )
    val allDecPepMatc = rsValidation.validatedDecoyRsm.get.peptideInstances.flatMap( pi => pi.peptideMatches )
    Assert.assertEquals( 438, allTarPepMatc.length )
    Assert.assertEquals( 251, allDecPepMatc.length )

    rsValidation.validatedTargetRsm.peptideInstances.foreach( pepInst => {
      pepInst.peptideMatches.foreach( peptideM => {
        Assert.assertTrue( peptideM.isValidated )
        Assert.assertTrue( peptideM.score > scoreTh )
      } )
    } )
  }

  @Test
  def testRankValidation() = {
    val readRS = rsProvider.getResultSet(rsIDWork).get
    val seqBuilder = Seq.newBuilder[IPeptideMatchFilter]
    val rank = 1
    seqBuilder += new RankPSMFilter( pepMatchMaxRank = 1 )
    val rsValidation = new ResultSetValidator(
      execContext = executionContext,
      targetRs = readRS,
      tdAnalyzer = Some(new BasicTDAnalyzer(TargetDecoyModes.CONCATENATED)),
      pepMatchPreFilters = Some( seqBuilder.result() ),
      pepMatchValidator = None,
      protSetFilters = None,
      storeResultSummary = false
    )
      

    val result = rsValidation.runService
    Assert.assertTrue( result )
    logger.info( " End Run ResultSetValidator Service with Rank filter, in Test " )

    Assert.assertNotNull( rsValidation.validatedTargetRsm )
    Assert.assertTrue( rsValidation.validatedDecoyRsm.isDefined )

    logger.debug( " Verify Result IN RSM " )
    val allTarPepMatc = rsValidation.validatedTargetRsm.peptideInstances.flatMap( pi => pi.peptideMatches )
    val allDecPepMatc = rsValidation.validatedDecoyRsm.get.peptideInstances.flatMap( pi => pi.peptideMatches )
    Assert.assertEquals( 774, allTarPepMatc.length )
    Assert.assertEquals( 638, allDecPepMatc.length )

    val pepMatchByQuId = new HashMap[Int, ArrayBuffer[PeptideMatch]]()
    allTarPepMatc.foreach( peptideM => {
      val pepMatches = pepMatchByQuId.get( peptideM.msQueryId ).getOrElse( new ArrayBuffer[PeptideMatch]() )
      //             System.out.println(peptideM.msQueryId+"\t"+peptideM.peptide.sequence+"\t"+peptideM.peptide.ptmString+"\t"+peptideM.score)
      pepMatches + peptideM
      Assert.assertTrue( peptideM.isValidated )
      pepMatchByQuId.put( peptideM.msQueryId, pepMatches )
    } )

    allDecPepMatc.foreach( peptideM => {
      val pepMatches = pepMatchByQuId.get( peptideM.msQueryId ).getOrElse( new ArrayBuffer[PeptideMatch]() )
      pepMatches + peptideM
      //             System.out.println(peptideM.msQueryId+"\t"+peptideM.peptide.sequence+"\t"+peptideM.peptide.ptmString)
      Assert.assertTrue( peptideM.isValidated )
      pepMatchByQuId.put( peptideM.msQueryId, pepMatches )
    } )

    pepMatchByQuId.foreach( entry => {
      var validatedEntry = false
      if ( entry._2.length.equals( 1 ) ) {
        validatedEntry = true
      } else {
        var index = 0
        while ( index < entry._2.length - 1 ) {
          validatedEntry = ( entry._2( index ).score - entry._2( index + 1 ).score ).abs < 0.1
          index += 1
        }
      }

      Assert.assertTrue( validatedEntry )
    } )
    
    val rsPepMatchByQuId = new HashMap[Int, ArrayBuffer[PeptideMatch]]()
    val rsPsm = readRS.peptideMatches ++ readRS.decoyResultSet.get.peptideMatches
    rsPsm.foreach( peptideM => {
      val pepMatches = rsPepMatchByQuId.get( peptideM.msQueryId ).getOrElse( new ArrayBuffer[PeptideMatch]() )
      pepMatches +=(peptideM)    
      rsPepMatchByQuId.put( peptideM.msQueryId, pepMatches )
    } )
    
    rsPepMatchByQuId.foreach(entry => {
     val psmEntry =  entry._2.sortWith((a,b) => a.score > b.score)
     var firstPSMScore = psmEntry(0).score
     entry._2.foreach(psm => {
       //logger.debug(" -- QID "+entry._1+" PSM "+psm.peptide.sequence+" firstPSMScore "+firstPSMScore+" <> "+psm.score+"  " +psm.isValidated+" ( "+(firstPSMScore - psm.score).abs+" )")
       if( (firstPSMScore - psm.score).abs >=0.1)
         Assert.assertTrue(!psm.isValidated)
       else 
         Assert.assertTrue(psm.isValidated)
     })
   })
    
    
  }

  @Test
  def testScoreFDRValidation() = {

    val testTDAnalyzer = Some(new BasicTDAnalyzer(TargetDecoyModes.CONCATENATED))
    val fdrValidator = new TDPepMatchValidatorWithFDROptimization(
      validationFilter = new ScorePSMFilter(),
      expectedFdr = Some(7.0f),
      tdAnalyzer = testTDAnalyzer
    )
//    ComputedFDRPeptideMatchFilter( 1.0F, new ScorePSMFilter() )
    logger.info( " ResultSetValidator testScoreFDRValidation Create service" )
    val rsValidation = ResultSetValidator(
      execContext = executionContext,
      targetRsId = rsIDWork,
      tdAnalyzer = testTDAnalyzer,
      pepMatchPreFilters = None,
      pepMatchValidator = Some( fdrValidator ),
      protSetFilters = None,
      storeResultSummary = false )
                    

    logger.debug( " ResultSetValidator testScoreFDRValidation RUN  service" )
    val result = rsValidation.runService
    Assert.assertTrue( result )
    logger.debug( " End Run ResultSetValidator Service with FDR filter using Score, in Test " )

    Assert.assertNotNull( rsValidation.validatedTargetRsm )
    Assert.assertTrue( rsValidation.validatedDecoyRsm.isDefined )
    Assert.assertTrue( rsValidation.validatedTargetRsm.properties.isDefined )
    
    val pepFilterPropsOpt = rsValidation.validatedTargetRsm.properties.get.getValidationProperties.get.getParams.getPeptideFilters
    Assert.assertTrue( pepFilterPropsOpt.isDefined )
    val pepFilterProps = pepFilterPropsOpt.get
    Assert.assertEquals( 1, pepFilterProps.size )
    val fPrp: FilterDescriptor = pepFilterProps( 0 )
    val props = fPrp.getProperties.get
    Assert.assertEquals( 1, props.size )
    Assert.assertEquals( new ScorePSMFilter().filterDescription, fPrp.getDescription.get )
    
    val scoreThresh = props( FilterPropertyKeys.THRESHOLD_VALUE).asInstanceOf[Float]
    Assert.assertEquals(52.89, scoreThresh, 0.01 )
    
    Assert.assertEquals(7.01f, rsValidation.validatedTargetRsm.properties.get.getValidationProperties.get.getResults.getPeptideResults.get.getFdr.get, 0.01f)

    logger.debug( " Verify Result IN RSM " )
    val allTarPepMatc = rsValidation.validatedTargetRsm.peptideInstances.flatMap( pi => pi.peptideMatches )
    val allDecPepMatc = rsValidation.validatedDecoyRsm.get.peptideInstances.flatMap( pi => pi.peptideMatches )
    Assert.assertEquals( 55, allTarPepMatc.length )
    Assert.assertEquals( 2, allDecPepMatc.length )

    allTarPepMatc.foreach( peptideM => {
      //             System.out.println(peptideM.msQueryId+"\t"+peptideM.peptide.sequence+"\t"+peptideM.peptide.ptmString+"\t"+peptideM.score)
      Assert.assertTrue( peptideM.isValidated )
      Assert.assertTrue( peptideM.score > scoreThresh )
    } )

    allDecPepMatc.foreach( peptideM => {
      //             System.out.println(peptideM.msQueryId+"\t"+peptideM.peptide.sequence+"\t"+peptideM.peptide.ptmString+"\t"+peptideM.score)
      Assert.assertTrue( peptideM.isValidated )
      Assert.assertTrue( peptideM.score > scoreThresh )
    } )

  }
  
  @Test
  def testRankAndScoreFDRValidation() = {

    val firstRankFilter = new RankPSMFilter(1)
    val valFilter = new ScorePSMFilter()
    val testTDAnalyzer = Some(new CompetitionBasedTDAnalyzer(valFilter))
    val fdrValidator = new TDPepMatchValidatorWithFDROptimization(
      validationFilter = valFilter,
      expectedFdr = Some(7.0f),
      tdAnalyzer = testTDAnalyzer
    )
    
    logger.info( "ResultSetValidator testRankAndScoreFDRValidation Create service" )
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
    
    logger.debug( "ResultSetValidator testRankAndScoreFDRValidation RUN service" )
    val result = rsValidation.runService
    Assert.assertTrue( result )
    logger.debug( "End Run ResultSetValidator Service with FDR filter using Rank and Score, in Test " )
    
    logger.debug( "Verify Result IN RS" )
    val rsTarPepMatches = rsValidation.validatedTargetRsm.resultSet.get.peptideMatches
    val rsDecPepMatches = rsValidation.validatedDecoyRsm.get.resultSet.get.peptideMatches
    Assert.assertEquals( 102, rsTarPepMatches.count(_.isValidated) )
    Assert.assertEquals( 16, rsDecPepMatches.count(_.isValidated) )
    
    logger.debug( "Verify Result IN RSM" )
    val allTarPepMatc = rsValidation.validatedTargetRsm.peptideInstances.flatMap( pi => pi.peptideMatches )
    val allDecPepMatc = rsValidation.validatedDecoyRsm.get.peptideInstances.flatMap( pi => pi.peptideMatches )
    Assert.assertEquals( 102, allTarPepMatc.length )
    Assert.assertEquals( 16, allDecPepMatc.length )
  }
  
  @Test
  def testPepMatchAndProtSetFDRValidation() = {

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
      protSetScoreUpdater = Some(new MascotProteinSetScoreUpdater(-20f)),
      protSetFilterRule1 = new ScoreProtSetFilter,
      protSetFilterRule2 = new ScoreProtSetFilter,
      expectedFdr = Some(1.0f)
    )
    
    logger.info( "ResultSetValidator testPepMatchAndProtSetFDRValidation Create service" )
    val rsValidation = ResultSetValidator(
      execContext = executionContext,
      targetRsId = rsIDWork,
      tdAnalyzer = testTDAnalyzer,
      pepMatchPreFilters = Some(Seq(firstRankFilter)),
      pepMatchValidator = Some(pepMatchValidator),
      protSetFilters = None,
      protSetValidator = Some(protSetValidator),
      storeResultSummary = false
    )
    
    logger.debug( "ResultSetValidator testPepMatchAndProtSetFDRValidation RUN service" )
    val result = rsValidation.runService
    Assert.assertTrue( result )
    logger.debug( "End Run ResultSetValidator Service for testPepMatchAndProtSetFDRValidation" )
    
    logger.debug( "Verify Result IN RSM" )
    val allTarProtSets = rsValidation.validatedTargetRsm.proteinSets
    val allDecProtSets = rsValidation.validatedDecoyRsm.get.proteinSets
    Assert.assertEquals( 8, allTarProtSets.count(_.isValidated) )
    Assert.assertEquals( 0, allDecProtSets.count(_.isValidated) )
  }

}