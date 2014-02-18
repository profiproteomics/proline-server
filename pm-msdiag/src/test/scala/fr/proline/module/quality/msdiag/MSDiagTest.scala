package fr.proline.module.quality.msdiag

import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION
import org.junit.After
import org.junit.Assert
import org.junit.Assert.assertEquals
import org.junit.Before
import org.junit.Test
import com.typesafe.scalalogging.slf4j.Logging
import fr.proline.core.om.model.msi.ResultSet
import fr.proline.module.quality.msdiag.msi.MSDiagChargeMatch
import fr.proline.module.quality.msdiag.msi.MSDiagRTMatch
import fr.proline.module.quality.msdiag.msi.MSDiagResultSetManager

@Test
class MSDiagTest extends AbstractResultFileImporterTest with Logging {

  private def getMethod(): String = {
    val st = Thread.currentThread.getStackTrace()
    val method = st(2).getMethodName()
    logger.debug("TEST [" + method + "] STARTS")
    method
  }

  @Before
  def before() {
    super.init
  }
  @After
  def after() {
    super.closeResources
  }

  private def getTargetRS(fileName: String): ResultSet = {
    val rfi = {
      if (fileName.endsWith(".dat")) MascotFileImporterTest.parse(fileName, super.getExecutionContext)
      else if (fileName.endsWith(".omx") || fileName.endsWith(".omx.bz2")) OmssaFileImporterTest.parse(fileName, super.getExecutionContext)
      else throw new Exception("Unsupported file extension")
    }
    rfi.runService
    val rsOpt = super.getResultSet(rfi.getTargetResultSetId)
    if (!rsOpt.isDefined) throw new Exception("Resultset not recorded")
    rsOpt.get
  }

  //  @Test
  def test {
    logger.debug("TEST STARTS")
    val spectrumTitle = "Cmpd 1006, +MS2(392.26), 11.9 min #1774-1793"
    logger.debug("Spectrum : '" + spectrumTitle + "'")
    //    val RTFirstRegEx = """([0-9\.]+) min""".r
    val RTFirstRegEx = """Cmpd.+MS.+, (\d+\.\d+) min""".r
    logger.debug("RegEx : '" + RTFirstRegEx + "'")
    //    RTFirstRegEx.findAllIn(spectrumTitle).foreach(println)
    RTFirstRegEx.findAllIn(spectrumTitle).matchData foreach {
      m => println(m.group(1))
    }
    logger.debug("TEST ENDS")
  }

  @Test
  def testVersion {
    val method = getMethod()
    val version = new fr.proline.module.quality.msdiag.Version()
    logger.debug("Module name : " + version.getModuleName)
    assertEquals("PM-MSDiag", version.getModuleName)
    logger.debug("Module version : " + version.getVersion)
    logger.debug("TEST [" + method + "] OK: versionning is successful")
  }

  //  @Test
  def testOmssaFile {
    val method = getMethod()
    val rs = getTargetRS("X16818DB.omx.bz2")
    // TEST SOMETHING

    logger.debug("TEST [" + method + "] OK: parsing is successful")
  }
//
//  @Test
//  def testMascotFile {
//    val method = getMethod()
//    val rs = getTargetRS("SGI_F154964.dat")
//    // TEST SOMETHING
//    logger.debug("Nb target PeptideMatches : " + rs.peptideMatches.size)
//    logger.debug("Nb decoy PeptideMatches : " + super.getResultSet(rs.getDecoyResultSetId).get.peptideMatches.size)
//    val msdiag = new MSDiag(rs, super.getResultSet(rs.getDecoyResultSetId))
//    val scores = Array(20f, 40f, 60f)
//    val report = msdiag.getMSDiagByCharge(scores)
//    logger.debug("ABU printing results")
//    MSDiagToString.printCountPerChargeAndScore(report)
//    MSDiagToString.printMozPerChargeAndScore(report)
//    MSDiagToString.printCountPerScore(report)
//    logger.debug("Nb redundant matches : " + msdiag.countAllRedundantMatches)
//    val reportRT = msdiag.getMSDiagByRetentionTimes(scores, super.getParsingRules(8))
//    MSDiagToString.printCountPerRTAndScore(reportRT)
//
//    logger.debug("ABU checking results")
//    checkCountPerChargeAndScore(report)
//    checkMinMozPerChargeAndScore(report)
//    checkMaxMozPerChargeAndScore(report)
//    checkCountPerScore(report)
//    checkNumberOfRedundancy(msdiag)
//    checkCountPerRTAndScore(reportRT)
//    
//    logger.debug("TEST [" + method + "] OK: parsing is successful")
//  }
  
  @Test
  def testMascotFile_PrintResultSets {
    val method = getMethod()
    val rst = getTargetRS("SGI_F154964.dat")
    val rsd = super.getResultSet(rst.getDecoyResultSetId)
    val rsm = new MSDiagResultSetManager(rst, rsd)
    println("Title\tRank\tCharge\tScore\tSequence\tCalcMass\tisDecoy")
    rsm.getPeptideMatches(false).foreach(pm => println(MSDiagToString.printPeptideMatch(pm, false)))
    rsm.getPeptideMatches(true).foreach(pm => println(MSDiagToString.printPeptideMatch(pm, true)))
//    logger.debug("LISTE DES RESULTATS TARGET")
//    rst.peptideMatches.foreach(pm => {
//      println(MSDiagToString.printPeptideMatch(pm, false))
//    })
//    logger.debug("LISTE DES RESULTATS DECOY")
//    rsd.get.peptideMatches.foreach(pm => {
//      println(MSDiagToString.printPeptideMatch(pm, true))
//    })
  }
  

//  @Test
  def testMascotFile_CountPerChargeAndScore {
    val method = getMethod()
    val rs = getTargetRS("SGI_F154964.dat")
    val msdiag = new MSDiag(rs, super.getResultSet(rs.getDecoyResultSetId))
    val report = msdiag.getMSDiagByCharge(Array(20f, 40f, 60f))
    MSDiagToString.printCountPerChargeAndScore(report)
    assertEquals(report(0)(0).countItems, 80) // charge=1 ; score <= 20
    assertEquals(report(0)(1).countItems, 5) // charge=1 ; 20 < score <= 40
    assertEquals(report(0)(2).countItems, 0) // charge=1 ; 40 < score <= 60
    assertEquals(report(0)(3).countItems, 0) // charge=1 ; score > 60
    assertEquals(report(1)(0).countItems, 406) // charge=2 ; score <= 20
    assertEquals(report(1)(1).countItems, 230) // charge=2 ; 20 < score <= 40
    assertEquals(report(1)(2).countItems, 208) // charge=2 ; 40 < score <= 60
    assertEquals(report(1)(3).countItems, 162) // charge=2 ; score > 60
    assertEquals(report(2)(0).countItems, 145) // charge=3 ; score <= 20
    assertEquals(report(2)(1).countItems, 46) // charge=3 ; 20 < score <= 40
    assertEquals(report(2)(2).countItems, 16) // charge=3 ; 40 < score <= 60
    assertEquals(report(2)(3).countItems, 7) // charge=3 ; score > 60
    logger.debug("TEST [" + method + "] OK: parsing is successful")
  }
  
//  @Test
  def testMascotFile_MozPerChargeAndScore {
    val method = getMethod()
    val rs = getTargetRS("SGI_F154964.dat")
    val msdiag = new MSDiag(rs, super.getResultSet(rs.getDecoyResultSetId))
    val report = msdiag.getMSDiagByCharge(Array(20f, 40f, 60f))
    MSDiagToString.printMozPerChargeAndScore(report)
    assertEquals(report(0)(0).getLowestExperimentalMoz, 502) // charge=1 ; score <= 20
    assertEquals(report(0)(1).getLowestExperimentalMoz, 559) // charge=1 ; 20 < score <= 40
    assertEquals(report(0)(2).getLowestExperimentalMoz, 0) // charge=1 ; 40 < score <= 60
    assertEquals(report(0)(3).getLowestExperimentalMoz, 0) // charge=1 ; score > 60
    assertEquals(report(1)(0).getLowestExperimentalMoz, 306) // charge=2 ; score <= 20
    assertEquals(report(1)(1).getLowestExperimentalMoz, 308) // charge=2 ; 20 < score <= 40
    assertEquals(report(1)(2).getLowestExperimentalMoz, 394) // charge=2 ; 40 < score <= 60
    assertEquals(report(1)(3).getLowestExperimentalMoz, 449) // charge=2 ; score > 60
    assertEquals(report(2)(0).getLowestExperimentalMoz, 280) // charge=3 ; score <= 20
    assertEquals(report(2)(1).getLowestExperimentalMoz, 386) // charge=3 ; 20 < score <= 40
    assertEquals(report(2)(2).getLowestExperimentalMoz, 419) // charge=3 ; 40 < score <= 60
    assertEquals(report(2)(3).getLowestExperimentalMoz, 623) // charge=3 ; score > 60
    assertEquals(report(0)(0).getHighestExperimentalMoz, 997) // charge=1 ; score <= 20
    assertEquals(report(0)(1).getHighestExperimentalMoz, 1010) // charge=1 ; 20 < score <= 40
    assertEquals(report(0)(2).getHighestExperimentalMoz, 0) // charge=1 ; 40 < score <= 60
    assertEquals(report(0)(3).getHighestExperimentalMoz, 0) // charge=1 ; score > 60
    assertEquals(report(1)(0).getHighestExperimentalMoz, 1070) // charge=2 ; score <= 20
    assertEquals(report(1)(1).getHighestExperimentalMoz, 1031) // charge=2 ; 20 < score <= 40
    assertEquals(report(1)(2).getHighestExperimentalMoz, 1093) // charge=2 ; 40 < score <= 60
    assertEquals(report(1)(3).getHighestExperimentalMoz, 1223) // charge=2 ; score > 60
    assertEquals(report(2)(0).getHighestExperimentalMoz, 1054) // charge=3 ; score <= 20
    assertEquals(report(2)(1).getHighestExperimentalMoz, 975) // charge=3 ; 20 < score <= 40
    assertEquals(report(2)(2).getHighestExperimentalMoz, 1030) // charge=3 ; 40 < score <= 60
    assertEquals(report(2)(3).getHighestExperimentalMoz, 623) // charge=3 ; score > 60
    logger.debug("TEST [" + method + "] OK: parsing is successful")
  }
  
//  @Test
  def testMascotFile_CountPerScore {
    val method = getMethod()
    val rs = getTargetRS("SGI_F154964.dat")
    val msdiag = new MSDiag(rs, super.getResultSet(rs.getDecoyResultSetId))
    val report = msdiag.getMSDiagByCharge(Array(20f, 40f, 60f))
    MSDiagToString.printCountPerScore(report)
    assertEquals(report(0)(0).countItems, 631) // score <= 20 ; all charges ; target+decoy
    assertEquals(report(0)(1).countItems, 281) // 20 < score <= 40 ; all charges ; target+decoy
    assertEquals(report(0)(2).countItems, 224) // 40 < score <= 60 ; all charges ; target+decoy
    assertEquals(report(0)(3).countItems, 169) // score > 60 ; all charges ; target+decoy
    assertEquals(report(0)(0).decoyMatches.size, 196) // score <= 20 ; all charges ; decoy only
    assertEquals(report(0)(1).decoyMatches.size, 7) // 20 < score <= 40 ; all charges ; decoy only
    assertEquals(report(0)(2).decoyMatches.size, 0) // 40 < score <= 60 ; all charges ; decoy only
    assertEquals(report(0)(3).decoyMatches.size, 0) // score > 60 ; all charges ; decoy only
    logger.debug("TEST [" + method + "] OK: parsing is successful")
  }
  
//  @Test
  def testMascotFile_NumberOfRedundancy {
    val method = getMethod()
    val rs = getTargetRS("SGI_F154964.dat")
    val msdiag = new MSDiag(rs, super.getResultSet(rs.getDecoyResultSetId))
    logger.debug("Nb redundant matches : " + msdiag.countAllRedundantMatches)
    assertEquals(msdiag.countAllRedundantMatches, 231)
    logger.debug("TEST [" + method + "] OK: parsing is successful")
  }
  
//  @Test
  def testMascotFile_CountPerRTAndScore {
    val method = getMethod()
    val rs = getTargetRS("SGI_F154964.dat")
    val msdiag = new MSDiag(rs, super.getResultSet(rs.getDecoyResultSetId))
    val report = msdiag.getMSDiagByRetentionTimes(Array(20f, 40f, 60f), super.getParsingRules(8))
    MSDiagToString.printCountPerRTAndScore(report)
    checkRT(report(0), Array(0,0,0,0))
	checkRT(report(1), Array(1,0,0,0))
	checkRT(report(2), Array(24,9,4,1))
	checkRT(report(3), Array(36,10,10,2))
	checkRT(report(4), Array(34,18,10,4))
	checkRT(report(5), Array(33,7,7,3))
	checkRT(report(6), Array(29,11,10,5))
	checkRT(report(7), Array(36,10,7,5))
	checkRT(report(8), Array(26,18,10,2))
	checkRT(report(9), Array(31,15,7,3))
	checkRT(report(10), Array(29,13,9,5))
	checkRT(report(11), Array(22,20,11,2))
	checkRT(report(12), Array(23,16,18,3))
	checkRT(report(13), Array(26,14,14,6))
	checkRT(report(14), Array(20,11,15,5))
	checkRT(report(15), Array(23,20,12,1))
	checkRT(report(16), Array(21,18,7,12))
	checkRT(report(17), Array(22,10,11,7))
	checkRT(report(18), Array(16,6,9,9))
	checkRT(report(19), Array(15,6,5,11))
	checkRT(report(20), Array(13,9,5,6))
	checkRT(report(21), Array(10,4,6,6))
	checkRT(report(22), Array(15,2,3,7))
	checkRT(report(23), Array(11,7,6,7))
	checkRT(report(24), Array(9,4,6,8))
	checkRT(report(25), Array(11,5,3,7))
	checkRT(report(26), Array(13,3,3,5))
	checkRT(report(27), Array(15,2,2,10))
	checkRT(report(28), Array(9,4,4,2))
	checkRT(report(29), Array(9,1,3,6))
	checkRT(report(30), Array(8,2,1,2))
	checkRT(report(31), Array(14,3,2,7))
	checkRT(report(32), Array(11,0,2,4))
	checkRT(report(33), Array(9,1,1,3))
	checkRT(report(34), Array(5,2,1,3))
	checkRT(report(35), Array(2,0,0,0))
    logger.debug("TEST [" + method + "] OK: parsing is successful")
  }
  private def checkRT(byRT: Array[MSDiagRTMatch], numbers: Array[Int]) {
    for(i <- 0 until numbers.length) {
      assertEquals(byRT(i), numbers(i))
    }
  }
}
