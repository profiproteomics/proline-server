package fr.proline.module.parser.mascot

import org.junit.Before
import org.junit.After
import org.junit.Test
import com.typesafe.scalalogging.LazyLogging
import java.io.File
import org.junit.Assert

/**
 * @author VD225637
 *
 */
@Test
class MascotPTMDefiitionTest extends LazyLogging {

	@Before
  def init() {
    logger.debug("Start Logging MascotPTMDefiitionTest")
    
  } 

  @After
  def tearDown() {
   
  }
  
  @Test
  def testReadModif() = {
	  val filename = "/dat_samples/GRE_F068213_M2.4_TD_EColi.dat"
	  val f = new File(this.getClass().getResource(filename).toURI())
	  val mascotProvider = new MascotResultFileProvider()
	  val result = mascotProvider.getPtmDefinitions(f, null)
	  Assert.assertNotNull(result)
	  Assert.assertEquals(29, result.length)
	  val ptmNames = result.groupBy(_.names).keySet
	  Assert.assertEquals(3, ptmNames.size)
  }

    
  @Test
  def testReadModifInFifthPosoition() = {	  
	  val filename = "/dat_samples/header_unimodFifth_F063872.dat"
	  val f = new File(this.getClass().getResource(filename).toURI())
	  val mascotProvider = new MascotResultFileProvider()
	  val result = mascotProvider.getPtmDefinitions(f, null)
	  Assert.assertNotNull(result)
	  Assert.assertEquals(19, result.length)
	  val ptmNames = result.groupBy(_.names).keySet
	  Assert.assertEquals(2, ptmNames.size)
  
  }

  
}