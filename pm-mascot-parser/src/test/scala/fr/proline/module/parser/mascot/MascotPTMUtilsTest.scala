package fr.proline.module.parser.mascot

import scala.collection.mutable.HashMap
import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotNull
import org.junit.Before
import org.junit.Test
import com.typesafe.scalalogging.LazyLogging
import fr.proline.core.om.model.msi.PtmDefinition
import fr.proline.core.om.model.msi.PtmLocation
import fr.proline.core.om.provider.msi.EmptyPTMProvider
import fr.proline.core.om.provider.msi.PTMFakeProvider

@Test
class MascotPTMUtilsTest extends LazyLogging {


  @Before
  def init() {
    logger.debug("Start Logging MascotPTMUtilsTest")
    MascotPTMUtils.ptmDefsByMascotModName.clear()
  }

  @After
  def tearDown() {
    MascotPTMUtils.ptmDefsByMascotModName.clear()
  }
  
  @Test
  def testParseSimplePTM() = {
    val ptmToParse = "Oxidation (M)"
    val ptmDefs = MascotPTMUtils.mascotModToPTMDefinitions(PTMFakeProvider, ptmToParse)
    assertNotNull(ptmDefs)
    assertEquals(1, ptmDefs.length)

    val singlePtmDef: PtmDefinition = ptmDefs(0)
    assertEquals("Post-translational", singlePtmDef.classification)
    assertEquals("Anywhere", singlePtmDef.location)
    assertEquals("Oxidation", singlePtmDef.names.shortName)
    assertEquals("M", singlePtmDef.residue.toString())
  }

  @Test
  def testParseProtNterPTM() = {
    val ptmToParse = "Acetyl (Protein N-term)"
    val ptmDefs = MascotPTMUtils.mascotModToPTMDefinitions(EmptyPTMProvider, ptmToParse)
    assertNotNull(ptmDefs)
    assertEquals(1, ptmDefs.length)

    val singlePtmDef = ptmDefs(0)
    assertEquals("Post-translational", singlePtmDef.classification)
    assertEquals(PtmLocation.PROT_N_TERM.toString, singlePtmDef.location)
    assertEquals("Acetyl", singlePtmDef.names.shortName)
    assertEquals('\0', singlePtmDef.residue)
  }

  @Test
  def testParseNterResiduePTM() = {
    val ptmToParse = "Acetyl (Nterm M)"
    val ptmDefs = MascotPTMUtils.mascotModToPTMDefinitions(EmptyPTMProvider, ptmToParse)
    assertNotNull(ptmDefs)
    assertEquals(1, ptmDefs.length)

    val singlePtmDef = ptmDefs(0)
    assertEquals("Post-translational", singlePtmDef.classification)
    assertEquals(PtmLocation.ANY_N_TERM.toString, singlePtmDef.location)
    assertEquals("Acetyl", singlePtmDef.names.shortName)
    assertEquals('M', singlePtmDef.residue)
  }
}