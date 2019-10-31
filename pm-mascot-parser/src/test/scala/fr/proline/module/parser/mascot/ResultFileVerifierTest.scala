package fr.proline.module.parser.mascot

import org.junit.Assert._
import org.junit.Test
import java.io.File
import fr.proline.core.om.model.msi.PtmDefinition

@Test
class ResultFileVerifierTest {

  val ecoli_datFileName = "/dat_samples/GRE_F068213_M2.4_TD_EColi.dat"
  
  @Test
  def testPtmExtraction = {
    val f = new File(this.getClass().getResource(ecoli_datFileName).toURI())
    val provider = new MascotResultFileProvider
    val start = System.currentTimeMillis()
    val ptmDefs = provider.getPtmDefinitions(f, Map.empty[String, Any])
    
    assertEquals( 29, ptmDefs.length )
    assertEquals( "H(3) C(2) N O", ptmDefs(0).ptmEvidences(0).composition)
    assertEquals( "Carbamidomethyl", ptmDefs(0).names.shortName)
    
    val ptmDefs2 = provider.getPtmDefinitions(f, Map.empty[String, Any])
    assertEquals( 29, ptmDefs2.length )
    
    var ptms = ptmDefs.toSet

    for (p <- ptmDefs2) {
      if ( !ptms.exists(_.sameAs(p))) ptms += p 
    }
    
    assertEquals( 29, ptms.size )
    
  }
  
}