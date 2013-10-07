package fr.proline.module.parser.mascot

import org.junit.Assert._
import org.junit.Test
import java.io.File
import fr.proline.core.om.model.msi.PtmDefinition

@Test
class ResultFileVerifierTest {

  val file = new File("D:/bruley/Data/Mascot/F067920.dat")
  
  @Test
  def testPtmExtraction = {
    val provider = new MascotResultFileProvider
    val start = System.currentTimeMillis()
    val ptmDefs = provider.getPtmDefinitions(file)
    
    assertEquals( 9, ptmDefs.length )
    assertEquals( "H(3) C(2) N O", ptmDefs(0).ptmEvidences(0).composition)
    assertEquals( "Carbamidomethyl", ptmDefs(0).names.shortName)
    
    val ptmDefs2 = provider.getPtmDefinitions(file)
    assertEquals( 9, ptmDefs2.length )
    
    var ptms = ptmDefs.toSet

    for (p <- ptmDefs2) {
      if ( !ptms.exists(_.sameAs(p))) ptms += p 
    }
    
    assertEquals( 9, ptms.size )
    
  }
  
}