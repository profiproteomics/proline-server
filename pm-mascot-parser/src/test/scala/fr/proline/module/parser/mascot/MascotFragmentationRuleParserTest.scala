package fr.proline.module.parser.mascot

import java.io.File
import org.junit.Assert._
import org.junit.Test

/**
 * @author David Bouyssie
 *
 */
@Test
class MascotFragmentationRuleParserTest {

  val fragmentationFile = new File( this.getClass().getResource("/mascot_config/fragmentation_rules").toURI() )
  
  @Test
  def testGetInstrumentConfigurations() = {
    
    val instConfigs = MascotFragmentationRuleParser.getInstrumentConfigurations(fragmentationFile)
    assertEquals( 12, instConfigs.length )
    
    val esiTrapConfig = instConfigs.find( _.instrument.name == "ESI-TRAP" )
    assertTrue( "'ESI-TRAP' instrument configuration must be parsed", esiTrapConfig != None )
    
    val fragRules = MascotFragmentation.rules
    val esiTrapFragRules = Array(1,2,8,9,10,13,14,15).map( i => fragRules(i - 1 ) )
    assertArrayEquals( esiTrapFragRules.asInstanceOf[Array[Object]], esiTrapConfig.get.fragmentationRules.get.asInstanceOf[Array[Object]] )
    
  }
  
}