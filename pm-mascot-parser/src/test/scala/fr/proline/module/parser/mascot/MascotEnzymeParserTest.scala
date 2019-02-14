package fr.proline.module.parser.mascot

import java.io.File

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

/**
 * @author David Bouyssie
 *
 */
@Test
class MascotEnzymeParserTest {

  val enzymesFile = new File( this.getClass().getResource("/mascot_config/enzymes").toURI() )
  
  @Test
  def testGetEnzymeDefinitions() = {
    val enzymeDefs = MascotEnzymeParser.getEnzymeDefinitions(enzymesFile).toSeq
    assertEquals( 22, enzymeDefs.length )
    
    val cnbrTrypDefOpt = enzymeDefs.find( _.name == "CNBr+Trypsin" )
    assertTrue( "'CNBr+Trypsin' enzyme must be parsed", cnbrTrypDefOpt != None )
    
    val cnbrTrypDef = cnbrTrypDefOpt.get
    assertEquals( 2, cnbrTrypDef.cleavages.length )
    
    val firstCleavage = cnbrTrypDef.cleavages(0)
    assertEquals( "M", firstCleavage.residues )
    assertEquals( None, firstCleavage.restrict )
    assertFalse( "cleavage site must be C-term", firstCleavage.isNterm )
    
    val secondCleavage = cnbrTrypDef.cleavages(1)
    assertEquals( "KR", secondCleavage.residues )
    assertEquals( "P", secondCleavage.restrict.get )
    assertFalse( "cleavage site must be C-term", firstCleavage.isNterm )
    
  }
  
}