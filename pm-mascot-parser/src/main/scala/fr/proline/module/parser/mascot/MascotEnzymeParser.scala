package fr.proline.module.parser.mascot

import java.io.{InputStream,File,FileInputStream}
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import fr.profi.util.io._
import fr.profi.util.regex.RegexUtils._

/**
 * @author David Bouyssie
 *
 */
object MascotEnzymeParser {
  
  def getEnzymeDefinitions( fileLocation: File ): Iterable[EnzymeDefinition] = {
    getEnzymeDefinitions( new FileInputStream(fileLocation.getAbsolutePath) )
  }
  
  def getEnzymeDefinitions( inputStream: InputStream ): Iterable[EnzymeDefinition] = {
    
    /* File data block:
    
      Title:thermolysin
      Independent:0
      SemiSpecific:0
      Cleavage[0]:ILFVMA
      Restrict[0]:DE
      Nterm[0]
      *
    */
    
    val enzymeDefByName = new collection.mutable.HashMap[String,EnzymeDefinition]()
    var enzymeName = ""      
    // mascot dat files have sections, enzyme config file do not
    // considering sections allows this method to be able to read both files
    var currentSection = ""
    val r = """^Content-Type:.+name=\"(.+)\"""".r
    
    // TODO: quit reading after enzyme section
    Source.fromInputStream(inputStream).eachLine( line =>
      if( line =~ "^Content-Type:.+" ) {
        r.findAllIn(line).matchData.foreach(m => currentSection = m.group(1))
      }
      else if( line != "*" && (currentSection == "" || currentSection == "enzyme") ) {
        // Create new enzyme definition for each found title
        if( line =~ "^Title:.+" ) {
          enzymeName = _parseLine(line)._3
          enzymeDefByName += enzymeName -> new EnzymeDefinition( enzymeName )
        }
        // Parse enzyme definition attributes
        else if( line =~ ".+:.+" || line =~ "[CN]term.*" ) {
          for( enzymeDef <- enzymeDefByName.get(enzymeName) ) {
            //enzymeDef = { cleavages = () } if !defined enzymeDef     
            
            // Parse the line
            val( key, idx, value ) = _parseLine(line)
            
            key match {
              case "Independent" => {
                if( value == "1" ) enzymeDef.independent = true
                else enzymeDef.independent = false
              }
              case "SemiSpecific" => {
                if( value == "1" ) enzymeDef.semiSpecific = true
                else enzymeDef.semiSpecific = false
              }
              case "Cleavage" => {
                enzymeDef.getOrCreateCleavage(idx).residues = value
              }
              case "Restrict" => {
                enzymeDef.getOrCreateCleavage(idx).restrict = Some(value)
              }
              case "Cterm" => {
                enzymeDef.getOrCreateCleavage(idx).isNterm = false
              }
              case "Nterm" => {
                enzymeDef.getOrCreateCleavage(idx).isNterm = true
              }
              case _ => throw new Exception("unknown enzyme key: "+ key )
            }
          }
        }
      }
    )
    
    enzymeDefByName.values
  }
  
  def _parseLine( s: String ): Tuple3[String,Int,String] = {
    val parts = s.split(":")
    
    // Parse the key
    val( key, idx ) = if( parts(0) =~ """.+\[(\d)\]""" ) _parseKey(parts(0))
                      else( parts(0), 0 )
    
    // Retrieve the value
    val value = if( parts.length == 2 ) parts(1) else null
    
    Tuple3(key,idx,value)
  }
  
  private def _parseKey( s: String ): Tuple2[String,Int] = {
    val matches = (s =# """(.+)\[(\d)\]""").get
    (matches.group(1), matches.group(2).toInt)
  }
  
}

case class EnzymeDefinition(
  val name: String,
  var independent: Boolean = false,
  var semiSpecific: Boolean = false,
  var cleavages: ArrayBuffer[EnzymeCleavage] = new ArrayBuffer[EnzymeCleavage](0)
  ) {
  
  def getOrCreateCleavage( idx: Int ): EnzymeCleavage = {
    if( this.cleavages.length == idx ) this.cleavages += EnzymeCleavage()
    this.cleavages(idx)
  }
  
}

case class EnzymeCleavage(
  var residues: String = null,
  var restrict: Option[String] = None,
  var isNterm: Boolean = true // false means Cterm
)
