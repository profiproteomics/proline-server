package fr.proline.jms.util.jsonrpc

import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

import com.typesafe.scalalogging.StrictLogging

class ProfiJSONRPC2ResponseTest extends StrictLogging {

  private val MAGIC_NUMBER = 42

  @Test
  def testProfiResponse() {
    val response = new ProfiJSONRPC2Response("toto", "1")

    val str1 = response.toJSONString()
    logger.debug(str1)

    assertTrue("First response contains \"1\"", str1.contains("\"1\""))
    assertTrue("First response contains \"toto\"", str1.contains("\"toto\""))

    response.setID(Integer.valueOf(MAGIC_NUMBER))

    val secondResult = new Array[String](2)
    secondResult(0) = "toto"
    secondResult(1) = "tata"

    response.setResult(secondResult)

    val str2 = response.toJSONString()
    logger.debug(str2)

    assertFalse("Second response does NOT contain \"1\"", str2.contains("\"1\""))

    assertTrue("Second response contains " + MAGIC_NUMBER + " Integer", str2.contains(Integer.toString(MAGIC_NUMBER)))
    assertTrue("Second response contains \"toto\"", str2.contains("\"toto\""))
    assertTrue("Second response contains \"tata\"", str2.contains("\"tata\""))
    
    
  }

  
   @Test
  def testNullProfiResponse() {
     
    var updatedSpectraCount2 = 0
    var updatedSpectraCount: Integer = 0
    val response = new ProfiJSONRPC2Response(updatedSpectraCount, "1")

    val str1 = response.toJSONString()
    logger.debug(str1)

    assertTrue("First response contains \"1\"", str1.contains("\"1\""))
//    assertTrue("Response contains result ", str1.contains("result"))
    
    
    
  }
}
