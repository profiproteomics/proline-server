package fr.proline.module.seq.jms;

import org.junit.Assert;
import org.junit.Test;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;

import fr.proline.jms.util.NodeConfig;

public class JMSConfigTest {

  /* JUnit @Rule annotation does not work in Scala : 
   * http://stackoverflow.com/questions/7352087/how-can-i-use-junit-expectedexception-in-scala
   */
  
  @Test
  public void testReadConfigFile() {

    // Load application config and replace some properties values with the previous ones
    ClassLoader classLoader = JMSConfigTest.class.getClassLoader();
    Config appConf = ConfigFactory.load(classLoader, "jms-node");
    Assert.assertTrue( appConf.hasPath("node_config.proline_service_request_queue_name")); 
    Assert.assertEquals("true", appConf.getString("node_config.enable_imports"));
  }
  
  @Test
  public void testUseNodeConfigFile() {
	  
	Config  jmsConfig =   NodeConfig.getJMSConfigParams();
	Assert.assertNotNull(jmsConfig);	
    Assert.assertEquals("true", jmsConfig.getString("node_config.enable_imports"));
  }

}
