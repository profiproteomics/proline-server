package fr.proline.module.parser.mascot

import java.io.File
import java.util.Calendar
import org.hamcrest.CoreMatchers
import org.junit.Assert._
import org.junit.After
import org.junit.Before
import org.junit.Test
import com.weiglewilczek.slf4s.Logging
import fr.proline.core.om.model.msi.PeptideMatch
import fr.proline.core.om.model.msi.ResultSet
import fr.proline.core.om.provider.msi.impl.ORMPTMProvider
import fr.proline.core.om.provider.msi.impl.ORMPeptideProvider
import fr.proline.core.om.provider.msi.ProvidersFactory
import fr.proline.module.parser.provider.fake.ProteinFakeProvider
import fr.proline.module.parser.provider.fake.SeqDbFakeProvider
import fr.proline.repository.utils.DatabaseTestCase
import fr.proline.repository.Database
import fr.proline.repository.DatabaseContext

@Test
class MascotParserTest extends Logging { // }extends DatabaseTestCase {
  
	val dictyDatFileName: String = "/F047876.dat"
	val k12ControlDatFileName: String = "/F065306.dat"
	  
	var file:File = null
	val providerKey = "MSParser"
	  
	var psDBTestCase: DatabaseTestCase = null
	var psDbCtx: DatabaseContext = null
	//var psDbEMF: javax.persistence.EntityManagerFactory= null
	//var psDbEM: javax.persistence.EntityManager = null
	
	@Before
	def init(){
	  logger.info("Start Logging ")
	  logger.debug("Start Logging Debug ")
	  // Init PS db connexion
	  psDBTestCase = new PSDatabaseTestCase()
	  psDBTestCase.initDatabase()
	  //psDBTestCase.initEntityManager(JPAUtil.PersistenceUnitNames.PS_Key.getPersistenceUnitName())
	  psDBTestCase.loadDataSet("/fr/proline/module/parser/mascot/Unimod_Dataset.xml")
	  
	  //psDbEMF = psDBTestCase.getConnector().getEntityManagerFactory()
	  psDbCtx = new DatabaseContext( psDBTestCase.getConnector )
	  
	  ProvidersFactory.registerPeptideProvider(providerKey, new ORMPeptideProvider(psDbCtx))
	  ProvidersFactory.registerPTMProvider(providerKey, new ORMPTMProvider(psDbCtx))
	  ProvidersFactory.registerProteinProvider(providerKey, ProteinFakeProvider)
	  ProvidersFactory.registerSeqDatabaseProvider(providerKey, SeqDbFakeProvider)
	}
	
    @After
    def closeResources(){
      psDbCtx.close()
      psDBTestCase = null
      //psDbEMF.close()
      //psDbEM.close()
    }
	
	@Test
    def testReadDictyDatFile() = {
	    val f = new File( this.getClass().getResource(dictyDatFileName).toURI())
	    val propertiesBuilder = Map.newBuilder[String, Any]
    	propertiesBuilder += (MascotParseParams.ION_SCORE_CUTOFF.toString ->0.5)
	    val mascotDatFile = new MascotResultFile( f, propertiesBuilder.result, providerKey );
	    val rs:ResultSet = mascotDatFile.getResultSet( wantDecoy = false)  
	    	    
	    //Test Search Param info
	    testDictyDescription(rs)
	    
	    //Test PTM Providers worked well 
	    testDictyPeptidePtms(rs)
  		assertNotNull(rs)
  		assertEquals(21393, rs.peptideMatches.length)
  		assertEquals(11323, rs.proteinMatches.length)
	}
	
	@Test
    def testReadControlDatFile() = {
	    val f = new File( this.getClass().getResource(k12ControlDatFileName).toURI())
	    val mascotDatFile = new MascotResultFile( f, Map.empty, providerKey )
	    val rs: ResultSet = mascotDatFile.getResultSet( wantDecoy = false)
 		
	    assertNotNull(rs)
 		
	    //Test Search Param info
	    testK12Description(rs)	    
 	    //Test PTM Providers worked well 
	    testK12PeptidePtms(rs)
	        
  		assertEquals(13103, rs.peptideMatches.length)
  		assertEquals(19651, rs.proteinMatches.length)
	}
	
	private def testK12Description( rs:ResultSet )={
		val msiSearchDate  = Calendar.getInstance()
		msiSearchDate.setTime(rs.msiSearch.date)
		
		val expectedDate= Calendar.getInstance()
		expectedDate.set(2012,4,21)
		assertThat(msiSearchDate.get(Calendar.MONTH), CoreMatchers.equalTo(expectedDate.get(Calendar.MONTH)))
		assertThat(msiSearchDate.get(Calendar.DAY_OF_MONTH), CoreMatchers.equalTo(expectedDate.get(Calendar.DAY_OF_MONTH)))
		assertThat(msiSearchDate.get(Calendar.YEAR), CoreMatchers.equalTo(expectedDate.get(Calendar.YEAR)))
		
		assertThat(rs.msiSearch.jobNumber, CoreMatchers.equalTo(65306))		
		assertThat(rs.msiSearch.submittedQueriesCount, CoreMatchers.equalTo(7896))
		assertThat(rs.msiSearch.resultFileName, CoreMatchers.equalTo("F065306.dat"))
		assertThat(rs.msiSearch.searchedSequencesCount, CoreMatchers.equalTo(32182))
		assertThat(rs.msiSearch.title, CoreMatchers.equalTo("K12 controle LC-MS DH5 xtract 1% VELOS7546.raw"))
		assertThat(rs.msiSearch.userName, CoreMatchers.equalTo("Kieffer Sylvie"))
		
		assertThat(rs.msiSearch.searchSettings.fixedPtmDefs.length, CoreMatchers.equalTo(1))
		assertThat(rs.msiSearch.searchSettings.fixedPtmDefs(0).names.shortName, CoreMatchers.equalTo("Carbamidomethyl"))
		assertThat(rs.msiSearch.searchSettings.fixedPtmDefs(0).residue, CoreMatchers.equalTo('C'))
		assertThat(rs.msiSearch.searchSettings.variablePtmDefs.length, CoreMatchers.equalTo(2))
	}
	
	
	private def testDictyDescription( rs:ResultSet )={
	    
		val msiSearchDate  = Calendar.getInstance()
		msiSearchDate.setTime(rs.msiSearch.date)
		
		val expectedDate= Calendar.getInstance()
		expectedDate.set(2009,7,29)
		assertThat(msiSearchDate.get(Calendar.MONTH), CoreMatchers.equalTo(expectedDate.get(Calendar.MONTH)))
		assertThat(msiSearchDate.get(Calendar.DAY_OF_MONTH), CoreMatchers.equalTo(expectedDate.get(Calendar.DAY_OF_MONTH)))
		assertThat(msiSearchDate.get(Calendar.YEAR), CoreMatchers.equalTo(expectedDate.get(Calendar.YEAR)))
		
		assertThat(rs.msiSearch.jobNumber, CoreMatchers.equalTo(47876))
		assertThat(rs.msiSearch.queriesCount, CoreMatchers.equalTo(4004))
		assertThat(rs.msiSearch.submittedQueriesCount, CoreMatchers.equalTo(4004))
		assertThat(rs.msiSearch.resultFileName, CoreMatchers.equalTo("F047876.dat"))
		assertThat(rs.msiSearch.searchedSequencesCount, CoreMatchers.equalTo(26782))
		assertThat(rs.msiSearch.title, CoreMatchers.equalTo("ENDODDVE7_1T_20"))
		assertThat(rs.msiSearch.userName, CoreMatchers.equalTo("Brugiere Sabine"))
		
		assertThat(rs.msiSearch.searchSettings.fixedPtmDefs.length, CoreMatchers.equalTo(0))
		assertThat(rs.msiSearch.searchSettings.variablePtmDefs.length, CoreMatchers.equalTo(4))
		assertThat(rs.msiSearch.searchSettings.variablePtmDefs(0).names.shortName, CoreMatchers.equalTo("Acetyl"))
		assertThat(rs.msiSearch.searchSettings.variablePtmDefs(0).residue, CoreMatchers.equalTo('\0'))
	}
	
	private def testDictyPeptidePtms( rs:ResultSet )={
	  
	  //Test PTM Providers worked well 
	   val allPepMatches:Array[PeptideMatch] = rs.peptideMatches
	   val PtmShortNames = Array[String]("Acetyl", "Dioxidation", "Oxidation", "Trioxidation")
	   allPepMatches filter(!_.peptide.ptms.isEmpty) foreach{ pepMatch =>
//	     logger.debug(" PepPtm \t" + pepMatch.peptide.sequence+"\t"+pepMatch.msQuery.initialId+"\t"+pepMatch.rank+"\t"+pepMatch.peptide.ptmString)
	     val pepPtms : Array[fr.proline.core.om.model.msi.LocatedPtm] = pepMatch.peptide.ptms
	     pepPtms.foreach { locatedPtm =>  
	          assertTrue(PtmShortNames.contains(locatedPtm.definition.names.shortName))
	          if( locatedPtm.definition.names.shortName.equals(PtmShortNames(0)) )
	            assertTrue(locatedPtm.isNTerm)
	          else if( locatedPtm.definition.names.shortName.equals(PtmShortNames(1))  || locatedPtm.definition.names.shortName.equals(PtmShortNames(2)))
	            assertTrue(locatedPtm.definition.residue.equals('M'))
	          else 
	            assertTrue(locatedPtm.definition.residue.equals('C'))
	     }
		}
	   
	   allPepMatches filter( nextPepMatch => nextPepMatch.msQuery.initialId.equals(385) && nextPepMatch.rank ==1) foreach {pepMatch =>
//	   	 logger.debug(" --------------- QUERY 385 !! "+pepMatch.peptide.sequence+ " --- " +pepMatch.peptide.ptms(0).definition.names.shortName)
 	     assertEquals("Oxidation", pepMatch.peptide.ptms(0).definition.names.shortName)
	   }

	}
	
	private def testK12PeptidePtms( rs:ResultSet )={
	  
	  //Test PTM Providers worked well 
	   val allPepMatches:Array[PeptideMatch] = rs.peptideMatches
	   val PtmShortNames = Array[String]("Acetyl", "Carbamidomethyl", "Oxidation")
	   allPepMatches filter(!_.peptide.ptms.isEmpty) foreach{ pepMatch =>
//	     logger.debug(" PepPtm \t" + pepMatch.peptide.sequence+"\t"+pepMatch.msQuery.initialId+"\t"+pepMatch.rank+"\t"+pepMatch.peptide.ptmString)
	     val pepPtms : Array[fr.proline.core.om.model.msi.LocatedPtm] = pepMatch.peptide.ptms
	     pepPtms.foreach { locatedPtm =>  
	          assertTrue(PtmShortNames.contains(locatedPtm.definition.names.shortName))
	          if( locatedPtm.definition.names.shortName.equals(PtmShortNames(0)) )
	            assertTrue(locatedPtm.isNTerm)
	          else if( locatedPtm.definition.names.shortName.equals(PtmShortNames(1)))
	            assertTrue(locatedPtm.definition.residue.equals('C'))
	          else 
	            assertTrue(locatedPtm.definition.residue.equals('M'))
	     }
		}
	   
	   allPepMatches filter( nextPepMatch => nextPepMatch.msQuery.initialId.equals(35) && nextPepMatch.rank ==1) foreach {pepMatch =>
//	   	 logger.debug(" --------------- QUERY 385 !! "+pepMatch.peptide.sequence+ " --- " +pepMatch.peptide.ptms(0).definition.names.shortName)
 	     assertEquals("Carbamidomethyl", pepMatch.peptide.ptms(0).definition.names.shortName)
	   }

	}
	
	class PSDatabaseTestCase extends DatabaseTestCase {
	  override def getDatabase() = Database.PS
	}
	
	class PDIDatabaseTestCase extends DatabaseTestCase {
	  override def getDatabase() = Database.PDI
	}
}