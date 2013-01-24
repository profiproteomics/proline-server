package fr.proline.module.parser.mascot

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import com.weiglewilczek.slf4s.Logging

import fr.proline.core.om.builder.{PeptideBuilder,PtmDefinitionBuilder}
import fr.proline.core.om.model.msi._
import fr.proline.core.om.provider.msi.{IPTMProvider,IPeptideProvider,IProteinProvider}
import fr.proline.repository.DatabaseContext

import matrix_science.msparser.{ms_mascotresfile,ms_peptide,ms_peptidesummary,vectori,VectorString}

class MascotDataParser( val pepSummary: ms_peptidesummary,
                        val mascotResFile:ms_mascotresfile,
                        val searchSettings:SearchSettings,
                        val msQueryByInitialId: Map[Int,MsQuery],
                        val entityProviders: EntityProviders,
                        val isDecoy: Boolean ) extends Logging {
  
  private var pepByUniqueKey:HashMap[String, Peptide] = null
  private var pepToPeptideMatches:HashMap[Peptide, ArrayBuffer[PeptideMatch]] = null
     
  /**
   * References ProteinWrapper by ProteinAcc_SeqDbId key 
   */
  private var protAccSeqDbToProteinWrapper :HashMap[String, ProteinWrapper]  = null
  /**
   * References ProteinMatch by ProteinWrapper. If associated Protein, it will be store in 
   * ProteinWrapper else the accession / seqdatabase id will be store in wrapper.
   */
  private var wrappedProtToProtMatch:HashMap[ProteinWrapper, ProteinMatch] = null
  
  /**
   *  Get Resulting Peptide Matches (for each identified peptide)
   */
  def getPeptideMatchesByPeptide(): HashMap[Peptide, Array[PeptideMatch]] = {
    this.pepToPeptideMatches.map { case (k, v) => k -> v.toArray }
  }
  
  /**
   *  Get Resulting Protein Matches 
   */
  def getProteinMatches(): Array[ProteinMatch] = wrappedProtToProtMatch.values.toArray

  


  /**
   * Create all necessary data for each peptide Matches identifying at least one protein.
   * Created objects are : 
   * - PeptideMatch & Query associated to new or existing Peptide
   * - SequenceMatch and ProteinMatch for identified Proteins (new or existing)
   * 
   *   
   */
  def parseMatches(rsId:Int): Boolean = {
       
    //Get Necessary providers : for peptide, protein and ptms
    var pepProvider = entityProviders.peptideProvider
    var ptmProvider = entityProviders.ptmProvider
    var protProvider = entityProviders.proteinProvider

	  // parsed data
    val bestPepMatchByPepKey = new HashMap[String, PeptideMatch]()
    pepByUniqueKey = new HashMap[String, Peptide]()    
    pepToPeptideMatches = new HashMap[Peptide, ArrayBuffer[PeptideMatch]]()
    wrappedProtToProtMatch = new HashMap[ProteinWrapper, ProteinMatch]()
    protAccSeqDbToProteinWrapper = new HashMap[String, ProteinWrapper]()
    
    val nbrQueries = mascotResFile.getNumQueries()
    val maxRankPerQuery = pepSummary.getMaxRankValue()
  
    var totalNbrPepMatches = 0
    var totalNbrSeqMatches = 0
    logger.debug("go through "+nbrQueries+" queries using max rank "+maxRankPerQuery)
    
    for ( q <- 1 to nbrQueries ) { // Go through each Query
      
      val query = msQueryByInitialId(q)      
      for ( k <- 1 to maxRankPerQuery ) { // Go through all peptides of each queries
        
		    //***** Get  ms_peptide and create / get corresponding Peptide object
	      var currentMSPep: ms_peptide = pepSummary.getPeptide(q, k)	    
	        
	      // Check that the peptide is not empty
	      if( currentMSPep.getAnyMatch() ) {
	    	  
	    	  var parsedPep = this.getOrCreatePeptide( currentMSPep, ptmProvider, pepProvider )
	    			  
		        // -- Retrieve some properties values 
	    	  val pepMatchScore = currentMSPep.getIonsScore().floatValue()
	  	      val pepMatchExpectValue = this.pepSummary.getPeptideExpectationValue( pepMatchScore, q )		      

	  	      // Create peptide match Mascot properties
	  	      val pepMatchMascotProps = new PeptideMatchMascotProperties( expectationValue = pepMatchExpectValue )
	  	      
	  	      // Add readable variable mods and positions if the peptide has at least one PTM
	  	      val readableVarMods = this.pepSummary.getReadableVarMods(q, k)
	  	      if( readableVarMods.length > 0 ) {
	            pepMatchMascotProps.setReadableVarMods( Some(readableVarMods) )
	            pepMatchMascotProps.setVarModsPositions( Some(currentMSPep.getVarModsStr) )            
	  	      }
	  	      val ambiguityStr = currentMSPep.getAmbiguityString()
	  	      if( ambiguityStr.length > 0 ) {
	  	        pepMatchMascotProps.setAmbiguityString( Some(ambiguityStr) )
	  	      }
  	      
	  	      val pepMatchProps = new PeptideMatchProperties( mascotProperties = Some(pepMatchMascotProps) )
  	      
	  	      val pepMatch = new PeptideMatch( 
  		                         id = PeptideMatch.generateNewId,
  		                         rank = k,
  		                         score = pepMatchScore,
  		                         scoreType = "mascot:ions score",
  		                         deltaMoz = (currentMSPep.getDelta() / query.charge).toFloat, // getDelta returns expMass - calcMass
  		                         isDecoy = this.isDecoy,
  		                         peptide = parsedPep,
  		                         missedCleavage = currentMSPep.getMissedCleavages(),
  		                         fragmentMatchesCount = currentMSPep.getNumIonsMatched(),
  		                         msQuery = query,
  		                         properties = Some(pepMatchProps),
  		                         resultSetId = rsId
  		                         )
	  	      totalNbrPepMatches += 1
  	  	      
	  	      // Save PeptideMatch In Peptide->PeptideMatch result Map
	  	      pepToPeptideMatches.getOrElseUpdate(parsedPep, new ArrayBuffer[PeptideMatch]) += pepMatch	  	  		
  		    
        }
		    
      } // End go through current query Peptide
    } // End go through Queries
    
    logger.debug(" Go through query / pep done ")
    
    val searchPeps = Seq() ++ pepToPeptideMatches.map(entry => (entry._1.sequence, entry._1.ptms) )
    
    val foundPep = pepProvider.getPeptidesAsOptionsBySeqAndPtms(searchPeps)
    logger.debug(" Search Pep in PS : "+foundPep.size)
    foundPep.foreach(fPep => {
      if(fPep.isDefined){
        
      	val uniqueKey = fPep.get.uniqueKey
      	logger.trace("Search pep "+uniqueKey+"  => "+uniqueKey)
      	
      	val oldPep = pepByUniqueKey.get(uniqueKey).get
      	pepByUniqueKey.put(uniqueKey, fPep.get)
      	if(pepToPeptideMatches.get(oldPep).isDefined){
      		var pepMatches = pepToPeptideMatches.get(oldPep).get
      		var newPepMatches = new ArrayBuffer[PeptideMatch]
  				pepMatches.foreach(f =>  {
  					  val newPepMatch = new PeptideMatch( 
  		                         id = f.id,
  		                         rank =f.rank,
  		                         score = f.score,
  		                         scoreType = f.scoreType,
  		                         deltaMoz = f.deltaMoz,
  		                         isDecoy = f.isDecoy,
  		                         peptide = fPep.get,
  		                         missedCleavage = f.missedCleavage,
  		                         fragmentMatchesCount =f.fragmentMatchesCount,
  		                         msQuery = f.msQuery,
  		                         properties = f.properties,
  		                         resultSetId =f.resultSetId
  		                      )
  					  newPepMatches+=newPepMatch
      	  })
      	  pepToPeptideMatches += fPep.get-> newPepMatches
      	  pepToPeptideMatches.remove(oldPep)
      	} // End PepMatch associated to peptide
      } //End peptide found 
    })
      
    // Determine best peptide match for each peptide
    logger.debug("Determining the best peptide match for each peptide...")
    for( (pep, pepMatches) <- pepToPeptideMatches ) {
      
      var bestPepMatch = pepMatches(0)
      for( i <- 1 until pepMatches.length ) {
        val nextPepMatch = pepMatches(i)
        if( (bestPepMatch.score < nextPepMatch.score ) || ( (bestPepMatch.score == nextPepMatch.score)&& (bestPepMatch.id < nextPepMatch.id) ))
          bestPepMatch = nextPepMatch
      }
      
      // currentMSPep.getAmbiguityString() + "%"+
      bestPepMatchByPepKey.update( pep.uniqueKey, bestPepMatch )
    }
    
    // Second pass to build protein matches and sequence matches    
    logger.debug(" Go through bestPepMatchByPepKey "+bestPepMatchByPepKey.size)
    for( bestPepMatch <- bestPepMatchByPepKey.values ) {
      
      val query = bestPepMatch.msQuery
      val q = query.initialId
      val k = bestPepMatch.rank
      
      // Get Protein Matches for peptide identified by its query and rank
      var preAAs = new VectorString()
      var postAAs = new VectorString()
      var starts = new vectori()
      var ends = new vectori()
      var frames = new vectori()
      var multiplicity = new vectori()
      var dbs = new vectori()
      var protAcc: VectorString = pepSummary.getAllProteinsWithThisPepMatch(q, k, starts, ends, preAAs, postAAs, frames, multiplicity, dbs)
       
      if ( protAcc.size() > 0 ) { // A least one protein matched    
        
        var parsedPep = bestPepMatch.peptide
        
        // *****  go through matched proteins
        for (indProt <- 0 until protAcc.size().intValue()) {
          
          totalNbrSeqMatches += 1 
        		  
          // matchedProt is valid only if parse using grouping !
          // val matchedProt: ms_protein = pepSummary.getProtein(protAcc.get(indProt), dbs.get(indProt))
          
          //Get SeqDatabase in which current protein was retrieve from 
    	   val seqDbs : Array[SeqDatabase] = searchSettings.seqDatabases filter { _.name == mascotResFile.params.getDB(dbs.get(indProt)) }          
          
          //****  Get or created ProteinWrapper, and Protein if defined, for matched Protein         
          var prot = Option.empty[Protein]
        		  
          // Check if the protein has been already accessed
          val protWrapperKey =  protAcc.get(indProt)+(dbs.get(indProt).toString) 
          if(protAccSeqDbToProteinWrapper.contains(protWrapperKey)) {
            prot = protAccSeqDbToProteinWrapper.get(protWrapperKey).get.wrappedProt
          } else { 
        	  // Try to get Protein from repository  
        	  prot = protProvider.getProtein(protAcc.get(indProt),seqDbs(0))
			  protAccSeqDbToProteinWrapper += protWrapperKey -> new ProteinWrapper( dbs.get(indProt), protAcc.get(indProt), prot)
          }
          
          //Create SequenceMatch and ProteinMatch, if necessary, for current Matched Protein
          var seqMatch = new SequenceMatch(
                               start = starts.get(indProt),
                               end = ends.get(indProt),
                               residueBefore = preAAs.get(indProt).charAt(0),
                               residueAfter = postAAs.get(indProt).charAt(0), 
                               isDecoy = this.isDecoy,
                               peptide = Some(parsedPep),
                               bestPeptideMatch = Some(bestPepMatch),
                               resultSetId = rsId
                             )
          
          // Get ProteinMatch associated to this protein, through its ProteinWrapper
          var protMatchOpt : Option[ProteinMatch] = wrappedProtToProtMatch.get(protAccSeqDbToProteinWrapper.get(protWrapperKey).get)
          
          var protMatch : ProteinMatch = null
          if(protMatchOpt == None) {
            val seqDbIds = seqDbs map { _.id }
            val protMatchAc = protAcc.get(indProt)
            val protMatchDesc = pepSummary.getProteinDescription( protMatchAc )
            
            // Not already define, create ProteinMatch and add new entry in protToProtMatch
            protMatch = new ProteinMatch( id = ProteinMatch.generateNewId,
                                          accession = protMatchAc,
                                          description = protMatchDesc,
                                          peptideMatchesCount = 0, // FIXME: assign the right number
                                          scoreType = "mascot:standard score",
                                          isDecoy = this.isDecoy,
                                          protein = (if (prot == None) null else prot), //If prot is None => No protein is defined not protein not retrieve !
                                          seqDatabaseIds = seqDbIds,
                                          resultSetId = rsId
                                         )
            wrappedProtToProtMatch += (protAccSeqDbToProteinWrapper.get(protWrapperKey).get -> protMatch)
          } else {
             protMatch = protMatchOpt.get
          }
                        
          // Add created seqMatch to Protein Match
          val newSeqMatches = new ArrayBuffer[SequenceMatch]()
          if( protMatch.sequenceMatches != null ) newSeqMatches ++= protMatch.sequenceMatches           
          newSeqMatches += seqMatch
          
          protMatch.sequenceMatches = newSeqMatches.toArray
          
        } //End go through matched proteins
        
      } // End current Peptide match at least one Protein.
    }
    
    logger.debug(" Found nbr SeqMatch "+totalNbrSeqMatches+" for "+totalNbrPepMatches+" peptides matches")
    return true

  }
  
  /**
   * Search for OM Peptide corresponding to specified ms_peptide. Search / Creation is done as follow
   *   - Calculate associated unique key and retrieve peptide from pepByUniqueKey map
   *   - Get peptide from repository using specified IPTMProvider (and store it in pepByUniqueKey)
   *   - Create new one !(and store it in pepByUniqueKey)
   */
  private def getOrCreatePeptide(mascotPeptide: ms_peptide,
                                 ptmProvider: IPTMProvider,
                                 pepProvider: IPeptideProvider ): Peptide = {

    val (qId, pRank) = ( mascotPeptide.getQuery(), mascotPeptide.getRank() )
   
    // Disable substitution of ambiguous residues
    val pepSeq = mascotPeptide.getPeptideStr(false)
    
    //--- Create PTMs : from search settings variable and fixed PTM    
    var pepVarAndFixedPtm = new ArrayBuffer[LocatedPtm](0)
    pepVarAndFixedPtm ++= this.createPeptidePtmFromVarPtm(mascotPeptide, ptmProvider)      
    pepVarAndFixedPtm ++= this.createPeptidePtmFromFixedPtm(mascotPeptide)
    val varPtmsasArray = pepVarAndFixedPtm.toArray[LocatedPtm]
    
    // 1. retrieve peptide from pepByUniqueKey
    val uniqueKey = pepSeq + "%" + Peptide.makePtmString(varPtmsasArray) 
 	var currentPep = pepByUniqueKey.get( uniqueKey )
 	var storeInMap = false 
 	
 	 // 2. get peptide from repository 
 	if (currentPep == None ){
 	  storeInMap = true
// 	  currentPep = pepProvider.getPeptide(pepSeq, varPtmsasArray)
   	}
 	
 	// 3. create new one 
    if (currentPep == None ) {
      // FIXME: include fixed PTMs
      currentPep = Some( new Peptide( sequence = pepSeq, ptms =varPtmsasArray ) )
    }    

    if(storeInMap)
     pepByUniqueKey += ( uniqueKey->  currentPep.get)
    currentPep.get  
  }
  
  /**
   * Create LocatedPtm for specified Peptide using variable PTM information
   */
  private def createPeptidePtmFromVarPtm(mascotPeptide: ms_peptide, ptmProvider: IPTMProvider): ArrayBuffer[LocatedPtm] = {    
    var pepVarPtms = new ArrayBuffer[LocatedPtm](0)
    
    // Create Variable PTMs: create LocatedPtm using ms-peptide modsChars
    var modsChars:Array[Char] = mascotPeptide.getVarModsStr.toCharArray()    //Mascot peptide PTMs string    
    for (k <- 0 until modsChars.length) {
    	
      val numMods:Int = modsChars.apply(k) - 48 //modsChars contains ascii value for char 0->9 so 48 -> 59.      
      // If a modification is defined
      if (numMods != 0) {
        
        val mascotMod = mascotResFile.params().getVarModsName(numMods) //Mascot Modif Name
        		
        // Get corresponding PtmDefinition
        var msVarPtms = MascotPTMUtils.mascotModToPTMDefinitions(ptmProvider,mascotMod)
        if(msVarPtms.isEmpty){
          throw new Exception("Undefined PTM specified for peptide "+mascotPeptide.getQuery+" - "+mascotPeptide.getRank+" ("+mascotPeptide.getPeptideStr+")")
        } 
        
        // Get PTM location on Peptide
        var location = k
        if(k == (modsChars.length - 1))
          location = -1
          
        // store new locatedPTM for peptide
        pepVarPtms += PtmDefinitionBuilder.buildLocatedPtm(ptmDef= msVarPtms(0) , seqPos=location)         
      } // END A PTM exist on current residue
      
    } // END Go through each sequence residue
    
    pepVarPtms
  }
  
  
  /**
   * Create LocatedPtm for specified Peptide 
   * using variable PTM information
   * 
   */
  private def createPeptidePtmFromFixedPtm(mascotPeptide : ms_peptide) : ArrayBuffer[LocatedPtm] = {    
	var pepFixedPtms = new ArrayBuffer[LocatedPtm](0)
	val fixedPtms = searchSettings.fixedPtmDefs	
	
	fixedPtms foreach (nextPTMDef => {
		//Get all residues modified by this FixedPTM and test if peptide is concerned.
	    var pepResidueIndexes : Seq[Int]  = null
		val modifResidue = nextPTMDef.residue	
		val modifLocation = PtmLocation.withName(nextPTMDef.location)
		var isNter = false
		var isCter = false
		var ptmMatchPep = false
		var seqPos = -1
		
		modifLocation match {
		  case PtmLocation.C_TERM =>  {  //Modification is a C-Term modification, so for all peptides
		    isCter =true
		    seqPos = -1
		    ptmMatchPep = true
		  }
		  case PtmLocation.N_TERM =>{  //Modification is a N-Term modification, so for all peptides
		    isNter =true
		    seqPos = 0
		    ptmMatchPep = true
		  } 
		  
		  case PtmLocation.PROT_C_TERM=>{ //Modification is a Prot C-Term modification, so for all peptides which match the C-Term of a protein
		    if(mascotPeptide.getAnyMatch){
		      val matchedProt = mascotPeptide.getProtein(0)
		      val residueAfterPep= matchedProt.getPeptideResidueAfter(matchedProt.getPepNumber(mascotPeptide.getQuery(), mascotPeptide.getRank()))
		      if(residueAfterPep == '-') {// peptide is at the CTerm of protein
		    	  ptmMatchPep = true
		    	  seqPos = -1
		    	  isCter =true
		      }
		    } else {
		      logger.warn(" FIXED PROT_C_TERM SPECIFIED, No Match for peptide ...... ")
		    }
		  }
		  
		  case PtmLocation.PROT_N_TERM=>{ //Modification is a Prot N-Term modification, so for all peptides which match the N-Term of a protein
		    if(mascotPeptide.getAnyMatch){
		      val matchedProt = mascotPeptide.getProtein(0)
		      val residueBeforePep= matchedProt.getPeptideResidueBefore(matchedProt.getPepNumber(mascotPeptide.getQuery(), mascotPeptide.getRank()))
		      if(residueBeforePep == '-') {// peptide is at the CTerm of protein
		    	  ptmMatchPep = true
		    	  seqPos = 0
		    	  isNter =true
		      }
		    } else {
		      logger.warn(" FIXED PROT_N_TERM SPECIFIED, No Match for peptide ...... ")
		    }
		  }
		  
		  case PtmLocation.ANYWHERE => {
			  //Verify specified residue is in peptide sequence
			 var pepSeq = mascotPeptide.getPeptideStr().toCharArray.toList
			 pepResidueIndexes = pepSeq.view.zipWithIndex.filter { _._1 == nextPTMDef.residue}.map{_._2}.force
			 if(! pepResidueIndexes.isEmpty)
			   ptmMatchPep = true			 
		  }
		}
		
		if(ptmMatchPep){
		  if(!isNter && ! isCter ){
		    pepResidueIndexes foreach (p =>{
		    	seqPos = p
		    	val newLocatedPtm =  new LocatedPtm(  definition = nextPTMDef, 
                     seqPosition = seqPos,
                     monoMass = nextPTMDef.precursorDelta.monoMass,
                     averageMass = nextPTMDef.precursorDelta.averageMass,
                     composition = nextPTMDef.precursorDelta.composition,
                     isNTerm = isNter,
                     isCTerm = isCter )
			  pepFixedPtms += newLocatedPtm
		    })
		  } else {
			  val newLocatedPtm =  new LocatedPtm(  definition = nextPTMDef, 
                     seqPosition = seqPos,
                     monoMass = nextPTMDef.precursorDelta.monoMass,
                     averageMass = nextPTMDef.precursorDelta.averageMass,
                     composition = nextPTMDef.precursorDelta.composition,
                     isNTerm = isNter,
                     isCTerm = isCter )
			  pepFixedPtms += newLocatedPtm
		  }
		}
	})
		pepFixedPtms
  }
 

  /**
   * For test / debug purpose only !
   * return nbr matches
   */
  private def printPeptide(qId: Int, pRank: Int): Int = {

    //Get Protein Matches for peptide identified by its query and rank
    var preAAs: VectorString = new VectorString()
    var postAAs: VectorString = new VectorString()
    var starts: vectori = new vectori()
    var ends: vectori = new vectori()
    var frames: vectori = new vectori()
    var multiplicity: vectori = new vectori()
    var dbs: vectori = new vectori()
    var protAcc: VectorString = pepSummary.getAllProteinsWithThisPepMatch(qId, pRank, starts, ends, preAAs, postAAs, frames, multiplicity, dbs)

    //Get corresponding ms_peptide 
    var currentMSPep: ms_peptide = pepSummary.getPeptide(qId, pRank)
    var nbrMatches = 0
    // VDS => Always return 0 if ms_peptidesummary created with MSPEPSUM_NO_PROTEIN_GROUPING
    //    val nbrMatchedProtein = currentMSPep.getNumProteins()   
    //    println(" ** "+qId+","+pRank+" -- "+currentMSPep.getPeptideStr()+" => Match prot # "+ protAcc.size()+" vs "+nbrMatchedProtein)

    for (indProt <- 0 until protAcc.size().intValue()) {
      //        println("  ** "+qId+","+pRank+" === > Match "+protAcc.get(indProt)+" start/end/preAA/postAA/..."+starts.get(indProt)+"/"+ends.get(indProt)+"/"+preAAs.get(indProt)+"/"+postAAs.get(indProt)+"/")       
      //      val matchedProt: ms_protein = pepSummary.getProtein(protAcc.get(indProt), dbs.get(indProt))
      //      if (matchedProt != null) {
      //        val pepNbr = matchedProt.getPepNumber(qId, pRank)
      //        println("YES MatchedProt\t"+qId + "\t" + pRank + "\t" + currentMSPep.getPeptideStr() + "\t" + currentMSPep.getIonsScore() + "\t" + matchedProt.getPeptideIonsScore(pepNbr) + "\t")
      //      } else
      //        println("NO MatchedProt\t"+qId + "\t" + pRank + "\t" + currentMSPep.getPeptideStr() + "\t" + currentMSPep.getIonsScore() + "\t" )
      println(qId + "\t" + pRank + "\t" + currentMSPep.getPeptideStr() + "\t" + currentMSPep.getIonsScore() + "\t" + protAcc.size() + "\t" + starts.get(indProt) + "\t" + ends.get(indProt) + "\t" + preAAs.get(indProt) + "\t" + postAAs.get(indProt) + "\t" + protAcc.get(indProt))
      nbrMatches += 1
      //Score Test Grouped <> Ungrouped Peptide 
      //          if(matchedProt != null){
      //            val pepNbr = matchedProt.getPepNumber(q,k)
      //            println(q+"\t"+k+"\t"+matchedProt.getPeptideIonsScore(pepNbr)+"\t"+nextPep.getIonsScore())
      //          } else 
      //              println(q+"\t"+k+"\t"+"00"+"\t"+nextPep.getIonsScore()+"\t"+"NO MATCH PROT")

    }
    nbrMatches
  }
  
  case class ProteinWrapper (val seqdbId:Int, val protAccess: String, var wrappedProt:Option[Protein]) {    
    
    override def equals(other : Any) : Boolean = {
      other match {
        case otherRefProt : ProteinWrapper =>  return  protAccess.equals(otherRefProt.protAccess) && seqdbId.equals(otherRefProt.seqdbId) 
        case _ =>  return false
      }
    }
  }
  
}


