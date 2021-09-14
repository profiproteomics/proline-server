package fr.proline.module.parser.mascot

import com.typesafe.scalalogging.LazyLogging
import fr.profi.chemistry.model.MolecularConstants
import fr.proline.core.om.model.msi._
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext
import fr.proline.core.om.provider.msi.{IPTMProvider, IPeptideProvider}
import matrix_science.msparser._

import scala.collection.mutable.{ArrayBuffer, HashMap}

object MascotDataParser {

  val LATIN_1_CHARSET = "ISO-8859-1"
  val PEPTIDE_ISOTOPE_SHIFT=1.00335

}

class MascotDataParser(
  val pepSummary: ms_peptidesummary,
  val mascotResFile: ms_mascotresfile,
  val searchSettings: SearchSettings,
  val mascotSearchParams: ms_searchparams,
  val msQueryByInitialId: Map[Int, MsQuery],
  val parserContext: ProviderDecoratedExecutionContext,
  val isDecoy: Boolean) extends LazyLogging {

  private var pepByUniqueKey: HashMap[String, Peptide] = null
  private var pepToPeptideMatches: HashMap[Peptide, ArrayBuffer[PeptideMatch]] = null

  /**
   * References ProteinWrapper by ProteinAcc_SeqDbId key
   */
  private var protAccSeqDbToProteinWrapper: HashMap[Tuple2[String, String], ProteinWrapper] = null
  
  /**
   * References ProteinMatch by ProteinWrapper. If associated Protein, it will be store in
   * ProteinWrapper else the accession / seqdatabase id will be store in wrapper.
   */
  private var wrappedProtToProtMatch: HashMap[ProteinWrapper, ProteinMatch] = null
  
  private var accessionToProteinMatches: HashMap[String, ProteinMatch] = null

  /**
   *  Get Resulting Peptide Matches (for each identified peptide)
   */
  def getPeptideMatchesByPeptide(): HashMap[Peptide, Array[PeptideMatch]] = {
    pepToPeptideMatches.map { case (k, v) => k -> v.toArray }
  }

  /**
   *  Get Resulting Protein Matches
   */
  def getProteinMatches(): Array[ProteinMatch] = accessionToProteinMatches.values.toArray

  /**
   * Create all necessary data for each peptide Matches identifying at least one protein.
   * Created objects are :
   * - PeptideMatch & Query associated to new or existing Peptide
   * - SequenceMatch and ProteinMatch for identified Proteins (new or existing)
   *
   *
   */
  def parseMatches(rsId: Long): Boolean = {

    //Get Necessary providers : for peptide, protein and ptms
    val pepProvider = parserContext.getProvider(classOf[IPeptideProvider])
    val ptmProvider = parserContext.getProvider(classOf[IPTMProvider])
    
    // parsed data
    val bestPepMatchByPepKey = new HashMap[String, PeptideMatch]()
    pepByUniqueKey = new HashMap[String, Peptide]()
    pepToPeptideMatches = new HashMap[Peptide, ArrayBuffer[PeptideMatch]]()
    wrappedProtToProtMatch = new HashMap[ProteinWrapper, ProteinMatch]()
    protAccSeqDbToProteinWrapper = new HashMap[Tuple2[String, String], ProteinWrapper]()

    val nbrQueries = mascotResFile.getNumQueries()
    val maxRankPerQuery = pepSummary.getMaxRankValue()

    var totalNbrPepMatches = 0
    var totalNbrSeqMatches = 0
    logger.debug("Go through " + nbrQueries + " queries using max rank " + maxRankPerQuery)

    for (q <- 1 to nbrQueries) { // Go through each Query

      val query = msQueryByInitialId(q)
      val queryPepMatches = new ArrayBuffer[(PeptideMatch, Seq[LocatedPtm])]()
      
      for (k <- 1 to maxRankPerQuery) { // Go through all peptides of each queries

        //***** Get  ms_peptide and create / get corresponding Peptide object
        val currentMSPep: ms_peptide = pepSummary.getPeptide(q, k)

        // Check that the peptide is not empty
        if (currentMSPep.getAnyMatch()) {

          val (parsedPep: Peptide, varPtms: Seq[LocatedPtm]) = getOrCreatePeptide(currentMSPep, ptmProvider, pepProvider)

          val pepMatchScore = currentMSPep.getIonsScore().floatValue()

          // --- Retrieve some properties values for PeptideMatchProperties --- //
          val pepMatchExpectValue = pepSummary.getPeptideExpectationValue(pepMatchScore, q)
          val readIsotopeErr = currentMSPep.getNum13C(mascotSearchParams.getTOL,mascotSearchParams.getTOLU,mascotSearchParams.getMASS)
          val isotopeErr = if(readIsotopeErr > -1) readIsotopeErr else 0

          // Create peptide match Mascot properties
          val pepMatchMascotProps = new PeptideMatchMascotProperties(expectationValue = pepMatchExpectValue)

          // Add readable variable mods and positions if the peptide has at least one PTM
          val readableVarMods = pepSummary.getReadableVarMods(q, k)
          if (readableVarMods.length > 0) {
            pepMatchMascotProps.setReadableVarMods(Some(readableVarMods))
            pepMatchMascotProps.setVarModsPositions(Some(currentMSPep.getVarModsStr))
          }
          var nlString = currentMSPep.getPrimaryNlStr()
          if (!nlString.isEmpty() && mascotResFile.getMascotVer.startsWith("2.2")) {
            // swap 1 and 2 indices
            nlString = nlString.replace('2', 'X').replace('1', '2').replace('X', '1')
          }
          if (!nlString.isEmpty()) {
            pepMatchMascotProps.setNlString(Some(nlString))
          }
          pepMatchMascotProps.setUsedPeaksCount(Some(currentMSPep.getPeaksUsedFromIons1()))

          val ambiguityStr = currentMSPep.getAmbiguityString()
          if (ambiguityStr.length > 0) {
            pepMatchMascotProps.setAmbiguityString(Some(ambiguityStr))
          }

          val pepMatchProps = new PeptideMatchProperties(mascotProperties = Some(pepMatchMascotProps), isotopeOffset=Some(isotopeErr))

          val pepMatchCharge =  currentMSPep.getCharge
          // Calculate deltaMoz using isotopeErr
          val calculatedDeltaMoz : Float = if(isotopeErr == 0) {
            (currentMSPep.getDelta() / query.charge).toFloat
          } else {
            val calcMoz = ( currentMSPep.getMrCalc + (isotopeErr * MascotDataParser.PEPTIDE_ISOTOPE_SHIFT) + (pepMatchCharge * MolecularConstants.PROTON_MASS)) / pepMatchCharge
            val newDelta = (query.moz - calcMoz).toFloat
            logger.trace(" ----- Changed deltaMoz from "+((currentMSPep.getDelta() / query.charge).toFloat)+" TO "+newDelta+" for "+ currentMSPep.getPeptideStr(false))
            newDelta
          }

          val pepMatch = new PeptideMatch(
            id = PeptideMatch.generateNewId,
            rank = k,
            score = pepMatchScore,
            scoreType = PeptideMatchScoreType.MASCOT_IONS_SCORE,
            charge = pepMatchCharge,
            deltaMoz = calculatedDeltaMoz, // getDelta returns expMass - calcMass
            isDecoy = isDecoy,
            peptide = parsedPep,
            missedCleavage = currentMSPep.getMissedCleavages(),
            fragmentMatchesCount = currentMSPep.getNumIonsMatched(),
            msQuery = query,
            properties = Some(pepMatchProps),
            resultSetId = rsId
          )
          if (readableVarMods.length > 0) {
            queryPepMatches += Tuple2(pepMatch, varPtms)
          }
          totalNbrPepMatches += 1

          // Save PeptideMatch In Peptide->PeptideMatch result Map
          pepToPeptideMatches.getOrElseUpdate(parsedPep, new ArrayBuffer[PeptideMatch]) += pepMatch

        }

      } // End go through current query Peptides
      
      // then compute ptm site probability property
      val pepMatchesBySequence: Map[String, ArrayBuffer[(PeptideMatch, Seq[LocatedPtm])]] = queryPepMatches.groupBy { e => e._1.peptide.sequence }
      for (matches <- pepMatchesBySequence.valuesIterator) {
        computeRelativeProbabilities(matches)
      }
      
    } // End go through Queries

    logger.debug("Go through Queries/PSM done")
    logger.debug("Search for " + pepToPeptideMatches.size + " different Peptides.")
    val pepsToSearch = Array.newBuilder[Pair[String, Array[LocatedPtm]]]
    pepToPeptideMatches.foreach(entry => {
      pepsToSearch += Pair(entry._1.sequence, entry._1.ptms)
    })
    val searchPeps = pepsToSearch.result

    val foundPeps = pepProvider.getPeptidesAsOptionsBySeqAndPtms(searchPeps)
    logger.debug("Found {}/{} Peptides in Datastore : ", foundPeps.filter(_.isDefined).size, searchPeps.size)

    // Iterate over found peptides in order to put them into existing peptide matches
    for (foundPepOpt <- foundPeps; fPep <- foundPepOpt) {

      val uniqueKey = fPep.uniqueKey

      // Replace old peptide definition by the new one in the map pepByUniqueKey
      val oldPep = pepByUniqueKey.put(uniqueKey, fPep).get

      if (pepToPeptideMatches.contains(oldPep)) {

        // Remove peptide matches mapping from the map pepToPeptideMatches
        val pepMatches = pepToPeptideMatches.remove(oldPep).get

        // Replace peptide definition of peptide matches by the new one
        pepToPeptideMatches += fPep -> pepMatches.map(_.copy(peptide = fPep))

      }
    } // End of for loop

    // Determine best peptide match for each peptide
    logger.debug("Determining the best PeptideMatch for each Peptide...")
    for ((pep, pepMatches) <- pepToPeptideMatches) {
      val bestPepMatch = PeptideMatch.getBestOnScoreDeltaMoZ(pepMatches.toArray)
      // currentMSPep.getAmbiguityString() + "%"+
      bestPepMatchByPepKey.update(pep.uniqueKey, bestPepMatch)
    }

    // Second pass to build protein matches and sequence matches
    logger.debug("Go through " + bestPepMatchByPepKey.size + " best PeptideMatches")
    for (bestPepMatch <- bestPepMatchByPepKey.values) {

      val query = bestPepMatch.msQuery
      val q = query.initialId
      val k = bestPepMatch.rank

      // Get Protein Matches for peptide identified by its query and rank
      val preAAs = new VectorString()
      val postAAs = new VectorString()
      val starts = new vectori()
      val ends = new vectori()
      val frames = new vectori()
      val multiplicity = new vectori()
      val dbs = new vectori()
      val protAcc: VectorString = pepSummary.getAllProteinsWithThisPepMatch(q, k, starts, ends, preAAs, postAAs, frames, multiplicity, dbs)

      try {

        if (protAcc.size() > 0) { // A least one protein matched

          val parsedPep = bestPepMatch.peptide

          // *****  go through matched proteins
          for (indProt <- 0 until protAcc.size().intValue()) {

            totalNbrSeqMatches += 1

            // matchedProt is valid only if parse using grouping !
            // val matchedProt: ms_protein = pepSummary.getProtein(protAcc.get(indProt), dbs.get(indProt))

            //Get SeqDatabase in which current protein was retrieve from
            val seqDbs: Array[SeqDatabase] = searchSettings.seqDatabases filter { _.name == mascotResFile.params.getDB(dbs.get(indProt)) }

            // --- Get or create ProteinWrapper, and Protein if defined, for matched Protein ---
            
            // Check if the protein has been already accessed            
            val protWrapperKey = new Tuple2(protAcc.get(indProt), (dbs.get(indProt).toString))
            val protOpt = if (protAccSeqDbToProteinWrapper.contains(protWrapperKey)) {
              protAccSeqDbToProteinWrapper.get(protWrapperKey).get.wrappedProt
            } else {
              // TODO: Try to get Protein from repository
              val tmpProtOpt = None
              protAccSeqDbToProteinWrapper += protWrapperKey -> new ProteinWrapper(dbs.get(indProt), protAcc.get(indProt), tmpProtOpt)
              
              tmpProtOpt
            }

            //Create SequenceMatch and ProteinMatch, if necessary, for current Matched Protein
            val seqMatch = new SequenceMatch(
              start = starts.get(indProt),
              end = ends.get(indProt),
              residueBefore = preAAs.get(indProt).charAt(0),
              residueAfter = postAAs.get(indProt).charAt(0),
              isDecoy = isDecoy,
              peptide = Some(parsedPep),
              bestPeptideMatch = Some(bestPepMatch),
              resultSetId = rsId
            )

            // Get ProteinMatch associated to this protein, through its ProteinWrapper
            val protMatchOpt: Option[ProteinMatch] = wrappedProtToProtMatch.get(protAccSeqDbToProteinWrapper.get(protWrapperKey).get)

            val protMatch = if (protMatchOpt.isDefined) protMatchOpt.get
            else {
              val seqDbIds = seqDbs map { _.id }
              val protMatchAc = protAcc.get(indProt)
              val protMatchDesc = pepSummary.getProteinDescription(protMatchAc, dbs.get(indProt))

              // Not already define, create ProteinMatch and add new entry in protToProtMatch
              val tmpProtMatch = new ProteinMatch(
                id = ProteinMatch.generateNewId,
                accession = protMatchAc,
                description = protMatchDesc,
                peptideMatchesCount = 0,
                scoreType = "mascot:standard score",
                isDecoy = isDecoy,
                protein =protOpt, //If prot is None => No protein is defined not protein not retrieve !
                seqDatabaseIds = seqDbIds,
                resultSetId = rsId
              )

              wrappedProtToProtMatch += (protAccSeqDbToProteinWrapper.get(protWrapperKey).get -> tmpProtMatch)
              
              tmpProtMatch
            }
            
            // Add created seqMatch to Protein Match
            val newSeqMatches = new ArrayBuffer[SequenceMatch]()
            if (protMatch.sequenceMatches != null) newSeqMatches ++= protMatch.sequenceMatches
            newSeqMatches += seqMatch

            protMatch.sequenceMatches = newSeqMatches.toArray

            // Update Protein Match score
            protMatch.score = protMatch.score + bestPepMatch.score

            //Update  Protein Match peptideMatchesCount
            protMatch.peptideMatchesCount = protMatch.peptideMatchesCount + pepToPeptideMatches.get(bestPepMatch.peptide).get.length

          } //End go through matched proteins

        } // End current Peptide match at least one Protein.

      } finally {
        /* Free memory in finally block (reverse order) */

        if (protAcc != null) {
          try {
            protAcc.clear()
            protAcc.delete()
          } catch {
            case t: Throwable => logger.error("Error deleting protAcc", t)
          }
        }

        try {
          dbs.clear()
          dbs.delete()
        } catch {
          case t: Throwable => logger.error("Error deleting dbs", t)
        }

        try {
          multiplicity.clear()
          multiplicity.delete()
        } catch {
          case t: Throwable => logger.error("Error deleting multiplicity", t)
        }

        try {
          frames.clear()
          frames.delete()
        } catch {
          case t: Throwable => logger.error("Error deleting frames", t)
        }

        try {
          ends.clear()
          ends.delete()
        } catch {
          case t: Throwable => logger.error("Error deleting ends", t)
        }

        try {
          starts.clear()
          starts.delete()
        } catch {
          case t: Throwable => logger.error("Error deleting starts", t)
        }

        try {
          postAAs.clear()
          postAAs.delete()
        } catch {
          case t: Throwable => logger.error("Error deleting postAAs", t)
        }

        try {
          preAAs.clear()
          preAAs.delete()
        } catch {
          case t: Throwable => logger.error("Error deleting preAAs", t)
        }

      } // End of try - finally block

    }

    // just before the creation of the RS, loop on ProteinMatches and merge those with same AC
    accessionToProteinMatches = new HashMap[String, ProteinMatch]
    // loop on each protein matches
    protAccSeqDbToProteinWrapper.foreach{ case (key, proteinWrapper) => {
      val accession = key._1
      val seqDbId = key._2.toLong
      if(accessionToProteinMatches.contains(accession)) {
        // update this protein match
        val finalProteinMatch = accessionToProteinMatches.get(accession).get
        // add seqDbId if not already present
        if(!finalProteinMatch.seqDatabaseIds.contains(seqDbId)) {
          finalProteinMatch.seqDatabaseIds = finalProteinMatch.seqDatabaseIds ++ Array(seqDbId)
        }
        // add sequence matches
        wrappedProtToProtMatch.get(proteinWrapper).get.sequenceMatches.foreach(seqMatch => {
          if(!finalProteinMatch.sequenceMatches.contains(seqMatch)) {
            finalProteinMatch.sequenceMatches = finalProteinMatch.sequenceMatches ++ Array(seqMatch)
          }
        })
      } else {
        // first occurence of the protein match, add it to the list
        accessionToProteinMatches.put(accession, wrappedProtToProtMatch.get(proteinWrapper).get)
      }
    }}

    logger.debug("Found " + totalNbrSeqMatches + " SeqMatches for " + totalNbrPepMatches + " PeptidesMatches")
    return true

  }

  /**
   * Search for OM Peptide corresponding to specified ms_peptide. Search / Creation is done as follow
   *   - Calculate associated unique key and retrieve peptide from pepByUniqueKey map
   *   - Get peptide from repository using specified IPTMProvider (and store it in pepByUniqueKey)
   *   - Create new one !(and store it in pepByUniqueKey)
   */
  private def getOrCreatePeptide(
    mascotPeptide: ms_peptide,
    ptmProvider: IPTMProvider,
    pepProvider: IPeptideProvider
  ): (Peptide, Seq[LocatedPtm]) = {

    // Disable substitution of ambiguous residues
    val pepSeq = mascotPeptide.getPeptideStr(false)

    //--- Create PTMs : from search settings variable and fixed PTM
    val pepVarPtm = createPeptidePtmFromVarPtm(mascotPeptide, ptmProvider)
    val pepFixedMod = createPeptidePtmFromFixedPtm(mascotPeptide)
    val varPtmPositions = pepVarPtm.map(_.seqPosition);
    val pepVarAndFixedPtm = pepVarPtm ++ pepFixedMod.filter(fm => !varPtmPositions.contains(fm.seqPosition))
    val ptmsAsArray = pepVarAndFixedPtm.toArray[LocatedPtm]

    // 1. retrieve peptide from pepByUniqueKey
    val uniqueKey = pepSeq + "%" + Peptide.makePtmString(ptmsAsArray)
    var currentPep = pepByUniqueKey.get(uniqueKey)
    var storeInMap = false

    // 2. get peptide from repository
//    if (currentPep.isEmpty) {
//      storeInMap = true
//      // 	  currentPep = pepProvider.getPeptide(pepSeq, varPtmsasArray)
//    }

    // 3. create new one
    if (currentPep.isEmpty) {
       storeInMap = true
      // FIXME: include fixed PTMs
      currentPep = Some(new Peptide(
        sequence = pepSeq,
        ptms = ptmsAsArray,
        calculatedMass = mascotPeptide.getMrCalc()
      ))
    }

    if (storeInMap) pepByUniqueKey += (uniqueKey -> currentPep.get)

    (currentPep.get, pepVarPtm)
  }

  /**
   * Create LocatedPtm for specified Peptide using variable PTM information
   */
  private def createPeptidePtmFromVarPtm(mascotPeptide: ms_peptide, ptmProvider: IPTMProvider): ArrayBuffer[LocatedPtm] = {
    val pepVarPtms = new ArrayBuffer[LocatedPtm](0)

    // Create Variable PTMs: create LocatedPtm using ms-peptide modsChars
    val modsChars: Array[Char] = mascotPeptide.getVarModsStr.toCharArray() //Mascot peptide PTMs string
    for (k <- 0 until modsChars.length) {

      val numMods: Int = modsChars.apply(k) - 48 //modsChars contains ascii value for char 0->9 so 48 -> 59.
      // If a modification is defined
      if (numMods != 0) {

        val mascotMod = mascotResFile.params().getVarModsName(numMods) //Mascot Modif Name

        // Get corresponding PtmDefinition
        val msVarPtms = MascotPTMUtils.mascotModToPTMDefinitions(ptmProvider, mascotMod)
        if (msVarPtms.isEmpty) {
          throw new Exception("Undefined PTM specified for peptide " + mascotPeptide.getQuery + " - " + mascotPeptide.getRank + " (" + mascotPeptide.getPeptideStr + ")")
        }

        // Get PTM location on Peptide
        var location = k
        if (k == (modsChars.length - 1))
          location = -1

        // store new locatedPTM for peptide
        try {
          // find the ptmDefinition corresponding to the residue in the peptide sequence
          pepVarPtms += LocatedPtm(ptmDef = msVarPtms.filter(_.residue.equals(mascotPeptide.getPeptideStr().charAt(location-1))).head, seqPos = location)
        } catch {
          // default value is the first ptmDefinition
          case e: Exception => pepVarPtms += LocatedPtm(ptmDef = msVarPtms(0), seqPos = location)
        }
      } // END A PTM exist on current residue

    } // END Go through each sequence residue

    pepVarPtms
  }

  /**
   * Create LocatedPtm for specified Peptide
   * using variable PTM information
   *
   */
  private def createPeptidePtmFromFixedPtm(mascotPeptide: ms_peptide): ArrayBuffer[LocatedPtm] = {
    val pepFixedPtms = new ArrayBuffer[LocatedPtm](0)
    val fixedPtms = searchSettings.fixedPtmDefs

    fixedPtms foreach (nextPTMDef => {
      //Get all residues modified by this FixedPTM and test if peptide is concerned.
      var pepResidueIndexes: Seq[Int] = null
      val modifResidue = nextPTMDef.residue
      val modifLocation = PtmLocation.withName(nextPTMDef.location)
      var isNter = false
      var isCter = false
      var ptmMatchPep = false
      var seqPos = -1

      modifLocation match {
        case PtmLocation.ANY_C_TERM => { //Modification is a C-Term modification, so for all peptides
          isCter = true
          seqPos = -1
          ptmMatchPep = true
        }
        case PtmLocation.ANY_N_TERM => {//Modification is a N-Term modification, so for all peptides
          isNter = true
          seqPos = 0
          ptmMatchPep = true
        }

        case PtmLocation.PROT_C_TERM => { //Modification is a Prot C-Term modification, so for all peptides which match the C-Term of a protein
          if (mascotPeptide.getAnyMatch) {
            val matchedProt = mascotPeptide.getProtein(0)
            val residueAfterPep = matchedProt.getPeptideResidueAfter(matchedProt.getPepNumber(mascotPeptide.getQuery(), mascotPeptide.getRank()))
            if (residueAfterPep == '-') { // peptide is at the CTerm of protein
              ptmMatchPep = true
              seqPos = -1
              isCter = true
            }
          } else {
            logger.warn(" FIXED PROT_C_TERM SPECIFIED, No Match for peptide ...... ")
          }
        }

        case PtmLocation.PROT_N_TERM => { //Modification is a Prot N-Term modification, so for all peptides which match the N-Term of a protein
          if (mascotPeptide.getAnyMatch) {
            val matchedProt = mascotPeptide.getProtein(0)
            val residueBeforePep = matchedProt.getPeptideResidueBefore(matchedProt.getPepNumber(mascotPeptide.getQuery(), mascotPeptide.getRank()))
            if (residueBeforePep == '-') { // peptide is at the CTerm of protein
              ptmMatchPep = true
              seqPos = 0
              isNter = true
            }
          } else {
            logger.warn(" FIXED PROT_N_TERM SPECIFIED, No Match for peptide ...... ")
          }
        }

        case PtmLocation.ANYWHERE => {
          //Verify specified residue is in peptide sequence
          var pepSeq = mascotPeptide.getPeptideStr().toCharArray.toList
          pepResidueIndexes = pepSeq.view.zipWithIndex.filter { _._1 == nextPTMDef.residue }.map { _._2 }.force
          if (!pepResidueIndexes.isEmpty)
            ptmMatchPep = true
        }
      }

      if (ptmMatchPep) {
        if (!isNter && !isCter) {
          pepResidueIndexes foreach (p => {
            seqPos = p + 1 //0 for N-term... for residue start at 1
            val newLocatedPtm = new LocatedPtm(
              definition = nextPTMDef,
              seqPosition = seqPos,
              monoMass = nextPTMDef.precursorDelta.monoMass,
              averageMass = nextPTMDef.precursorDelta.averageMass,
              composition = nextPTMDef.precursorDelta.composition,
              isNTerm = isNter,
              isCTerm = isCter
            )
            pepFixedPtms += newLocatedPtm
          })
        } else {
          val newLocatedPtm = new LocatedPtm(
            definition = nextPTMDef,
            seqPosition = seqPos,
            monoMass = nextPTMDef.precursorDelta.monoMass,
            averageMass = nextPTMDef.precursorDelta.averageMass,
            composition = nextPTMDef.precursorDelta.composition,
            isNTerm = isNter,
            isCTerm = isCter
          )
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

  private def computeRelativeProbabilities(assignments: Seq[(PeptideMatch, Seq[LocatedPtm])], md10Prob: Float = 0.1f) = {

    val confidenceByPepMatch = new HashMap[PeptideMatch, Float]()
    val confidenceSumBySite = new HashMap[LocatedPtm, Float]()
    val calcConfidence = (s1: Float, si: Float) => scala.math.pow(10, md10Prob * (s1 - si)).toFloat

    var peptideMatchesConfidenceSum = 0.0f
    val firstRankPepMatchScore = assignments(0)._1.score

    for ((peptideMatch, varPtms) <- assignments) {

      val confidence = calcConfidence(-firstRankPepMatchScore, -peptideMatch.score)

      // Update peptide match confidence
      confidenceByPepMatch(peptideMatch) = confidence

      // Update total confidence for all peptide matches
      peptideMatchesConfidenceSum += confidence

      // Update confidence value of each PTM site
      for (ptm <- varPtms) {
        confidenceSumBySite(ptm) = confidenceSumBySite.getOrElse(ptm, 0.0f) + confidence
      }
    }

    for ((pepMatch, varPtms) <- assignments) {

      val mascotDeltaScore = confidenceByPepMatch(pepMatch) / peptideMatchesConfidenceSum

      val probabilityByPtmString = Map.newBuilder[String, Float]
      for (ptm <- varPtms) {
        probabilityByPtmString += ptm.toReadableString -> confidenceSumBySite(ptm) / peptideMatchesConfidenceSum
      }

      // Update peptide match PTM site properties
      val ptmSiteProperties = pepMatch.properties.get.ptmSiteProperties.getOrElse(PeptideMatchPtmSiteProperties())
      ptmSiteProperties.setMascotDeltaScore(Some(mascotDeltaScore))
      ptmSiteProperties.setMascotProbabilityBySite(Some(probabilityByPtmString.result))
      pepMatch.properties.get.ptmSiteProperties = Some(ptmSiteProperties)
    }

  }
   
  case class ProteinWrapper(seqDbId: Int, protAccess: String, wrappedProt: Option[Protein]) {

    override def equals(other: Any): Boolean = {
      other match {
        case otherRefProt: ProteinWrapper => return protAccess.equals(otherRefProt.protAccess) && seqDbId.equals(otherRefProt.seqDbId)
        case _                            => return false
      }
    }
  }

}


