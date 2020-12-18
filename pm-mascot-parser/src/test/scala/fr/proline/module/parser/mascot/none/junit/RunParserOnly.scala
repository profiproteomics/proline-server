package fr.proline.module.parser.mascot.none.junit

import java.io.File

import com.typesafe.scalalogging.LazyLogging
import fr.profi.chemistry.model.MolecularConstants
import fr.proline.module.parser.mascot.MascotDataParser
import matrix_science.msparser._
import matrixscience.NativeLibrariesLoader

object RunParserOnly extends LazyLogging {

  /* Load Matrix Science Mascot Parser native libraries */
  val loaded = NativeLibrariesLoader.loadNativeLibraries
  if (!loaded) {
    val message = "Mascot Parser native libraries not loaded"
    logger.error(message)

    throw new RuntimeException(message)
  }

  private var _datFileName1: String = "/dat_samples/GRE_F068213_M2.4_TD_EColi.dat"  //"C:\\vero\\DEV\\Proline\\data\\Platcher\\F130423.dat"
  private var _datFileName2: String = "/dat_samples/Mascot_ErrorTolerant_F981130.dat"//F124428.dat" //error Tolerant search
  private var absoluteDatFileNameSet = false

  def runParse(datFileName: String): ms_mascotresfile = {
    logger.info(" --- Get File " + datFileName)
    var datFile: File = null
    if (absoluteDatFileNameSet)
      datFile = new File(datFileName)
    else
      datFile = new File(RunParserOnly.getClass.getResource(datFileName).toURI)

    val fileAbsolutePath = datFile.getAbsolutePath
    logger.info(" --- RunParserOnly  " + datFile.exists)
    // Create Mascot ms_mascotresfile and ms_peptidesummary from specified file name
    val mascotResFile: ms_mascotresfile = new ms_mascotresfile(fileAbsolutePath, 0, "")
    if (!mascotResFile.isValid()) throw new Exception("Invalid Mascot result file " + datFile.getAbsolutePath() + " specified : "+mascotResFile.getErrorString(1))
    val sp : ms_searchparams = mascotResFile.params()

    logger.debug(" mascotResFile.isErrorTolerant ? "+mascotResFile.isErrorTolerant)
    logger.debug(" sp.getERRORTOLERANT "+sp.getERRORTOLERANT)

    logger.debug(" sp.getACCESSION "+sp.getACCESSION)
    logger.debug(" sp.getErrTolParentFilename "+sp.getErrTolParentFilename)
    readMods(sp)
    mascotResFile

  }

  def readMods(mascotSearchParams : ms_searchparams): Unit = {
    // Retrieve PTMs, fixed and variable, specified for Mascot Search
    val mascotVarModsAsStr = mascotSearchParams.getIT_MODS()
    logger.debug("Search variable Ptms using Mascot string : " + mascotVarModsAsStr)

    val mascotFixedModsAsStr = mascotSearchParams.getMODS()
    logger.debug("Search fixed Ptms using Mascot string : " + mascotFixedModsAsStr)

  }

  def getPepSumm(mascotResultFile: ms_mascotresfile): ms_peptidesummary ={
    case object MascotResFlags {
      val NoGroup = ms_mascotresults.MSRES_NOFLAG
      val Group = ms_mascotresults.MSRES_GROUP_PROTEINS
      val Subsets = ms_mascotresults.MSRES_SHOW_SUBSETS
      val MudPIT = ms_mascotresults.MSRES_MUDPIT_PROTEIN_SCORE
      val Decoy = ms_mascotresults.MSRES_DECOY
      val ErrorTolerant = ms_mascotresults.MSRES_INTEGRATED_ERR_TOL
      val ShowAllFromErrTol = ms_mascotresults.MSRES_SHOW_ALL_FROM_ERR_TOL
    }

    var flagCombination = MascotResFlags.Group | MascotResFlags.Subsets | MascotResFlags.MudPIT
    if(mascotResultFile.isErrorTolerant) {
      flagCombination = flagCombination | MascotResFlags.ErrorTolerant //| MascotResFlags.ShowAllFromErrTol
    }
    val flags2 = if(mascotResultFile.isErrorTolerant)  ms_peptidesummary.MSPEPSUM_NONE else ms_peptidesummary.MSPEPSUM_NO_PROTEIN_GROUPING

    val (minProtProb, maxHitsToReport) = (0, 0) // Report All proteins
    val (unigeneIndexFile, minPepLenInPepSummary, singleHit) = (null, 0, null)

     new ms_peptidesummary(
      mascotResultFile,
      flagCombination,
      minProtProb,
      maxHitsToReport,
      unigeneIndexFile,
      0.0d,
      minPepLenInPepSummary,
      singleHit,
      flags2
    )

  }

  def printMsPepSum(pepSum: ms_peptidesummary): Unit ={
    //VDS : No groups with parser option...
    val nbrHit = pepSum.getNumberOfHits()
    val prot = pepSum.getHit(1)
    for(hit <- 1 to nbrHit) {
      val accession = prot.getAccession();
      val description = pepSum.getProteinDescription(accession);
      val mass = pepSum.getProteinMass(accession);
      val dbIdx = prot.getDB();

      val protein_hit = "Protein Hit " + hit;

      logger.debug(protein_hit + "\n===================");
      logger.debug("Accession   : " + accession);
      logger.debug("Description : " + description);
      logger.debug("Score       : " + prot.getScore());
      logger.debug("Mass        : " + mass);
      logger.debug("Frame       : " + prot.getFrame());
      logger.debug("Coverage    : " + prot.getCoverage());
      logger.debug("RMS error   : " + prot.getRMSDeltas(pepSum));
      logger.debug("Peptides    : " + prot.getNumDisplayPeptides());
    }
  }

  def readData(mascotResFile: ms_mascotresfile, pepSummary: ms_peptidesummary): Unit ={
    val nbrQueries = mascotResFile.getNumQueries()
    val maxRankPerQuery = pepSummary.getMaxRankValue()

    logger.debug("Go through " + nbrQueries + " queries using max rank " + maxRankPerQuery)
    val mascotSearchParams : ms_searchparams = mascotResFile.params( )
    val isErrT = mascotResFile.isErrorTolerant
    val startIndex = nbrQueries/2

    for (q <- startIndex to startIndex+50) { // Go through each Query

      for (k <- 1 to maxRankPerQuery) { // Go through all peptides of each queries

        //***** Get  ms_peptide and create / get corresponding Peptide object
        val currentMSPep: ms_peptide = pepSummary.getPeptide(q, k)

        // Check that the peptide is not empty
        if (currentMSPep.getAnyMatch()) {

          val pepMatchScore = currentMSPep.getIonsScore().floatValue()
          val pepSeq = currentMSPep.getPeptideStr(false)

          // --- Retrieve some properties values for PeptideMatchProperties --- //
          val pepMatchExpectValue = pepSummary.getPeptideExpectationValue(pepMatchScore, q)

          val readIsotopeErr = currentMSPep.getNum13C(mascotSearchParams.getTOL,mascotSearchParams.getTOLU,mascotSearchParams.getMASS)
          val isotopeErr = if(readIsotopeErr > -1) readIsotopeErr else 0

          // Add readable variable mods and positions if the peptide has at least one PTM
          val readableVarMods = pepSummary.getReadableVarMods(q, k)
          val varPtmsPos = currentMSPep.getVarModsStr
          var nlString = currentMSPep.getPrimaryNlStr()

          //Get Err Tol info
          val errTolModName = if(currentMSPep.getIsFromErrorTolerant) pepSummary.getErrTolModName(q,k) else "No ErrTol"
          val errTolModDelta = if(currentMSPep.getIsFromErrorTolerant) pepSummary.getErrTolModDelta(q,k) else "No ErrTol"
          val errTolModNL = if(currentMSPep.getIsFromErrorTolerant) pepSummary.getErrTolModNeutralLoss(q,k) else "No ErrTol"
          val errTolModMaster = if(currentMSPep.getIsFromErrorTolerant) pepSummary.getErrTolModMasterString(q,k) else "No ErrTol"

          val peaksUsedFromIons1 = currentMSPep.getPeaksUsedFromIons1()
          val ambiguityStr = currentMSPep.getAmbiguityString()

          val pepMatchCharge =  currentMSPep.getCharge
          val qCharge = mascotResFile.getObservedCharge(q)
          // Calculate deltaMoz using isotopeErr
          val calculatedDeltaMoz : Float = if(isotopeErr == 0) {
            (currentMSPep.getDelta() / qCharge).toFloat
          } else {
            val calcMoz = ( currentMSPep.getMrCalc + (isotopeErr * MascotDataParser.PEPTIDE_ISOTOPE_SHIFT) + (pepMatchCharge * MolecularConstants.PROTON_MASS)) / pepMatchCharge
            val newDelta = (mascotResFile.getObservedMass(q) - calcMoz).toFloat
            logger.debug(" ----- Changed deltaMoz from "+((currentMSPep.getDelta() / qCharge).toFloat)+" TO "+newDelta+" for "+ currentMSPep.getPeptideStr(false))
            newDelta
          }
          logger.debug("******-----  READ PepMatch from q " +q + "rank " +k +" : "+pepSeq)
          logger.debug(" pepMatchScore: " +pepMatchScore+" pepMatchCharge "+pepMatchCharge+" calculatedDeltaMoz "+calculatedDeltaMoz+ " pepMatchExpectValue: "+pepMatchExpectValue)
          logger.debug(" readableVarMods: " +readableVarMods+" varPtmsPos "+ varPtmsPos)
          logger.debug(" peaksUsedFromIons1 "+peaksUsedFromIons1+" nlString "+nlString +" ambiguityStr: " +ambiguityStr +" isotopeErr: "+isotopeErr )
          if(isErrT) {
            logger.debug( " errTolModName: " +errTolModName+" errTolModDelta "+ errTolModDelta)
            logger.debug( " errTolModNL "+errTolModNL +" errTolModMaster: " +errTolModMaster)
          }

          //READ PTMs info
          val modsChars: Array[Char] = varPtmsPos.toCharArray() //Mascot peptide PTMs string
          if(!currentMSPep.getIsFromErrorTolerant) {
            for (charX <- 0 until modsChars.length) {
              val numMods: Int = modsChars.apply(charX) - 48 //modsChars contains ascii value for char 0->9 so 48 -> 59.
              // If a modification is defined
              if (numMods != 0) {
                val mascotMod = mascotResFile.params().getVarModsName(numMods) //Mascot Modif Name
                // Get PTM location on Peptide
                var location = charX
                if (charX == (modsChars.length - 1))
                  location = -1
                try {
                  logger.debug(" READ mascotMod: " + mascotMod + " AT " + location + " RES " + currentMSPep.getPeptideStr().charAt(location - 1))
                } catch {
                  // default value is the first ptmDefinition
                  case e: Exception => logger.debug(" READ mascotMod: " + mascotMod + " AT " + location +" Term")
                }
              } // END A PTM exist on current residue
            } // END Go through each sequence residue
          } else {
            //Error Tolerant
            for (charX <- 0 until modsChars.length) {
              if('X'.equals(modsChars.apply(charX))){
                val errTolMod =  pepSummary.getErrTolModName(q,k)
                var location = charX
                if (charX == (modsChars.length - 1))
                  location = -1
                logger.debug(" READ ErrTol Modif : " + errTolMod + " At " + location)
              }
            }
          }

        }

      } // End go through current query Peptides

    } // End go through Queries
  }

  def main(args: Array[String]): Unit = {
    logger.debug("Start Logging Debug...RunParserOnly")
    val resultFileService = RunParserOnly
//    val fileName =  if(args.length>0) args.apply(0) else RunParserOnly._datFileName1
    val fileName =  RunParserOnly._datFileName1
    logger.debug("************************ Run Parse only on file  : " + fileName)
    val mascotResultFile1= resultFileService.runParse(fileName)
    val msPepSum1 = getPepSumm(mascotResultFile1)
    resultFileService.printMsPepSum(msPepSum1)
    resultFileService.readData(mascotResultFile1, msPepSum1)

    logger.debug("************************ Run Parse only on ErrTOl file  : " + RunParserOnly._datFileName2)
    val mascotResultFile2 = resultFileService.runParse(RunParserOnly._datFileName2)
    val msPepSum2 = getPepSumm(mascotResultFile2)
    resultFileService.printMsPepSum(msPepSum2)
    resultFileService.readData(mascotResultFile2, msPepSum2)
  }

}
