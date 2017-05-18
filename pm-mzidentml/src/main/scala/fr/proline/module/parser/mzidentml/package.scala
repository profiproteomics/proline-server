package fr.proline.module.parser

import scala.collection.JavaConversions._

import fr.profi.cv._
import fr.profi.obo.PsiMs
import fr.profi.obo.PsiMsTermId
import fr.profi.util.primitives.StringMap

import uk.ac.ebi.jmzidml.model.mzidml.AbstractParam
import uk.ac.ebi.jmzidml.model.mzidml.CvParam
import uk.ac.ebi.jmzidml.model.mzidml.DatabaseFilters
import uk.ac.ebi.jmzidml.model.mzidml.Filter
import uk.ac.ebi.jmzidml.model.mzidml.UserParam

package object mzidentml {
  
  object FilterParamType extends Enumeration {
    val EXCLUDE = Value("EXCLUDE")
    val INCLUDE = Value("INCLUDE")    
  }
  
  implicit class RichDatabaseFilters( val dbFilters: DatabaseFilters ) extends AnyVal {
    
    def findFilter( termId: PsiMsTermId.Value ): Option[Filter] = {
      require( dbFilters != null, "dbFilters is null" )
      
      dbFilters.getFilter().find(_.getFilterType.getCvParam.getAccession == termId.toString)
    }
    
    def findFilterParamValue(
      filterId: PsiMs.Value,
      paramId: PsiMs.Value,
      paramType: FilterParamType.Value = FilterParamType.INCLUDE
    ): Option[String] = {
      this.findFilter(filterId).flatMap( _.findFilterCvParam(paramId, paramType).map(_.getValue) )
    }
  }
  
  implicit class RichFilter( val filter: Filter ) extends AnyVal { 
    def findFilterCvParam( term: PsiMs.Value, paramType: FilterParamType.Value = FilterParamType.INCLUDE ): Option[CvParam] = {
      
      val paramList = paramType match {
        case FilterParamType.EXCLUDE => filter.getExclude()
        case FilterParamType.INCLUDE => filter.getInclude()
      }
      if (paramList == null) return None
      
      findCvParam(paramList.getCvParam(), term)
    }
  }
  
  def findParam( params: java.util.List[AbstractParam], paramName: String ): Option[AbstractParam] = {
    params.find(_.getName() == paramName)
  }

  def findCvParam( cvParams: java.util.List[CvParam], term: PsiMs.Value ): Option[CvParam] = {
    cvParams.find(_.getAccession == term.toString)
  }
  
  def filterCvParams( cvParams: java.util.List[CvParam], termSet: Set[PsiMs.Value] ): List[CvParam] = {
    val termsAsStrSet = termSet.map(_.toString())
    cvParams.filter( cvParam => termsAsStrSet.contains(cvParam.getAccession) ).toList
  }
  
  def findCvParamValue( cvParams: java.util.List[CvParam], termId: PsiMs.Value ): Option[String] = {
    findCvParam(cvParams,termId).map(_.getValue())
  }
  
  def findUserParam( userParams: java.util.List[UserParam], name: String ): Option[UserParam] = {
    userParams.find(_.getName == name)
  }
  
  def findUserParamValue( userParams: java.util.List[UserParam], name: String ): Option[String] = {
    findUserParam(userParams,name).map(_.getValue())
  }
  
  def userParamsToStringMap( userParams: java.util.List[UserParam] ): StringMap = {
    val paramMap = new StringMap()
    
    userParams.map { userParam =>
      paramMap += userParam.getName() -> userParam.getValue()
    }
    
    paramMap
  }
  
  object ScoreParamName extends Enumeration {
    val COMET_EVALUE = Value( PsiCvParam(PsiMs.CometExpectationValue).getName() )
    val MASCOT_SCORE = Value( PsiCvParam(PsiMs.MascotScore).getName() )
    val MSGF_EVALUE = Value( PsiCvParam(PsiMs.MSGFEValue).getName() )
    val OMSSA_EVALUE = Value( PsiCvParam(PsiMs.OMSSAEvalue).getName() )
    val PEPTIDE_SHAKER_SCORE = Value( PsiCvParam(PsiMs.PeptideShakerPSMScore).getName() )
    val SEQUEST_EXPECT = Value( "expect" ) // Sequest + PeptideProphet expectation value
    val XTANDEM_EVALUE = Value( PsiCvParam(PsiMs.XTandemExpect).getName() )
  }
  
  object PepXmlUserParams {
    val fixedModificationKeys = Array(
      "add_A_alanine",
      "add_B_user_amino_acid",
      "add_C_cysteine",
      "add_Cterm_peptide",
      "add_Cterm_protein",
      "add_D_aspartic_acid",
      "add_E_glutamic_acid",
      "add_F_phenylalanine",
      "add_G_glycine",
      "add_H_histidine",
      "add_I_isoleucine",
      "add_J_user_amino_acid",
      "add_K_lysine",
      "add_L_leucine",
      "add_M_methionine",
      "add_N_asparagine",
      "add_Nterm_peptide",
      "add_Nterm_protein",
      "add_O_ornithine",
      "add_P_proline",
      "add_Q_glutamine",
      "add_R_arginine",
      "add_S_serine",
      "add_T_threonine",
      "add_U_user_amino_acid",
      "add_V_valine",
      "add_W_tryptophan",
      "add_X_user_amino_acid",
      "add_Y_tyrosine",
      "add_Z_user_amino_acid"
    )
    
    val ionSeriesKeys = Array(
      "use_A_ions",
      "use_B_ions",
      "use_C_ions",
      "use_NL_ions",
      "use_X_ions",
      "use_Y_ions",
      "use_Z_ions"      
    )
  }
  
  class PepXmlUserParams( userParams: java.util.List[UserParam] ) {
    
    val paramMap = userParamsToStringMap(userParams)
    
    val activationMethod = paramMap.getString("activation_method")
    
    val usedIonSeries = PepXmlUserParams.ionSeriesKeys.map( k => k -> paramMap.getBoolean(k) ).toMap
    val useSparseMatrix = paramMap.getBoolean("use_sparse_matrix")
    
    val fixedModifications = PepXmlUserParams.fixedModificationKeys.map( k => k -> paramMap.getDouble(k) ).toMap
    val variableCTerminus = paramMap.getDouble("variable_C_terminus")
    val variableCTerminusDistance = paramMap.getInt("variable_C_terminus_distance")
    val variableNTerminus = paramMap.getDouble("variable_N_terminus")
    val variableNTerminusDistance = paramMap.getInt("variable_N_terminus_distance")
    val variableMod1 = _parseVariableMod(paramMap.getString("variable_mod1"))
    val variableMod2 = _parseVariableMod(paramMap.getString("variable_mod2"))
    val variableMod3 = _parseVariableMod(paramMap.getString("variable_mod3"))
    val variableMod4 = _parseVariableMod(paramMap.getString("variable_mod4"))
    val variableMod5 = _parseVariableMod(paramMap.getString("variable_mod5"))
    val variableMod6 = _parseVariableMod(paramMap.getString("variable_mod6"))
    
    val allowedMissedCleavage = paramMap.getInt("allowed_missed_cleavage")
    val clearMzRange = _parseDoubleRange(paramMap.getString("clear_mz_range"))
    val clipNtermMethionine = paramMap.getInt("clip_nterm_methionine")
    val databaseName = paramMap.getString("database_name")
    val decoyPrefix = paramMap.getString("decoy_prefix")
    val decoySearch = paramMap.getBoolean("decoy_search")
    val digestMassRange = _parseDoubleRange(paramMap.getString("digest_mass_range"))
    val fragmentBinOffset = paramMap.getDouble("fragment_bin_offset")
    val fragmentBinTol = paramMap.getDouble("fragment_bin_tol")
    val isotopeError = paramMap.getInt("isotope_error")
    val massTypeFragment = paramMap.getInt("mass_type_fragment")
    val massTypeParent = paramMap.getInt("mass_type_parent")
    val maxFragmentCharge = paramMap.getInt("max_fragment_charge")
    val maxPrecursorCharge = paramMap.getInt("max_precursor_charge")
    val maxVariableModsInPeptide = paramMap.getInt("max_variable_mods_in_peptide")
    val minimumIntensity = paramMap.getInt("minimum_intensity")
    val minimumPeaks = paramMap.getInt("minimum_peaks")
    val msLevel = paramMap.getInt("ms_level")
    val nucleotideReadingFrame = paramMap.getInt("nucleotide_reading_frame")
    val numEnzymeTermini = paramMap.getInt("num_enzyme_termini")
    val numOutputLines = paramMap.getInt("num_output_lines")
    val numResults = paramMap.getInt("num_results")
    val outputPepxmlfile = paramMap.getBoolean("output_pepxmlfile")
    val overrideCharge = paramMap.getInt("override_charge")
    val peptideMassTolerance = paramMap.getDouble("peptide_mass_tolerance")
    val peptideMassUnits = paramMap.getInt("peptide_mass_units")
    val precursorCharge = _parseIntRange( paramMap.getString("precursor_charge") )
    val precursorToleranceType = paramMap.getInt("precursor_tolerance_type")
    val printExpectScore = paramMap.getBoolean("print_expect_score")
    val removePrecursorPeak = paramMap.getBoolean("remove_precursor_peak")
    val removePrecursorTolerance = paramMap.getDouble("remove_precursor_tolerance")
    val sampleEnzymeNumber = paramMap.getInt("sample_enzyme_number")
    val scanRange = _parseIntRange( paramMap.getString("scan_range") )
    val searchEnzymeNumber = paramMap.getInt("search_enzyme_number")
    val showFragmentIons = paramMap.getBoolean("show_fragment_ions")
    val skipResearching = paramMap.getBoolean("skip_researching")
    val spectrumBatchSize = paramMap.getInt("spectrum_batch_size")
    val theoreticalFragmentIons = paramMap.getInt("theoretical_fragment_ions")
    val targetDecoyApproach = paramMap.getBoolean("TargetDecoyApproach")
    val minIsotopeError = paramMap.getInt("MinIsotopeError")
    val maxIsotopeError = paramMap.getInt("MaxIsotopeError")
    val fragmentMethod = paramMap.getString("FragmentMethod")
    val instrument = paramMap.getString("Instrument")
    val protocol = paramMap.getString("Protocol")
    val numTolerableTermini = paramMap.getInt("NumTolerableTermini")
    val numMatchesPerSpec = paramMap.getInt("NumMatchesPerSpec")
    val minPepLength = paramMap.getInt("MinPepLength")
    val maxPepLength = paramMap.getInt("MaxPepLength")
    val minCharge = paramMap.getInt("MinCharge")
    val maxCharge = paramMap.getInt("MaxCharge")
    
    private def _parseDoubleRange(rangeAsStr: String): (Double,Double) = {
      val strArray = rangeAsStr.split(" ")
      (strArray.head.toDouble,strArray.last.toDouble)
    }
    
    private def _parseIntRange(rangeAsStr: String): (Int,Int) = {
      val strArray = rangeAsStr.split(" ")
      (strArray.head.toInt,strArray.last.toInt)
    }
    
    case class VariableMod( mass: Double, residue: String, a: Int, b: Int )
    private def _parseVariableMod(varModAsStr: String): VariableMod = {
      val strArray = varModAsStr.split(" ")
      VariableMod(strArray(0).toDouble,strArray(1), strArray(2).toInt, strArray(3).toInt )
    }
      
    /*
     <AdditionalSearchParams>
        <cvParam cvRef="MS" accession="MS:1001211" name="parent mass type mono" value=""/>
        <cvParam cvRef="MS" accession="MS:1001256" name="fragment mass type mono" value=""/>
        <userParam name="activation_method" value="ALL"/>
        <userParam name="add_A_alanine" value="0.000000"/>
        <userParam name="add_B_user_amino_acid" value="0.000000"/>
        <userParam name="add_C_cysteine" value="57.021464"/>
        <userParam name="add_Cterm_peptide" value="0.000000"/>
        <userParam name="add_Cterm_protein" value="0.000000"/>
        <userParam name="add_D_aspartic_acid" value="0.000000"/>
        <userParam name="add_E_glutamic_acid" value="0.000000"/>
        <userParam name="add_F_phenylalanine" value="0.000000"/>
        <userParam name="add_G_glycine" value="0.000000"/>
        <userParam name="add_H_histidine" value="0.000000"/>
        <userParam name="add_I_isoleucine" value="0.000000"/>
        <userParam name="add_J_user_amino_acid" value="0.000000"/>
        <userParam name="add_K_lysine" value="0.000000"/>
        <userParam name="add_L_leucine" value="0.000000"/>
        <userParam name="add_M_methionine" value="0.000000"/>
        <userParam name="add_N_asparagine" value="0.000000"/>
        <userParam name="add_Nterm_peptide" value="0.000000"/>
        <userParam name="add_Nterm_protein" value="0.000000"/>
        <userParam name="add_O_ornithine" value="0.000000"/>
        <userParam name="add_P_proline" value="0.000000"/>
        <userParam name="add_Q_glutamine" value="0.000000"/>
        <userParam name="add_R_arginine" value="0.000000"/>
        <userParam name="add_S_serine" value="0.000000"/>
        <userParam name="add_T_threonine" value="0.000000"/>
        <userParam name="add_U_user_amino_acid" value="0.000000"/>
        <userParam name="add_V_valine" value="0.000000"/>
        <userParam name="add_W_tryptophan" value="0.000000"/>
        <userParam name="add_X_user_amino_acid" value="0.000000"/>
        <userParam name="add_Y_tyrosine" value="0.000000"/>
        <userParam name="add_Z_user_amino_acid" value="0.000000"/>
        <userParam name="allowed_missed_cleavage" value="2"/>
        <userParam name="clear_mz_range" value="0.000000 0.000000"/>
        <userParam name="clip_nterm_methionine" value="0"/>
        <userParam name="database_name" value="/dbase/Misc/iPRG2014/iPRG2015.TargDecoy.fasta"/>
        <userParam name="decoy_prefix" value="DECOY_"/>
        <userParam name="decoy_search" value="0"/>
        <userParam name="digest_mass_range" value="600.000000 5000.000000"/>
        <userParam name="fragment_bin_offset" value="0.000000"/>
        <userParam name="fragment_bin_tol" value="0.020000"/>
        <userParam name="isotope_error" value="1"/>
        <userParam name="mass_type_fragment" value="1"/>
        <userParam name="mass_type_parent" value="1"/>
        <userParam name="max_fragment_charge" value="3"/>
        <userParam name="max_precursor_charge" value="6"/>
        <userParam name="max_variable_mods_in_peptide" value="5"/>
        <userParam name="minimum_intensity" value="0"/>
        <userParam name="minimum_peaks" value="10"/>
        <userParam name="ms_level" value="2"/>
        <userParam name="nucleotide_reading_frame" value="0"/>
        <userParam name="num_enzyme_termini" value="1"/>
        <userParam name="num_output_lines" value="5"/>
        <userParam name="num_results" value="50"/>
        <userParam name="num_threads" value="0"/>
        <userParam name="output_outfiles" value="0"/>
        <userParam name="output_pepxmlfile" value="1"/>
        <userParam name="output_pinxmlfile" value="0"/>
        <userParam name="output_sqtfile" value="0"/>
        <userParam name="output_sqtstream" value="0"/>
        <userParam name="output_suffix"/>
        <userParam name="output_txtfile" value="0"/>
        <userParam name="override_charge" value="0"/>
        <userParam name="peptide_mass_tolerance" value="100.000000"/>
        <userParam name="peptide_mass_units" value="2"/>
        <userParam name="precursor_charge" value="0 0"/>
        <userParam name="precursor_tolerance_type" value="1"/>
        <userParam name="print_expect_score" value="1"/>
        <userParam name="remove_precursor_peak" value="0"/>
        <userParam name="remove_precursor_tolerance" value="1.500000"/>
        <userParam name="sample_enzyme_number" value="1"/>
        <userParam name="scan_range" value="0 0"/>
        <userParam name="search_enzyme_number" value="1"/>
        <userParam name="show_fragment_ions" value="0"/>
        <userParam name="skip_researching" value="1"/>
        <userParam name="spectrum_batch_size" value="0"/>
        <userParam name="theoretical_fragment_ions" value="0"/>
        <userParam name="use_A_ions" value="0"/>
        <userParam name="use_B_ions" value="1"/>
        <userParam name="use_C_ions" value="0"/>
        <userParam name="use_NL_ions" value="1"/>
        <userParam name="use_X_ions" value="0"/>
        <userParam name="use_Y_ions" value="1"/>
        <userParam name="use_Z_ions" value="0"/>
        <userParam name="use_sparse_matrix" value="0"/>
        <userParam name="variable_C_terminus" value="0.000000"/>
        <userParam name="variable_C_terminus_distance" value="-1"/>
        <userParam name="variable_N_terminus" value="42.010565"/>
        <userParam name="variable_N_terminus_distance" value="0"/>
        <userParam name="variable_mod1" value="15.994900 M 0 3"/>
        <userParam name="variable_mod2" value="0.000000 X 0 3"/>
        <userParam name="variable_mod3" value="0.000000 X 0 3"/>
        <userParam name="variable_mod4" value="0.000000 X 0 3"/>
        <userParam name="variable_mod5" value="0.000000 X 0 3"/>
        <userParam name="variable_mod6" value="0.000000 X 0 3"/>
        <userParam name="TargetDecoyApproach" value="false"/>
        <userParam name="MinIsotopeError" value="-1"/>
        <userParam name="MaxIsotopeError" value="2"/>
        <userParam name="FragmentMethod" value="HCD"/>
        <userParam name="Instrument" value="QExactive"/>
        <userParam name="Protocol" value="NoProtocol"/>
        <userParam name="NumTolerableTermini" value="1"/>
        <userParam name="NumMatchesPerSpec" value="1"/>
        <userParam name="MinPepLength" value="6"/>
        <userParam name="MaxPepLength" value="40"/>
        <userParam name="MinCharge" value="2"/>
        <userParam name="MaxCharge" value="3"/>
      </AdditionalSearchParams>
     */
  }

}