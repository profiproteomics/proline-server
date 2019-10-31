package fr.proline.module.exporter.pridexml

object PrideSchemaConstants {

	final def PRIDE_SCHEMA_VERSION = "2.1"
    
  final def CVPARAM_NODE="cvParam"
  final def CVLABEL_ATT="cvLabel"
  final def CV_ACC_ATT="accession"
  final def CV_NAME_ATT="name"
  final def CV_VALUE_ATT="value"

  final def PRIDE_CV_NAME="PRIDE"
  final def PRIDE_CV_FULLNAME="PRIDE Controlled Vocabulary"
  final def PRIDE_CV_VERSION="1.101"
  final def PRIDE_CV_ADDRESS="http://ebi-pride.googlecode.com/svn/trunk/pride-core/schema/pride_cv.obo"

  
  final def PSI_CV_FULLNAME="The PSI Ontology"
  final def PSI_CV_VERSION="1.0.0"
  final def PSI_CV_ADDRESS="http://psidev.sourceforge.net/ontology/"
  final def PSI_CV_NAME="PSI"

  final def MS_CV_NAME="MS"
  final def MS_CV_FULLNAME="PSI Mass Spectrometry Ontology"
  final def MS_CV_VERSION="1.2"
  final def MS_CV_ADDRESS="http://psidev.cvs.sourceforge.net/viewvc/*checkout*/psidev/psi/psi-ms/mzML/controlledVocabulary/psi-ms.obo"

	final def SCHEMA_VERSION_ATTR = "version"
  final def EXP_COLL_NODE="ExperimentCollection"
  final def EXP_NODE="Experiment"
  final def EXP_TITLE_NODE="Title"
  final def EXP_SHORT_LABEL_NODE="ShortLabel" 
  final def PROTOCOL_NODE="Protocol"
  final def PROTOCOL_NAME_NODE="ProtocolName"
  
  final def ADDITIONAL_NODE="additional"
    
//    
//    // Static Value for nodes    
//    final def UNIMOD_DB_NAME="UNIMOD"
//    final def PSIMOD_DB_NAME="MOD"
  final def PROTOCOL_STD_NAME="Protocol Name : To replace ! "
    
    // CV entries
    
    
  final def MS_CV_NANOELECTROSPRAY_ACC="MS:1000398"
  final def MS_CV_NANOELECTROSPRAY_NAME="nanoelectrospray"

  final def PRIDE_CV_SOFT_ACC="PRIDE:0000175"
  final def PRIDE_CV_SOFT_NAME="XML generation software"
      
  final def PRIDE_CV_FRAGMENTMASS_TOL_ACC="PRIDE:0000161"
  final def PRIDE_CV_FRAGMENTMASS_TOL_NAME="Fragment mass tolerance setting"
      
  final def MS_CV_ELECTRON_MULTIPLIER_ACC="MS:1000253"
  final def MS_CV_ELECTRON_MULTIPLIER_NAME="electron multiplier"

  final def PRIDE_CV_PEPMASS_TOL_ACC="PRIDE:0000078"
  final def PRIDE_CV_PEPMASS_TOL_NAME="Peptide mass tolerance setting"

  final def PRIDE_CV_ALLOWED_MC_ACC="PRIDE:0000162"
  final def PRIDE_CV_ALLOWED_MC_NAME="Allowed missed cleavages"

  final def PRIDE_CV_PROJECT_ACC="PRIDE:0000097"
  final def PRIDE_CV_PROJECT_NAME="Project"
  
  final def MS_CV_MASS_2_CHARGE_ACC="MS:1000744"
  final def MS_CV_MASS_2_CHARGE_NAME="Mass-to-charge ratio"
  
  final def MS_CV_CHARGE_STATE_ACC="MS:1000041"
  final def MS_CV_CHARGE_STATE_NAME="Charge state"
  
  final def MS_CV_INTENSITY_ACC="MS:1000042"
  final def MS_CV_INTENSITY_NAME="Peak intensity"
      
  final def PRIDE_CV_MASCOT_SCORE_ACC="PRIDE:0000069"
  final def PRIDE_CV_MASCOT_SCORE_NAME="Mascot Score"
  
  final def PRIDE_CV_RESIDUE_BEFORE_ACC="PRIDE:0000065"
  final def PRIDE_CV_RESIDUE_BEFORE_NAME="Upstream flanking sequence"
  
  final def PRIDE_CV_RESIDUE_AFTER_ACC="PRIDE:0000066"
  final def PRIDE_CV_RESIDUE_AFTER_NAME="Downstream flanking sequence"
    
    //Fragment ION Annotation node    
	final def PRIDE_CV_PROD_ION_CHARGE_ACC = "PRIDE:0000204"
	final def PRIDE_CV_PROD_ION_CHARGE_NAME = "product ion charge"
	
	final def PRIDE_CV_PROD_ION_I_ACC = "PRIDE:0000189"
	final def PRIDE_CV_PROD_ION_I_NAME = "product ion intensity"
	
	final def PRIDE_CV_PROD_ION_MZ_ACC = "PRIDE:0000188"
	final def PRIDE_CV_PROD_ION_MZ_NAME = "product ion m/z"
	
	final def PRIDE_CV_PROD_ION_MASS_ERR_ACC = "PRIDE:0000190"
	final def PRIDE_CV_PROD_ION_MASS_ERR_NAME = "product ion mass error"
    
	//Generic Ion TYPE
	final def PRIDE_CV_ION_TYPE_ACC = "PRIDE:0000192"
	final def PRIDE_CV_ION_TYPE_NAME = "product ion type"
	
	final def PRIDE_CV_PROT_DESCRIPTION_ACC = "PRIDE:0000063"
	final def PRIDE_CV_PROT_DESCRIPTION_NAME = "Protein description line"

	final def PRIDE_CV_PROT_IDENTIFIED_PEP_FRAG_ACC = "PRIDE:0000113"
	final def PRIDE_CV_PROT_IDENTIFIED_PEP_FRAG_NAME = "Identified by peptide fragmentation"

	final def PRIDE_CV_PROT_SAMSET_ACC = "PRIDE:0000098"
	final def PRIDE_CV_PROT_SAMSET_NAME = "Indistinguishable alternative protein accession"

	//ion type y
	final def PRIDE_CV_Y_ION_ACC = "PRIDE:0000193"
	final def PRIDE_CV_Y_ION_NAME = "y ion"	
	final def PRIDE_CV_YH2O_ION_ACC = "PRIDE:0000197"
	final def PRIDE_CV_YH2O_ION_NAME = "y ion -H2O"	
	final def PRIDE_CV_YNH3_ION_ACC = "PRIDE:0000198"
	final def PRIDE_CV_YNH3_ION_NAME = "y ion -NH3"
	
	//ion type b
	final def PRIDE_CV_B_ION_ACC = "PRIDE:0000194"
	final def PRIDE_CV_B_ION_NAME = "b ion"	
	final def PRIDE_CV_BH2O_ION_ACC = "PRIDE:0000196"
	final def PRIDE_CV_BH2O_ION_NAME = "b ion -H2O"	
	final def PRIDE_CV_BNH3_ION_ACC = "PRIDE:0000195"
	final def PRIDE_CV_BNH3_ION_NAME = "b ion -NH3"
	
	//ion type a
	final def PRIDE_CV_A_ION_ACC = "PRIDE:0000233"
	final def PRIDE_CV_A_ION_NAME = "a ion"	
	final def PRIDE_CV_AH20_ION_ACC = "PRIDE:0000234"
	final def PRIDE_CV_AH20_ION_NAME = "a ion -H2O"	
	final def PRIDE_CV_ANH3_ION_ACC = "PRIDE:0000235"
	final def PRIDE_CV_ANH3_ION_NAME = "a ion -NH3"	

	//ion type c
	final def PRIDE_CV_C_ION_ACC = "PRIDE:0000236"
	final def PRIDE_CV_C_ION_NAME = "c ion"	
	final def PRIDE_CV_CH20_ION_ACC = "PRIDE:0000237"
	final def PRIDE_CV_CH20_ION_NAME = "c ion -H2O"	
	final def PRIDE_CV_CNH3_ION_ACC = "PRIDE:0000238"
	final def PRIDE_CV_CNH3_ION_NAME = "c ion -NH3"	
	
	//ion type x
	final def PRIDE_CV_X_ION_ACC = "PRIDE:0000227"
	final def PRIDE_CV_X_ION_NAME = "x ion"	
	final def PRIDE_CV_XH20_ION_ACC = "PRIDE:0000228"
	final def PRIDE_CV_XH20_ION_NAME = "x ion -H2O"	
	final def PRIDE_CV_XNH3_ION_ACC = "PRIDE:0000229"
	final def PRIDE_CV_XNH3_ION_NAME = "x ion -NH3"
	
	//ion type z
	final def PRIDE_CV_Z_ION_ACC = "PRIDE:0000230"
	final def PRIDE_CV_Z_ION_NAME = "z ion"
	final def PRIDE_CV_ZH20_ION_ACC = "PRIDE:0000231"
	final def PRIDE_CV_ZH20_ION_NAME = "z ion -H2O"	
	final def PRIDE_CV_ZNH3_ION_ACC = "PRIDE:0000232"
	final def PRIDE_CV_ZNH3_ION_NAME = "z ion -NH3"

}