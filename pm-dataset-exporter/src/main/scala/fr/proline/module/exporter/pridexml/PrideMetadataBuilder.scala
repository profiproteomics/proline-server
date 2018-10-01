package fr.proline.module.exporter.pridexml

import fr.proline.core.om.model.msi.MSISearch
import uk.ac.ebi.pride.jaxb.model.AnalyzerList
import uk.ac.ebi.pride.jaxb.model.Contact
import uk.ac.ebi.pride.jaxb.model.DataProcessing
import uk.ac.ebi.pride.jaxb.model.Instrument
import uk.ac.ebi.pride.jaxb.model.Param
import uk.ac.ebi.pride.jaxb.model.Software

trait PrideMetadataBuilder {

  def getDataProcessing(): DataProcessing
  def getInstrument(): Instrument
  def getContact(): Contact
  
}


object PrideMetadataBuilder {
  
  def apply(search: MSISearch): PrideMetadataBuilder = SingleSearchBuilder(search)
  
}

case class SingleSearchBuilder(msiSearch:MSISearch) extends PrideMetadataBuilder {

  def getDataProcessing(): DataProcessing = {
	  val dataProcessing = new DataProcessing()
	  val software = new Software()
	  software.setName(msiSearch.searchSettings.softwareName)
	  software.setVersion(msiSearch.searchSettings.softwareVersion)
	  dataProcessing.setSoftware(software)
	  val method = new Param()
	  var value = msiSearch.searchSettings.msmsSearchSettings.get.ms2ErrorTol.toString+' '+msiSearch.searchSettings.msmsSearchSettings.get.ms2ErrorTolUnit
	  method.getCvParam.add(CvParam(PrideSchemaConstants.PRIDE_CV_FRAGMENTMASS_TOL_ACC, PrideSchemaConstants.PRIDE_CV_FRAGMENTMASS_TOL_NAME, value))
	  value = msiSearch.searchSettings.ms1ErrorTol.toString+' '+msiSearch.searchSettings.ms1ErrorTolUnit
	  method.getCvParam.add(CvParam(PrideSchemaConstants.PRIDE_CV_PEPMASS_TOL_ACC, PrideSchemaConstants.PRIDE_CV_PEPMASS_TOL_NAME, value))
	  value = msiSearch.searchSettings.maxMissedCleavages.toString
	  method.getCvParam.add(CvParam(PrideSchemaConstants.PRIDE_CV_ALLOWED_MC_ACC, PrideSchemaConstants.PRIDE_CV_ALLOWED_MC_NAME, value))
	  dataProcessing.setProcessingMethod(method)
	  dataProcessing
  }
  
  def getInstrument(): Instrument = {
     val instrument = new Instrument()
     instrument.setInstrumentName(msiSearch.searchSettings.instrumentConfig.name)
     val source = new Param()
     source.getCvParam.add(CvParam(PrideSchemaConstants.MS_CV_NANOELECTROSPRAY_ACC, PrideSchemaConstants.MS_CV_NANOELECTROSPRAY_NAME, "MS", ""))
     instrument.setSource(source)
     val analyzerList = new AnalyzerList()
     val analyser1 = msiSearch.searchSettings.instrumentConfig.ms1Analyzer
     val analyser2 = msiSearch.searchSettings.instrumentConfig.msnAnalyzer
     analyzerList.getAnalyzer.add(getAnalyzer(analyser1))
     analyzerList.getAnalyzer.add(getAnalyzer(analyser2))
//     var analyzer = new Param()
//     analyzer.getCvParam().add(CvParam(value="MS, resolution 60'000", name="orbitrap", accession="MS:1000484", cvLabel="MS"))
//     analyzerList.getAnalyzer().add(analyzer)
//     analyzer = new Param()
//     analyzer.getCvParam().add(CvParam(value="MS/MS, Top20", name="linear ion trap", accession="MS:1000291", cvLabel="MS"))
//     analyzerList.getAnalyzer().add(analyzer)
     analyzerList.setCount(analyzerList.getAnalyzer.size())
     instrument.setAnalyzerList(analyzerList)
     val detector = new Param()
     //TODO get information from DBs !!
     detector.getCvParam.add(CvParam(value="", accession=PrideSchemaConstants.MS_CV_ELECTRON_MULTIPLIER_ACC, name=PrideSchemaConstants.MS_CV_ELECTRON_MULTIPLIER_NAME, cvLabel="MS"))
     instrument.setDetector(detector)
     instrument
  }
  
  private def getAnalyzer( instrumAnalyzer : String) : Param ={
   var analyzer = new Param()
    instrumAnalyzer match {
      case "QUAD" => { analyzer.getCvParam.add(CvParam(value="QUAD", name="mass analyzer type", accession="MS:1000443", cvLabel="MS")) }
      case  "TOF" => { analyzer.getCvParam.add(CvParam(value="TOF", name="mass analyzer type", accession="MS:1000443", cvLabel="MS")) }
      case  "TRAP" => { analyzer.getCvParam.add(CvParam(value="TRAP", name="mass analyzer type", accession="MS:1000443", cvLabel="MS")) }
      case  "FTICR" => { analyzer.getCvParam.add(CvParam(value="", name="fourier transform ion cyclotron resonance mass spectrometer", accession="MS:1000079", cvLabel="MS")) }
      case  "4SECTOR" => { analyzer.getCvParam.add(CvParam(value="", name="mass analyzer type", accession="MS:1000443", cvLabel="MS")) }
      case  "QIT" => { analyzer.getCvParam.add(CvParam(value="", name="quadrupole ion trap", accession="MS:1000082", cvLabel="MS")) }
      case   "ISD" => { analyzer.getCvParam.add(CvParam(value="ISD", name="mass analyzer type", accession="MS:1000443", cvLabel="MS")) }
      case   "FTMS" => { analyzer.getCvParam.add(CvParam(value="FTMS", name="mass analyzer type", accession="MS:1000443", cvLabel="MS")) }
     case _  => { }
    }
    analyzer
  } 
  
  def getContact(): Contact = {
    val contact = new Contact()
    contact.setName(msiSearch.userName)
    contact.setContactInfo(msiSearch.userEmail)
    contact
  }
  
}