package fr.proline.module.exporter.pridexml

import uk.ac.ebi.pride.jaxb.model.DataProcessing
import fr.proline.core.om.model.msi.MSISearch
import uk.ac.ebi.pride.jaxb.model.Software
import uk.ac.ebi.pride.jaxb.model.Param
import uk.ac.ebi.pride.jaxb.model.Instrument
import org.h2.command.ddl.Analyze
import uk.ac.ebi.pride.jaxb.model.AnalyzerList
import uk.ac.ebi.pride.jaxb.model.Contact

trait PrideMetadataBuilder {

  def getDataProcessing(): DataProcessing
  def getInstrument(): Instrument
  def getContact(): Contact
  
}


object PrideMetadataBuilder {
  
  def apply(search: MSISearch): PrideMetadataBuilder = SingleSearchBuilder(search)
  
}

case class SingleSearchBuilder(val msiSearch:MSISearch) extends PrideMetadataBuilder {

  def getDataProcessing(): DataProcessing = {
	  val dataProcessing = new DataProcessing()
	  val software = new Software()
	  software.setName(msiSearch.searchSettings.softwareName)
	  software.setVersion(msiSearch.searchSettings.softwareVersion)
	  dataProcessing.setSoftware(software)
	  val method = new Param();
	  var value = msiSearch.searchSettings.msmsSearchSettings.get.ms2ErrorTol.toString+' '+msiSearch.searchSettings.msmsSearchSettings.get.ms2ErrorTolUnit
	  method.getCvParam().add(CvParam("PRIDE:0000161", "Fragment mass tolerance setting", value))
	  value = msiSearch.searchSettings.ms1ErrorTol.toString+' '+msiSearch.searchSettings.ms1ErrorTolUnit
	  method.getCvParam().add(CvParam("PRIDE:0000078", "Peptide mass tolerance setting", value))
	  value = msiSearch.searchSettings.maxMissedCleavages.toString
	  method.getCvParam().add(CvParam("PRIDE:0000162", "Allowed missed cleavages", value))
	  dataProcessing.setProcessingMethod(method)
	  dataProcessing
  }
  
  def getInstrument(): Instrument = {
     val instrument = new Instrument()
     instrument.setInstrumentName(msiSearch.searchSettings.instrumentConfig.name)
     val source = new Param()
     source.getCvParam().add(CvParam("MS:1000398", "nanoelectrospray", "MS", ""))
     instrument.setSource(source)
     val analyzerList = new AnalyzerList()
     var analyzer = new Param()
     analyzer.getCvParam().add(CvParam(value="MS, resolution 60'000", name="orbitrap", accession="MS:1000484", cvLabel="MS"))
     analyzerList.getAnalyzer().add(analyzer)
     analyzer = new Param()
     analyzer.getCvParam().add(CvParam(value="MS/MS, Top20", name="linear ion trap", accession="MS:1000291", cvLabel="MS"))
     analyzerList.getAnalyzer().add(analyzer)
     analyzerList.setCount(analyzerList.getAnalyzer().size())
     instrument.setAnalyzerList(analyzerList)
     val detector = new Param()
     detector.getCvParam.add(CvParam(value="", name="electron multiplier", accession="MS:1000253", cvLabel="MS"))
     instrument.setDetector(detector)
     instrument
  }
  
  def getContact(): Contact = {
    val contact = new Contact()
    contact.setName(msiSearch.userName)
    contact.setContactInfo(msiSearch.userEmail)
    contact
  }
  
}