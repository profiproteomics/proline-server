package fr.proline.module.exporter.msi

import java.io.File
import java.nio.file.Files

import com.typesafe.scalalogging.LazyLogging
import fr.proline.context.IExecutionContext
import fr.proline.core.dal.AbstractDatastoreTestCase
import fr.proline.core.dbunit.SmallRuns_XIC
import fr.proline.module.exporter.ViewSetExporter
import fr.proline.module.exporter.msi.template.SpectraListAsTSV
import fr.proline.module.exporter.msi.view.{BuildRSMSpectraViewSet, FormatCompatibility}
import fr.proline.module.fragmentmatch.service.SpectrumMatchesGenerator
import fr.proline.repository.DriverType
import org.apache.commons.io.FileUtils
import org.junit.Assert.assertTrue
import org.junit.Test


object SpectraListExporterTest extends AbstractDatastoreTestCase {

  override val driverType = DriverType.H2
  override val dbUnitResultFile = SmallRuns_XIC
  override val useJPA: Boolean = false

}

class SpectraListExporterTest extends LazyLogging {

  val targetRSMId = 2L
  val targetRSId = 2L

  val quantRSMId = 7L
  val quantRSId = 7L

  val executionContext = SpectraListExporterTest.executionContext
  val viewSetTemplate = SpectraListAsTSV

  @Test
  def testExportIdentRSM4PeakView() {

    generateSpectrumMatches(SpectraListExporterTest.executionContext, targetRSMId, targetRSId)
    val exportPath = Files.createTempDirectory(null)
    val exportLoc = exportPath.toFile
      val viewSet = BuildRSMSpectraViewSet(
        executionContext,
        1L,
        targetRSMId,
        viewSetName = SpectraListExporterTest.dbUnitResultFile.toString,
        viewSetTemplate = viewSetTemplate,
        mode = FormatCompatibility.PEAKVIEW)

      ViewSetExporter.exportViewSetToDirectory(viewSet, exportLoc )

      val f = new File(new File(exportLoc, SpectraListExporterTest.dbUnitResultFile.toString), "spectraList.tsv")
      assertTrue(f.exists())
      FileUtils.deleteDirectory(exportLoc)
  }

  @Test
  def testExportQuantification4PeakView() {

    generateSpectrumMatches(SpectraListExporterTest.executionContext, quantRSMId, quantRSId)
    val exportPath = Files.createTempDirectory(null)
    val exportLoc = exportPath.toFile

    val viewSet = BuildRSMSpectraViewSet(
      executionContext,
      1L,
      quantRSMId,
      viewSetName = SpectraListExporterTest.dbUnitResultFile.toString,
      viewSetTemplate = viewSetTemplate,
      mode = FormatCompatibility.PEAKVIEW)
    ViewSetExporter.exportViewSetToDirectory(viewSet, exportLoc)
    val f = new File(new File(exportLoc, SpectraListExporterTest.dbUnitResultFile.toString), "spectraList.tsv")
    assertTrue(f.exists())
    FileUtils.deleteDirectory(exportLoc)
  }

  @Test
  def testExportIdentRSM4Spectronaut() {

    generateSpectrumMatches(SpectraListExporterTest.executionContext, targetRSMId, targetRSId)
    val exportPath = Files.createTempDirectory(null)
    val exportLoc = exportPath.toFile
    val viewSet = BuildRSMSpectraViewSet(
      executionContext,
      1L,
      targetRSMId,
      viewSetName = SpectraListExporterTest.dbUnitResultFile.toString,
      viewSetTemplate = viewSetTemplate,
      mode = FormatCompatibility.SPECTRONAUT)

    ViewSetExporter.exportViewSetToDirectory(viewSet, exportLoc )

    val f = new File(new File(exportLoc, SpectraListExporterTest.dbUnitResultFile.toString), "spectraList.tsv")
    assertTrue(f.exists())
    FileUtils.deleteDirectory(exportLoc)
  }

  @Test
  def testExportQuantification4Spectronaut() {

    generateSpectrumMatches(SpectraListExporterTest.executionContext, quantRSMId, quantRSId)
    val exportPath = Files.createTempDirectory(null)
    val exportLoc = exportPath.toFile

    val viewSet = BuildRSMSpectraViewSet(
      executionContext,
      1L,
      quantRSMId,
      viewSetName = SpectraListExporterTest.dbUnitResultFile.toString,
      viewSetTemplate = viewSetTemplate,
      mode = FormatCompatibility.SPECTRONAUT)
    ViewSetExporter.exportViewSetToDirectory(viewSet, exportLoc)
    val f = new File(new File(exportLoc, SpectraListExporterTest.dbUnitResultFile.toString), "spectraList.tsv")
    assertTrue(f.exists())
    FileUtils.deleteDirectory(exportLoc)
  }

  private def generateSpectrumMatches(execContext: IExecutionContext, rsmId: Long, rsId: Long) {
    val service = new SpectrumMatchesGenerator(execContext, rsId, Some(rsmId), None,None, false)
    service.run
  }

}