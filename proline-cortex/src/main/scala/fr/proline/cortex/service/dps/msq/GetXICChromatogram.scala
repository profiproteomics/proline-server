package fr.proline.cortex.service.dps.msq

import java.util

import com.almworks.sqlite4java.SQLiteException
import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging
import fr.profi.mzdb.XicMethod
import fr.profi.mzdb.io.reader.cache.MzDbEntityCache
import fr.profi.mzdb.model.{DataPoint, Peak}
import fr.proline.context.DatabaseConnectionContext
import fr.proline.core.orm.uds.RawFile
import fr.proline.cortex.api.service.dps.msq.IGetXICChromatogramService
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.cortex.util.fs.MountPointRegistry
import fr.proline.jms.service.api.AbstractRemoteProcessingService
import fr.proline.jms.service.api.ISingleThreadedService
import fr.proline.cortex.service.SingleThreadIdentifierType
import fr.profi.util.serialization.ProfiJson
import javax.persistence.EntityManager

import scala.collection.mutable.ArrayBuffer

/**
 * Define JMS Service to retrieve a XIC Chromatogram
 *
 * Input params:
 * raw_file_identifier: The identifier of the raw file, defined as its name without the extension.
 * mz: m/z value for the XIC to retrieve.
 * ppm: tolerance for the m/z value
 *
 * Output params:
 * String : Chromatogram as Json
 */
class GetXICChromatogram extends AbstractRemoteProcessingService with IGetXICChromatogramService with ISingleThreadedService with LazyLogging {

  /* JMS Service identification */
  val singleThreadIdent: String = SingleThreadIdentifierType.SHORT_SERVICES_SINGLETHREAD_IDENT.toString

  def doProcess(paramsRetriever: NamedParamsRetriever): Any = {

    require(paramsRetriever != null, "no parameter specified")
    require(paramsRetriever.hasParam(PROCESS_METHOD.RAW_FILE_IDENTIFIER_PARAM), "raw_file_identifier parameter not specified")
    require(paramsRetriever.hasParam(PROCESS_METHOD.MZ_PARAM), "mz parameter not specified")
    require(paramsRetriever.hasParam(PROCESS_METHOD.PPM_PARAM), "ppm parameter not specified")


    val rawFileIdentifierList = paramsRetriever.getList(PROCESS_METHOD.RAW_FILE_IDENTIFIER_PARAM).toArray
    val mzList = paramsRetriever.getList(PROCESS_METHOD.MZ_PARAM).toArray
    val mzTolPPM = paramsRetriever.getDouble(PROCESS_METHOD.PPM_PARAM);


    var chromatograms = ArrayBuffer[ArrayBuffer[fr.proline.core.orm.lcms.Peak]]();


    // Retrieve UdsDb context and entity manager
    val udsDbCtx = new DatabaseConnectionContext(DbConnectionHelper.getDataStoreConnectorFactory().getUdsDbConnector)

    try {
      val udsEM = udsDbCtx.getEntityManager

      // Call
      for ( i <- 0 to (rawFileIdentifierList.length - 1)) {
        val rawFileIdentifier = (rawFileIdentifierList(i)).asInstanceOf[String]
        val mz = mzList(i).asInstanceOf[Double]

        var peakList = extractXIC(udsEM, rawFileIdentifier, mz, mzTolPPM)
        chromatograms.append(peakList);
      }

    } finally {
      DbConnectionHelper.tryToCloseDbContext(udsDbCtx)
    }

    ProfiJson.serialize(chromatograms);

  }

  private def extractXIC(udsEM : EntityManager, rawFileIdentifier : String, mz: Double, mzTolPPM: Double):  ArrayBuffer[fr.proline.core.orm.lcms.Peak] = {

    // Search for this raw file identifier in the UDSdb
    val rawFile = udsEM.find(classOf[RawFile], rawFileIdentifier)

    // Manage raw file not found
    if (rawFile == null) {
      logger.info(s"The raw file '$rawFileIdentifier' has not been found")
    } else {

      val mzDbDirectoryLocalPathname = MountPointRegistry.replacePossibleLabel (rawFile.getMzDbFileDirectory ()).localPathname;
      val mzDbFileLocal = new java.io.File (mzDbDirectoryLocalPathname, rawFile.getMzDbFileName());
      if (mzDbFileLocal.exists () ) {
        // we have found the mzDbFile

        // calculate min m/z and max m/z according to tolerance
        val minMz = mz - mz * mzTolPPM / 1e6f;
        val maxMz = mz + mz * mzTolPPM / 1e6f;

        // we extract the XIC Chromatogram
        var peakList = ArrayBuffer[fr.proline.core.orm.lcms.Peak]();
        retrievePeaks (peakList, mzDbFileLocal, mz, minMz, maxMz)
        return peakList;
      } else {
        logger.info (s"The mzdb file '" + mzDbFileLocal.getAbsolutePath() + "' has not been found")
      }
    }

    return null
  }



    /**
   *
   * extract the XIC Chromatogram from the mzdbFile between minMz and maxMz
   */
  private def retrievePeaks(peakListReturned : ArrayBuffer[fr.proline.core.orm.lcms.Peak], mzDbFile: java.io.File, moz : Double, minMz: Double, maxMz: Double):  ArrayBuffer[fr.proline.core.orm.lcms.Peak] = {

    var peaks = null : Array[Peak];

    val mzDbReader = new fr.profi.mzdb.MzDbReader(mzDbFile, true);
    logger.info(s"getMsXicInMzRange '$minMz' '$maxMz' starts")
    peaks = mzDbReader.getMsXicInMzRange(minMz, maxMz, XicMethod.MAX);


    if ((peaks != null) && (peaks.length > 0)) {


      try {
        var previousSpectrumId: Int = peaks(0).getLcContext.getSpectrumId.toInt
        for (peak <- peaks) {
          val spectrumId: Int = peak.getLcContext.getSpectrumId.toInt
          if (previousSpectrumId != getPreviousSpectrumId(mzDbReader, spectrumId)) { // there is a gap between peaks, add 0 values after the previous peak and before this one

            val peak1 : fr.proline.core.orm.lcms.Peak = new fr.proline.core.orm.lcms.Peak(-1L, moz, (mzDbReader.getSpectrumHeaderById.get(getNextSpectrumId(mzDbReader, previousSpectrumId).toLong).getElutionTime ).toFloat, 0.0F);
            peakListReturned.append(peak1);
            if (getPreviousSpectrumId(mzDbReader, spectrumId) > getNextSpectrumId(mzDbReader, previousSpectrumId)) {
              val peak2 : fr.proline.core.orm.lcms.Peak = new fr.proline.core.orm.lcms.Peak(-1L, moz, (mzDbReader.getSpectrumHeaderById.get(getPreviousSpectrumId(mzDbReader, spectrumId).toLong).getElutionTime ).toFloat, 0.0F);
              peakListReturned.append(peak2);
            }
          }
          val rt: Float = peak.getLcContext.getElutionTime
          val peak1 : fr.proline.core.orm.lcms.Peak = new fr.proline.core.orm.lcms.Peak(-1L, moz, rt, peak.getIntensity.toFloat);
          peakListReturned.append(peak1);

          previousSpectrumId = peak.getLcContext.getSpectrumId.toInt
        }
      } catch {
        case sle: SQLiteException =>
          logger.error("Error while reading mzdb file", sle)
      }

    }

    return peakListReturned;

  }

  /**
   *
   * In Mzdb files, the spectrum are listed in this order : 1 MS1, some MS2, 1 MS1, some MS2
   *
   * For Xic, we are interested in MS1 spectrum
   *
   */
  private def getNextSpectrumId(reader: fr.profi.mzdb.MzDbReader, spectrumIndex: Int): Int = getNextSiblingSpectrumId(reader, spectrumIndex, 1)

  private def getPreviousSpectrumId(reader: fr.profi.mzdb.MzDbReader, spectrumIndex: Int) = getNextSiblingSpectrumId(reader, spectrumIndex, -1)

  private def getNextSiblingSpectrumId(reader: fr.profi.mzdb.MzDbReader, spectrumIndex: Int, way: Int): Int = {
    try {
      val msLevel = 1 // MS1

      val header = reader.getSpectrumHeaderById.get(spectrumIndex.toLong)
      val maxSpectrum = reader.getSpectraCount
      var k = Math.max(1, Math.min(maxSpectrum, header.getSpectrumId + way))

      var break : Boolean = false
      while ( (k > 0) && (k < maxSpectrum) && (!break) ) {
        if (reader.getSpectrumHeaderById.get(k).getMsLevel == msLevel) {
          break = true
        } else {
          k += way
        }


      }
      return k.toInt
    } catch {
      case e: SQLiteException =>
        logger.error("Error while reading spectrumsCount", e)
    }
    0
  }

}