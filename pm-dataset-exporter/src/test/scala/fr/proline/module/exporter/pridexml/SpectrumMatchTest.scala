package fr.proline.module.exporter.pridexml

import org.junit.Assert
import org.junit.Test

import com.typesafe.scalalogging.LazyLogging

import fr.profi.util.serialization.CustomDoubleJacksonSerializer
import fr.profi.util.serialization.ProfiJSMSerialization
import fr.proline.core.om.model.msi.FragmentMatch
import fr.proline.core.om.model.msi.SpectrumMatch
import fr.proline.core.om.model.msi.TheoreticalFragmentSeries

class SpectrumMatchTest extends LazyLogging {

  object CustomSerializer extends ProfiJSMSerialization with CustomDoubleJacksonSerializer

  @Test
  def testSpectrumMatchDeserialize() {
    val jsonStr = """{"frag_table":[{"frag_series":"y0++","masses":[0.0,434.268553324962,390.752539104962,334.210507104962,277.668475104962,249.15774323496197,0.0,0.0,0.0,0.0]},{"frag_series":"b","masses":[102.054954966812,189.086983406812,302.171047406812,415.25511140681203,472.27657514681204,573.324253646812,686.4083176468121,743.429781386812,840.482545266812,0.0]},
      {"frag_series":"y*++","masses":[0.0,434.760561116307,391.244546896307,334.702514896307,278.160482896307,249.64975102630697,199.12591177630696,142.58387977630696,114.07314790630699,65.54676596630696]},
      {"frag_series":"b++","masses":[51.531115716812,95.047129936812,151.58916193681202,208.13119393681202,236.64192580681203,287.165765056812,343.707797056812,372.218528926812,420.744910866812,0.0]},
      {"frag_series":"y","masses":[0.0,885.540394866812,798.508366426812,685.424302426812,572.340238426812,515.3187746868119,414.2710961868119,301.1870321868119,244.165568446812,147.11280456681195]},
      {"frag_series":"y*","masses":[0.0,868.513845765802,781.481817325802,668.397753325802,555.313689325802,498.29222558580193,397.2445470858019,284.1604830858019,227.13901934580198,130.08625546580194]},
      {"frag_series":"y0","masses":[0.0,867.529830183112,780.4978017431121,667.413737743112,554.329673743112,497.3082100031119,0.0,0.0,0.0,0.0]},
      {"frag_series":"b0++","masses":[42.525833374962005,86.041847594962,142.583879594962,199.12591159496202,227.63664346496202,278.16048271496203,334.70251471496204,363.213246584962,411.73962852496203,0.0]},
      {"frag_series":"y++","masses":[0.0,443.273835666812,399.757821446812,343.215789446812,286.673757446812,258.16302557681195,207.63918632681197,151.09715432681196,122.58642245681199,74.06004051681197]},
      {"frag_series":"b0","masses":[84.04439028311201,171.076418723112,284.160482723112,397.244546723112,454.266010463112,555.3136889631121,668.3977529631121,725.419216703112,822.4719805831121,0.0]}],
      "frag_matches":[{"label":"b(2)","moz":189.10501,"calculated_moz":189.086983406812,"intensity":270400.0},
      {"label":"b(3)","moz":302.24473,"calculated_moz":302.171047406812,"intensity":518400.0},
      {"label":"b(4)","moz":415.37808,"calculated_moz":415.25511140681203,"intensity":812100.0},
      {"label":"b(5)","moz":472.35155,"calculated_moz":472.27657514681204,"intensity":121100.0},
      {"label":"b(8)","moz":743.34921,"calculated_moz":743.429781386812,"intensity":142600.0},
      {"label":"y(8)","moz":798.58163,"calculated_moz":798.508366426812,"intensity":1487000.0},
      {"label":"y(7)","moz":685.53052,"calculated_moz":685.424302426812,"intensity":2386000.0},
      {"label":"y(6)","moz":572.39044,"calculated_moz":572.340238426812,"intensity":2785000.0},
      {"label":"y(5)","moz":515.3713,"calculated_moz":515.3187746868119,"intensity":270300.0},
      {"label":"y(3)","moz":301.30834,"calculated_moz":301.1870321868119,"intensity":193200.0},
      {"label":"y(2)","moz":244.25894,"calculated_moz":244.165568446812,"intensity":89730.0},
      {"label":"y*(7)","moz":668.44017,"calculated_moz":668.397753325802,"intensity":226200.0},
      {"label":"y*(3)","moz":284.26385,"calculated_moz":284.1604830858019,"intensity":884500.0},
      {"label":"y(8)++","moz":399.98601,"calculated_moz":399.757821446812,"intensity":793300.0},
      {"label":"b0(3)","moz":284.26385,"calculated_moz":284.160482723112,"intensity":884500.0},
      {"label":"b0(7)","moz":668.44017,"calculated_moz":668.3977529631121,"intensity":226200.0}]}"""

    val spectrumMatch = CustomSerializer.deserialize[SpectrumMatch](jsonStr)
    Assert.assertNotNull(spectrumMatch)

  }

  @Test
  def testSpectrumMatchSerialize() {
    val fm = new FragmentMatch("b0(7)", None, 668.6, 668.0, 9999, None)
    val spectrumMatch = new SpectrumMatch()
    spectrumMatch.fragMatches = Array(fm)
    val tfs = new TheoreticalFragmentSeries("y0++", Array(12.0, 13.0562, 1257.235689, 9999.00000001))
    spectrumMatch.fragTable = Array(tfs)
    logger.info(CustomSerializer.serialize(spectrumMatch))

  }

}