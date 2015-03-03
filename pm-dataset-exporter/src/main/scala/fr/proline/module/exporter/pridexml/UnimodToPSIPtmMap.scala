package fr.proline.module.exporter.pridexml

import scala.collection.JavaConversions._
import uk.ac.ebi.pridemod.ModReader
import uk.ac.ebi.pridemod.model.PRIDEModPTM
import uk.ac.ebi.pridemod.io.pridemod.xml.PrideModReader
import uk.ac.ebi.pridemod.io.pridemod.model.UnimodMapping

object UnimodToPSIPtmMap {

  lazy val map = {
    val prideModdUrl = classOf[ModReader].getClassLoader.getResourceAsStream("pride_mods.xml")
    val reader = new PrideModReader(prideModdUrl)
    val map = collection.mutable.Map[Int, PRIDEModPTM]()
    for (oldMod <- reader.getPrideMod().getPrideModifications().getPrideModification()) {
      val accession = oldMod.getPsiId()
      val name = oldMod.getPsiName()
      val monoMass = oldMod.getDiffMono().doubleValue()
      val specicityList = oldMod.getSpecificityList();
      val unimodReference = oldMod.getUnimodMappings().getUnimodMapping().get(0).asInstanceOf[UnimodMapping].getId().intValue()
      val ptm = new PRIDEModPTM(accession, name, name, monoMass, null, specicityList, unimodReference.toString, null)
      map += (unimodReference -> ptm);
    }
    map
  }

}