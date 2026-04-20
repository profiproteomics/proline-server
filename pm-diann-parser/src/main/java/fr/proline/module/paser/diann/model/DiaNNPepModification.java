/*
 * Copyright (C)  2026.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the CeCILL FREE SOFTWARE LICENSE AGREEMENT
 * ; either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * CeCILL License V2.1 for more details.
 *
 * You should have received a copy of the CeCILL License
 * along with this program;
 * If not, see <http://www.cecill.info/licences/Licence_CeCILL_V2.1-en.html>.
 *
 */

package fr.proline.module.paser.diann.model;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class DiaNNPepModification {
  String m_ptmModif;;
  Boolean m_isUnimod;

  int m_ptmLocation;
  private static final Logger logger = LoggerFactory.getLogger(DiaNNPepModification.class);
  public DiaNNPepModification(String ptmModif, Boolean isUnimod, int ptmLocation){
    m_ptmModif = ptmModif;
    m_isUnimod = isUnimod;
    m_ptmLocation = ptmLocation;
  }

  public String getModifDescription(){
    return m_ptmModif;
  }

  public Boolean isUnimodModif(){
    return m_isUnimod;
  }

  public int getUnimodId(){
    int id =0;
    if(m_isUnimod){
      String[] parts = m_ptmModif.split(":");
      try {
        if(parts.length == 2){}
          id = Integer.parseInt(parts[1]);
      }catch(NumberFormatException e){
        id =0;
      }

    }
    return id;
  }

  public int getModifLocation(){
    return m_ptmLocation;
  }


//  private static int nbModif = 0;
//  private static Set<String> foundModifs =  new HashSet<>();


  public static List<DiaNNPepModification>  parseModification(String modifiedSequence){
    List<DiaNNPepModification> modifications = new ArrayList<>();
    boolean stillSearch = true;
    int start = 0;
    while(stillSearch){
       start = modifiedSequence.indexOf('(', start);
       if(start == -1)
         stillSearch = false;
       else {

         int end = modifiedSequence.indexOf(')', start);

         if (end != -1) {
           String content = modifiedSequence.substring(start + 1, end);
//           nbModif++;
//           foundModifs.add(content);

           DiaNNPepModification pepModification = new DiaNNPepModification(content, content.startsWith("Unimod:"), start);
           modifications.add(pepModification);
           start = end + 1;

         } else {
           String messsage = "Error Parsing Modification. Start with no End in "+modifiedSequence;
           logger.warn(messsage);
           throw new RuntimeException(messsage);
         }
       }
    }
//    if(nbModif % 500 == 0)
//      logger.info("Found "+nbModif+" modifications "+foundModifs.size()+" differents");
    return modifications;
  }
}
