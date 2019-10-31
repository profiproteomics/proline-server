package fr.proline.module.parser.maxquant;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import fr.proline.core.om.model.msi.IonTypes;
import fr.proline.core.om.model.msi.PtmDefinition;
import fr.proline.core.om.model.msi.PtmEvidence;
import fr.proline.core.om.model.msi.PtmLocation;
import fr.proline.core.om.model.msi.PtmNames;
import fr.proline.core.om.provider.msi.IPTMProvider;
import scala.Enumeration.Value;
import scala.Option;

public class PTMUtils {
	
	private static Map<String, List<PtmDefinition>> cachedPTMsByFullName = new HashMap<>();
	private static List<PtmDefinition> cachedPtms = new ArrayList<PtmDefinition>();
		
	static public List<PtmDefinition> parsePTMString(IPTMProvider ptmProvider, String ptmDefDescription) throws IllegalArgumentException {
		
		// Check if this modification has been already retrieved
	    if (cachedPTMsByFullName.containsKey(ptmDefDescription)) {
	      return cachedPTMsByFullName.get(ptmDefDescription);
	    }

	    List<PtmDefinition> ptmDefs = new ArrayList<PtmDefinition>();
	    String modName = ptmDefDescription;

	    String posConstraint = "";
	    
	    if (modName.contains("(")) { //Split name to ptmName + Residue/Site
	    	Pattern modRegEx = Pattern.compile("(.+) \\((.+)\\)");
	    	Matcher matcher= modRegEx.matcher(modName);
	    	if(matcher.find()){
	    		posConstraint = matcher.group(2);
	    		modName = matcher.group(1);
	    	}
	    }

	    // Get the map (location->residue) of each residue/site this PTM could be applied to
	    Map<Character, Value> posConstraints = parsePositionConstraint(posConstraint);

	    // Create a PtmDefinition for each position constraint (residue and/or location)
	    Iterator<Entry<Character, Value>> locationResidueIt = posConstraints.entrySet().iterator();
	    while(locationResidueIt.hasNext()){
	    	Entry<Character,Value> nextEntry = locationResidueIt.next();
	    	Character residue = nextEntry.getKey();
	    	char resChar = (residue != null) ? residue : '\0';
	    	Value location = nextEntry.getValue();
	    	if(resChar == '\0' && location.equals(PtmLocation.ANYWHERE()))
	    		throw new IllegalArgumentException("Error in modification description : No residue specified for Anywhere PTM.");
	    	PtmDefinition foundPtm = null;
	    	for(PtmDefinition nextCachedPtm : cachedPtms) {
	    		if(nextCachedPtm.names().equals(modName) && nextCachedPtm.residue()==resChar && nextCachedPtm.location().equals(location)){
	    			foundPtm = nextCachedPtm;	// PtmDefinition Already retrieve or created			
	    		}
	    	}

	        if (foundPtm == null) {
	        	Option<PtmDefinition> searchPtm = ptmProvider.getPtmDefinition(modName, resChar, location);

	          // TODO: DBO => I think it should be forbidden to do this because composition has to be known
	          if (searchPtm == null || searchPtm.isEmpty()) { //Ptm Definition don't exist. Create One 

	        	  Option<Object>  ptmIdOpt = ptmProvider.getPtmId(modName); // Get PtmName if exist
	        	  Long ptmId = 0L;

	        	  if (ptmIdOpt != null && ptmIdOpt.isDefined())
	        		  ptmId = (Long) ptmIdOpt.get();
	        	  else
	        		  ptmId = PtmNames.generateNewId();

	        	  // FIXME Create PtmEvidence : type Precursor is required ! + PtmClassification
	        	  PtmEvidence ptmEvidence = new PtmEvidence( IonTypes.Precursor(),
	        		  						"UNVALID", //composition
	        		  						Double.MAX_VALUE, //monoMass 
        		  							Double.MAX_VALUE,//averageMass  
        		  							true //isRequired
        		  							);
	        	  
	        	  PtmEvidence[] evidences = {ptmEvidence};
	        	  foundPtm = new PtmDefinition(PtmDefinition.generateNewId(),
	        		  			location.toString(),
	        		  			new PtmNames(modName, modName),
	        		  			evidences,
	        		  			resChar,
	        		  			"Post-translational",//classification 
	        		  			ptmId,
	        		  			0 //unimodId
	        		  			);
	          } else
	        	  foundPtm = searchPtm.get();
	        }//End Ptm not already created or retrieved

	          ptmDefs.add(foundPtm);
	          cachedPtms.add(foundPtm);
	    
	    } //End go through residue

	    cachedPTMsByFullName.put(ptmDefDescription, ptmDefs);
	    return ptmDefs;
	}

	
	/**
	   * Parse posConstraintAsStr containing the list of residues and/or location where a PTM could be applied
	   * The String is Mascot formated (ex: "Protein N-term M", "Protein C-term" or "CM")
	   *
	   * Return an Array containing tuple Option[residue] - location for each residue/site
	   *
	   */
	static private Map<Character, Value> parsePositionConstraint(String posConstraintAsStr ) {

		  // TODO: create an ArrayBuffer of PtmSpecificity objects
		  HashMap<Character, Value> posConstraints = new HashMap<Character, Value>();
	    	
		  // Copy the variables
		  String residuesAsStr = posConstraintAsStr;
		  String locConstraintAsStr = posConstraintAsStr;
	    
		  // Check if the position constraint contains a location constraint
		  Pattern locPattern = Pattern.compile("(.+term)(.*)");
		  Matcher locMatcher = locPattern.matcher(posConstraintAsStr);
		  if (locMatcher.find()) {
			  locConstraintAsStr = locMatcher.group(1);
			  if(locMatcher.groupCount()==2)
				  residuesAsStr = locMatcher.group(2).trim();
		  
		  
	      // Add '-' char if missing
	      if (! locConstraintAsStr.matches("(.+)-term")) { locConstraintAsStr = locConstraintAsStr.replaceFirst("term", "-term"); }

	      // If the location constraint is not specific to the protein	      
	      if (! Pattern.compile("(?i)protein").matcher(locConstraintAsStr).find()) { 
	    	  locConstraintAsStr = "Any " + locConstraintAsStr; 
	      } else {
		      // Else if the space is missing between protein and [NC]-term
	    	  Matcher spaceMatcher =   Pattern.compile("(?i)protein([NC]-term)").matcher(locConstraintAsStr);
	    	 if(spaceMatcher.find())
	    		 locConstraintAsStr = "Protein " + spaceMatcher.group(1);
	      }     

	    } else { locConstraintAsStr = "Anywhere"; }

	    // Try to convert the parsed string into an entry of the Location enum

		Value locVal =  PtmLocation.withName(locConstraintAsStr);
		char[] residues = residuesAsStr.toCharArray();
	    if (residues.length > 0) {
	      for (char residue : residues) {
	        posConstraints.put(new Character(residue), locVal);
	      }
	    } else {
	      posConstraints.put(null, locVal);
	    }

	    return posConstraints;
	  }
}
