package fr.proline.module.seq.service;

import static org.junit.Assert.*;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.junit.Test;

import fr.proline.module.seq.BioSequenceProvider;
import fr.proline.module.seq.dto.BioSequenceWrapper;
import fr.proline.module.seq.service.FastaPathsScanner;

public class ScannerTest {

    private static final String FASTA_FILE_PATH = ".";

    @Test
    public void testScanPaths() {
    	ArrayList<String> values = new ArrayList<>();
        values.add("YEAG_ECOLI"); 
        values.add("YQEB_ECOLI");
        for(int i=0;i<values.size();i++){
        	Map<String, List<BioSequenceWrapper>> result = BioSequenceProvider.findBioSequencesBySEDbIdentValues(values);
            System.out.println(values.get(i)); 
        	if(result.get(values.get(i))!=null){
        		 List<BioSequenceWrapper> bioSequenceWrapperList = result.get(values.get(i));
                 System.out.println("the biosequence is "+ bioSequenceWrapperList.get(0).getSequence());
                 System.out.println("the biosequence length is "+bioSequenceWrapperList.get(0).getSequence().length());
                 result.clear();
        	}else
        	{
        		System.out.println("the biosequence does not exist");
        	}  
        }
	final Map<String, List<File>> foundFastaFiles = FastaPathsScanner.scanPaths(new FastaPathsScanner(),
		new String[] { FASTA_FILE_PATH, });
	assertNotNull("FoundFastaFiles Map", foundFastaFiles);
    }

}
