package fr.proline.module.seq;

import fr.profi.util.StringUtils;
import fr.proline.module.seq.dto.DBioSequence;
import fr.proline.module.seq.dto.DDatabankInstance;
import fr.proline.module.seq.dto.DDatabankProtein;
import fr.proline.module.seq.dto.DRepositoryProtein;
import fr.proline.module.seq.orm.*;
import fr.proline.module.seq.orm.dao.DatabankProteinDao;
import fr.proline.repository.IDatabaseConnector;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.persistence.EntityManager;
import java.util.*;


public final class BioSequenceProvider {

  private static final Logger LOG = LoggerFactory.getLogger(BioSequenceProvider.class);


  /**
   * Create a Map from proteins identifiers (name/acc..) to an associated RelatedIdentifiers
   * The RelatedIdentifiers contains the List of DBioSequence for each proteinIdentifier found and the List of DDatabankProtein
   * (if one value corresponds to multiple proteins)
   *
   * @param proteinIdentifiers List of proteins identifiers to search for
   * @return Same protein identifierd mapped to a RelatedIdentifiers (BioSequecences and Protein from DBs)
   */
  public static Map<String, RelatedIdentifiers> findSEDbIdentRelatedData(final Collection<String> proteinIdentifiers) {
    Map<String, RelatedIdentifiers> result = null;

    /* Client / Provider side */
    final IDatabaseConnector seqDb = DatabaseAccess.getSEQDatabaseConnector(false);

    EntityManager seqEM = seqDb.createEntityManager();

    try {
      result = findMatchingProteins(seqEM, proteinIdentifiers, null);
    } finally {

      if (seqEM != null) {
        try {
          seqEM.close();
        } catch (Exception exClose) {
          LOG.error("Error closing SEQ Db EntityManager", exClose);
        }
      }
    }
    return result;
  }

  /**
   * Create a Map from proteins identifiers (name/acc..) to an associated RelatedIdentifiers
   * The RelatedIdentifiers contains the List of DBioSequence for each proteinIdentifier found and the List of DDatabankProtein
   * (if one value corresponds to multiple proteins)
   *
   * @param proteinIdentifiers List of proteins identifiers to search for
   * @param proteinsAccByDatabankSubMap proteins identifiers grouped by DatabankInstance where they are referenced.

   * @return
   */
  public static Map<String, RelatedIdentifiers> findSEDbIdentRelatedData(final Collection<String> proteinIdentifiers, Map<DDatabankInstance, List<String>> proteinsAccByDatabankSubMap ) {
    Map<String, RelatedIdentifiers> result = null;

    /* Client / Provider side */
    final IDatabaseConnector seqDb = DatabaseAccess.getSEQDatabaseConnector(false);

    EntityManager seqEM = seqDb.createEntityManager();

    try {
      result = findMatchingProteins(seqEM, proteinIdentifiers, proteinsAccByDatabankSubMap ); // null for previous behaviour
    } finally {

      if (seqEM != null) {
        try {
          seqEM.close();
        } catch (Exception exClose) {
          LOG.error("Error closing SEQ Db EntityManager", exClose);
        }
      }
    }
    return result;
  }
  /**
   * Find all DataBankProteins matching to the list of supplied identifiers. The search did not care about the databank
   * associated with the proteins found in the SeqDb, but returns all of them.
   *
   * @param seqEM EntityManager to access to SequenceDB
   * @param proteinsIdentifiers List of proteins identifiers to search for
   * @param proteinsAccByDatabankSubMap proteins identifiers grouped by DatabankInstance where they are referenced.
   * @return
   */
  private static Map<String, RelatedIdentifiers> findMatchingProteins(final EntityManager seqEM, final Collection<String> proteinsIdentifiers,  Map<DDatabankInstance, List<String>> proteinsAccByDatabankSubMap ) {

    final Map<String, RelatedIdentifiers> result = new HashMap<>();
    List<DatabankProtein> foundProteins = new ArrayList<>();
    List<String> notFoundProteins = new ArrayList<>();

    if(proteinsAccByDatabankSubMap == null)
      //Get Protein from seqRepo only based on the protein identifier
      foundProteins =  DatabankProteinDao.findProteins(seqEM, proteinsIdentifiers);
    else {
      //try to get more specific protein using DSeqDBInstance release or fasta path.
      notFoundProteins.addAll(proteinsIdentifiers);
      //get protein using prot identifier and DSeqDBInstance release
      for(DDatabankInstance dbInst : proteinsAccByDatabankSubMap.keySet()) {
        if (!StringUtils.isEmpty(dbInst.getRelease())) {
          List<DatabankProtein> currentFoundProts = DatabankProteinDao.findProteinsInDatabankNameAndRelease(seqEM, dbInst.getName(), dbInst.getRelease(), notFoundProteins);
          foundProteins.addAll(currentFoundProts);
          //Remove protein found using identifier and DSeqDBInstance release.
          for (DatabankProtein nextProt : currentFoundProts) {
            notFoundProteins.remove(nextProt.getIdentifier());
          }
        }
      }

      //If still some Protein not found, try using fasta name
      if(!notFoundProteins.isEmpty()) {
        for (DDatabankInstance dbInst : proteinsAccByDatabankSubMap.keySet()) {
          List<DatabankProtein> currentFoundProts = DatabankProteinDao.findProteinsInDatabankName(seqEM, dbInst.getName(), notFoundProteins);

          //Test if fasta file is the same as seqDb one
          for(DatabankProtein dbProt : currentFoundProts){

            String seqDBFastaname = dbInst.getSourcePath().replace("\\","/");
            int lastIndex =seqDBFastaname.lastIndexOf('/');
            seqDBFastaname = seqDBFastaname.substring(lastIndex+1,seqDBFastaname.length());

            String dbProtFastaname = dbProt.getDatabankInstance().getSourcePath().replace("\\","/");
            lastIndex = dbProtFastaname.lastIndexOf('/');
            dbProtFastaname = dbProtFastaname.substring(lastIndex+1,dbProtFastaname.length());

            if(seqDBFastaname.equals(dbProtFastaname)){
              foundProteins.add(dbProt);
              //Remove protein found using identifier and DSeqDBInstance fasta name.
              notFoundProteins.remove(dbProt.getIdentifier());
            }
          }
        }
      }

      //Still some missing protein, just use protein identifier
      if(!notFoundProteins.isEmpty()) {
        List<DatabankProtein> currentFoundProts = DatabankProteinDao.findProteins(seqEM, notFoundProteins);
        foundProteins.addAll(currentFoundProts);
        if(currentFoundProts.size()< notFoundProteins.size()){
          LOG.debug(" {} proteins were not found in sequence database ", (notFoundProteins.size()-currentFoundProts.size()));
          if(LOG.isTraceEnabled()){
            for(DatabankProtein dbProt : currentFoundProts){
              notFoundProteins.remove(dbProt.getIdentifier());
            }
            LOG.trace(" Not found proteins: "+notFoundProteins.toString());
          }
        }
       }
    }

    if ((foundProteins != null) && !foundProteins.isEmpty()) {

      for (final DatabankProtein protein : foundProteins) {

        final String key = protein.getIdentifier();// Should not be null
        if ((key != null) && (!key.isEmpty())) {
          final String description = protein.getDescription();//could be null

          RelatedIdentifiers relatedIdentifiers = result.get(key);
          if (relatedIdentifiers == null) {
            relatedIdentifiers = new RelatedIdentifiers();
            result.put(key, relatedIdentifiers);
          }

          relatedIdentifiers.getDBioSequences().add(buildDBioSequence(protein));
          relatedIdentifiers.getDDatabankProteins().add(new DDatabankProtein(key, description));

        }
      } //End go through matchingIdentifiers
    }

    return result;
  }

  private static DBioSequence buildDBioSequence(final DatabankProtein protein) {
    assert (protein != null) : "buildDBioSequence() protein is null";

    final BioSequence bioSequence = protein.getBioSequence();// Should not be null
    final long sequenceId = bioSequence.getId();
    final String sequence = bioSequence.getSequence();// Should not be null

    final DatabankInstance seDbInstance = protein.getDatabankInstance();// Should not be null

    final String seDbRelease = seDbInstance.getRelease();// Should not be null
    final String sourcePath = seDbInstance.getSourcePath();// Should not be null

    final Databank seDb = seDbInstance.getDatabank();// Should not be null
    final String seDbName = seDb.getName();// Should not be null
    final Alphabet alphabet = seDb.getAlphabet();// Should not be null

    final DDatabankInstance seDbInstanceW = new DDatabankInstance(seDbName, alphabet, sourcePath);

    DRepositoryProtein repositoryIdentValue = null;

    final RepositoryProtein repositoryIdent = protein.getRepositoryIdentifier();

    if (repositoryIdent != null) {// Can be null
      repositoryIdentValue = buildDRepositoryProtein(repositoryIdent);
    }

    return new DBioSequence(sequenceId, sequence, seDbInstanceW, seDbRelease, repositoryIdentValue);
  }

  private static DRepositoryProtein buildDRepositoryProtein(final RepositoryProtein repositoryIdent) {

    assert (repositoryIdent != null) : "buildDRepositoryProtein() repositoryIdent is null";

    Repository repository = repositoryIdent.getRepository();// Should not be null
    String repositoryName = repository.getName();// Should not be null
    String repositoryURL = repository.getURL();// Can be null
    String repositoryIdentValue = repositoryIdent.getValue();// Should not be null

    return new DRepositoryProtein(repositoryName, repositoryURL, repositoryIdentValue);
  }

  public static class RelatedIdentifiers {

    List<DBioSequence> bioSequences;
    List<DDatabankProtein> proteins;

    public RelatedIdentifiers() {
      bioSequences = new ArrayList<>();
      proteins = new ArrayList<>();
    }

    public List<DBioSequence> getDBioSequences() {
      return bioSequences;
    }

    public List<DDatabankProtein> getDDatabankProteins() {
      return proteins;
    }

  }
}
