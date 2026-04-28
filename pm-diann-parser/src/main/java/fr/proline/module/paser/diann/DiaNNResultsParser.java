package fr.proline.module.paser.diann;

import fr.profi.api.service.IServiceWrapper;
import fr.profi.chemistry.model.Enzyme;
import fr.profi.util.StringUtils;
import fr.profi.util.serialization.ProfiJson;
import fr.proline.context.DatabaseConnectionContext;
import fr.proline.context.MsiDbConnectionContext;
import fr.proline.core.dal.tables.msi.MsiDbPeaklistSoftwareColumns;
import fr.proline.core.om.model.msi.*;
import fr.proline.core.om.provider.ProviderDecoratedExecutionContext;
import fr.proline.core.om.provider.msi.IInstrumentConfigProvider;
import fr.proline.core.om.provider.msi.IPTMProvider;
import fr.proline.core.om.storer.msi.IPeaklistWriter;
import fr.proline.core.om.storer.msi.IRsStorer;
import fr.proline.core.om.storer.msi.RsStorer;
import fr.proline.core.om.storer.msi.impl.SQLMsiSearchWriter;
import fr.proline.core.om.storer.msi.impl.StorerContext;
import fr.proline.module.paser.diann.builder.DiaNNPeaklistContainer;
import fr.proline.module.paser.diann.builder.DiaNNProcessData;
import fr.proline.module.paser.diann.builder.MSDataBuilder;
import fr.proline.module.paser.diann.model.DiaNNResult;
import fr.proline.module.paser.diann.parquet.DiaNNParquetReader;
import fr.proline.repository.util.JDBCWork;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOCase;
import org.apache.commons.io.filefilter.FileFilterUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.Option;
import scala.collection.JavaConverters;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class DiaNNResultsParser  extends IServiceWrapper {

  private static final Logger logger = LoggerFactory.getLogger(DiaNNResultsParser.class);

  public static String MAIN_REPORT_PARQUET = "report.parquet";
  public static String REPORT_LOG = "report.log.txt";

  public static String INSTR_CONFIG_OPTION_KEY = "instrumentConfigId";
  public static String PEAKLIST_SOFT_ID_OPTION_KEY = "peaklistSoftwareId";
  private static final Long DEFAULT_INSTRUM_CFG_ID = 4L;
  private static final Long DEFAULT_PEAKLIST_SOFT_ID = 1L; //extract msn.. diann to create TODO

  private final File m_diaNNDirPath;
  private  File m_mainReportFile;

  private Map<String,Object> m_diaNNOptions;
  private Long m_instrConfigId;
  private Long m_peaklistSoftwareId;

  private List<String> m_usedFixedPTMs;
  private List<String> m_usedVarPTMs;
  private Map<String,String> m_filePathsByRun;

  private final ProviderDecoratedExecutionContext m_parserContext;


  Map<String, ResultSet> m_resultSetsByRun;
  Map<Long,Long> m_rsmIdsByRSId;
  Map<Long, Map<Long,Spectrum>> m_spectraByIdByRsId;
  Map<String, Set<String>> m_precIdByPepKey;
  Long m_datasetId;

  enum EnzymeParse {
    TRYPSIN("K*,R*", "Trypsin/P"),
    TRYPSINP("K*,R*,!*P", "Trypsin"),
    LYSC("K*,!*P", "Lys-C"),
    LYSCP("K*,R*", "Lys-C/P");

    String m_cutValue;
    String m_name;

    EnzymeParse(String cutValue, String name) {
      this.m_name = name;
      this.m_cutValue = cutValue;
    }

    static EnzymeParse fromString(String cutValue) {
      for (EnzymeParse e : EnzymeParse.values()) {
        if(e.m_cutValue.equals(cutValue))
          return e;
      }
      return null;
    }
  }

  public DiaNNResultsParser(ProviderDecoratedExecutionContext parserContext, File diannFolder, Map<String,Object> parserOptions) {
    m_parserContext = parserContext;
    m_diaNNDirPath = diannFolder;
    logger.debug("- ** DiaNNResultsParser initialization using folder "+m_diaNNDirPath.getAbsolutePath());
  //    m_parserOptions = parserOptions;
    try {
      m_instrConfigId = parserOptions.containsKey(INSTR_CONFIG_OPTION_KEY) ? (Long)parserOptions.get(INSTR_CONFIG_OPTION_KEY) : DEFAULT_INSTRUM_CFG_ID;
      m_peaklistSoftwareId = parserOptions.containsKey(PEAKLIST_SOFT_ID_OPTION_KEY) ?(Long) parserOptions.get(PEAKLIST_SOFT_ID_OPTION_KEY) : DEFAULT_PEAKLIST_SOFT_ID;
    } catch (Exception e) {
      m_instrConfigId = DEFAULT_INSTRUM_CFG_ID;
      m_peaklistSoftwareId = DEFAULT_PEAKLIST_SOFT_ID;
    }

    m_spectraByIdByRsId =  new HashMap<>();
    m_resultSetsByRun = new HashMap<>();
    m_datasetId = -1L;
    parseDiaNNParams();
  }

  public DiaNNResultsParser(ProviderDecoratedExecutionContext parserContext, String diannFolderPath, Map<String,Object> parserOptions) {
    this(parserContext, new File(diannFolderPath), parserOptions);
  }

  private void parseDiaNNParams(){

    List<File> reportFiles = (List<File>)(FileUtils.listFiles(m_diaNNDirPath, FileFilterUtils.nameFileFilter(MAIN_REPORT_PARQUET, IOCase.INSENSITIVE), null));
    if(reportFiles == null || reportFiles.size() != 1){
      throw new IllegalArgumentException("Invalid path specified:  No or more than one report found");
    }
    m_mainReportFile = new File(m_diaNNDirPath, MAIN_REPORT_PARQUET);
    if(!m_mainReportFile.exists()){
      throw new IllegalArgumentException("DiaNN report file does not exist: " + m_mainReportFile.getAbsolutePath());
    }
    File reportLogFile = new File(m_diaNNDirPath, REPORT_LOG);
    if(!reportLogFile.exists()){
      throw new IllegalArgumentException("DiaNN report LOG file does not exist: " + reportLogFile.getAbsolutePath());
    }
    logger.debug("... Found files ... parseDiaNNParams extractParams ");
    try {
      m_diaNNOptions = extractParams(reportLogFile);
      m_filePathsByRun = new HashMap<>();
      logger.debug(" - parseDiaNNParams extract {} params ",m_diaNNOptions.size());
      ArrayList<String> filePath= getParamValueAsList("f");
      if(filePath == null){
        throw new IllegalArgumentException("DiaNN report LOG file " + reportLogFile.getAbsolutePath()+" is not valid. No file specified ");
      }
      logger.debug(" - result contains {} raw files",filePath.size());
      for(String filePathStr : filePath){
        String nextRun = FilenameUtils.getBaseName(filePathStr);
        m_filePathsByRun.put(nextRun, filePathStr);
      }

    } catch (IOException e) {
      throw new IllegalArgumentException("DiaNN report LOG file " + reportLogFile.getAbsolutePath()+" is not valid: "+e.getMessage());
    }
    logger.debug(" - Parse Modification ");
    //Read Modif in log
    m_usedFixedPTMs = getFixedModif();
    m_usedVarPTMs = getVarModif();

  }

  private DiaNNResult readDiaNNResult(){
    try {
      DiaNNParquetReader fileReader = new DiaNNParquetReader(m_mainReportFile);
      return fileReader.readData();
    } catch (SQLException e) {
      logger.error("Error reading DiaNN file {}", m_mainReportFile.getAbsolutePath(),e);
      throw new RuntimeException(e);
    }
  }

  @Override
  public boolean runService() {
    logger.info("-- Running DiaNN Parser on {}", m_diaNNDirPath.getAbsolutePath());
    DiaNNResult result= readDiaNNResult();
    logger.info("-- Data read from file ");
    createResultsData(result);
    logger.info("-- Proline Data created ");
    return true;
  }

  public List<Long> getResultSetsIds(){
    return m_resultSetsByRun.values().stream().map(ResultSet::id).toList();
  }

  public Map<Long,Long> getRSMIdByResultSetId(){
    return m_rsmIdsByRSId;
  }

  public Long getCreatedQuantDatasetId(){
    return m_datasetId;
  }

  private void createResultsData(DiaNNResult diaNNResult) {
    MsiDbConnectionContext msiDbCtx = m_parserContext.getMSIDbConnectionContext();
    boolean localMSITransaction= false;
    boolean msiTransacOk= false;

    StorerContext storerContext = null;
    Map<String, Long> rsIdByName = new HashMap<>();

    try {
      // Check if a transaction is already initiated
      if (!msiDbCtx.isInTransaction()) {
        msiDbCtx.beginTransaction();
        localMSITransaction = true;
      }

      logger.debug(" --- Start Create ResultSet's data");
      //Create Commons Objects
      List<PtmDefinition> fixedPtmDef =  createPtmList(m_usedFixedPTMs);
      List<PtmDefinition> varPtmDef =  createPtmList(m_usedVarPTMs);
      logger.debug(" .. Found {} var ptm and {} fixed ptm ", varPtmDef.size(), fixedPtmDef.size());
      logger.debug(" ..... {} ", varPtmDef);
      logger.debug(" ..... {} ",  fixedPtmDef);

      // Retrieve the instrument configuration VDS TODO Read from parserOption
      IInstrumentConfigProvider instConfigProvider= m_parserContext.getProvider(IInstrumentConfigProvider.class);
      Option<InstrumentConfig> instrumentConfigOpt = instConfigProvider.getInstrumentConfig(m_instrConfigId);
      if(instrumentConfigOpt.isEmpty()){
        throw new RuntimeException("can't find an Instrument Config for id = " + m_instrConfigId);
      }

      createResultSets(diaNNResult, varPtmDef, fixedPtmDef, instrumentConfigOpt, rsIdByName);

      //Store data
      logger.info(" --- Storing ResultSets");
      storerContext = StorerContext.apply(m_parserContext);
      Map<String, Long> rsMap =  storeResultSets(m_resultSetsByRun.values().stream().toList(), instrumentConfigOpt.get(),  storerContext);
      for(String run : rsMap.keySet()) {
        logger.info(" New ID {} for run {} (was {}) ", rsMap.get(run), run, rsIdByName.get(run));
      }

      logger.debug(" --- Start Create Validated and Quantitation data");
      createProcessedResults(diaNNResult);

      // Commit transaction if it was initiated locally
      if (localMSITransaction) {
        msiDbCtx.commitTransaction();
      }
      msiTransacOk = true;
    }catch( Throwable t){
      logger.error("Error while importing DiaNN resultFile", t);
      throw new RuntimeException(t);

    } finally{

      if (storerContext != null) {
        storerContext.clearContext();
      }

      if (localMSITransaction && !msiTransacOk) {
        logger.info("Roll backing MSI Db Transaction");

        try {
          msiDbCtx.rollbackTransaction();
        } catch (Exception ex){
          logger.error("Error roll backing MSI Db Transaction", ex);
        }

      }

    }
  }

  private void createResultSets(DiaNNResult diaNNResult, List<PtmDefinition> varPtmDef, List<PtmDefinition> fixedPtmDef, Option<InstrumentConfig> instrumentConfigOpt, Map<String, Long> rsIdByName) {
    //Create SeqDatabase
    logger.debug(" - Create SeqDatabase ");
    List<SeqDatabase> seqDbsList = createSeqDatabase();

    Enzyme[] enzymes = new Enzyme[1];
    enzymes[0] = createEnzyme();
    String mcAsStr = m_diaNNOptions.getOrDefault("missed-cleavages", "0").toString();
    int mc = 0;
    if(mcAsStr != null ) {
      try {
        mc = Integer.parseInt(mcAsStr);
      }catch (NumberFormatException e) {
        logger.warn(" ERROR parsing missed-cleavages param ! {}: {}", mcAsStr, e.getMessage());
      }
    }

    logger.debug(" - Create Common Search Settings ");
    //String ms1Charge = parseCharge(); VDS TODO get from diann option min-pr-charge/max-pr-charge
    SearchSettings commonSS = new SearchSettings(
            SearchSettings.generateNewId(),
            "DiaNN",
            "1.9.2",
            "none",
            mc,
            "1+ - 4+",
            0,
            "ppm",
            false,
            enzymes,
            varPtmDef.toArray(new PtmDefinition[0]),
            fixedPtmDef.toArray(new PtmDefinition[0]),
            seqDbsList.toArray(new SeqDatabase[0]),
            instrumentConfigOpt.get(),
            Option.empty(), //no fragmentationRuleSet
            Option.empty(), //no msmsSearchSettings
            Option.empty(), //no msmsSearchSettings
            Option.empty() //no msmsSearchSettings
    );


    //Create Run Specific objects
    List<String> runs = diaNNResult.getRuns();
    logger.debug(" - Go though {} runs ", runs.size());
    MSDataBuilder dataBuilder = new MSDataBuilder(m_parserContext);
    for(String run : runs) {
      MSISearch msiSearch = createMsiSearch(run,commonSS);
      Long rsId = ResultSet.generateNewId();
      rsIdByName.put(run, rsId);
      ResultSet rsRun = dataBuilder.createResultSet(msiSearch, rsId, run, diaNNResult);
      Map<Long, Spectrum> spectraById = dataBuilder.getSpectraById();
      m_resultSetsByRun.put(run, rsRun);
      m_spectraByIdByRsId.put(rsId, spectraById);
    }
    m_precIdByPepKey =  dataBuilder.getPrecIdByPepUniqueKey();

  }

  private void createProcessedResults(DiaNNResult diaNNResult){

    DiaNNProcessData processData = new DiaNNProcessData(m_parserContext, diaNNResult, m_resultSetsByRun, m_precIdByPepKey);
    processData.runService();
    m_rsmIdsByRSId = processData.getRSMIdsByRSIds();
    m_datasetId = processData.getCreatedQuantDatasetId();
  }

  private List<SeqDatabase> createSeqDatabase() {
    List<SeqDatabase> seqDbsList = new ArrayList<>();
    ArrayList<String> fastaName = getParamValueAsList("fasta");
    ArrayList<String> specLibName = getParamValueAsList("lib");
    if(fastaName == null && specLibName == null ) {
      seqDbsList.add(new SeqDatabase(SeqDatabase.generateNewId(),
              "NoSeqDB", // name
              "NoPath", // file Path
              0, //nbr seq
              new Date(),  // release date
              "Unknown", //version
              0, //nbr searched seq
              Option.empty(),  // SeqDatabaseProperties
              Option.empty()));
    } else if(fastaName != null) {
      for(String nextFasta : fastaName) {
        seqDbsList.add(new SeqDatabase(SeqDatabase.generateNewId(),
                FilenameUtils.getBaseName(nextFasta), // name
                nextFasta, // file Path
                0, //nbr seq
                new Date(),  // release date
                "Unknown", //version
                0, //nbr searched seq
                Option.empty(),  // SeqDatabaseProperties
                Option.empty()));
      }
    } else {
      for(String nextSpeclib : specLibName) {
        seqDbsList.add(new SeqDatabase(SeqDatabase.generateNewId(),
                FilenameUtils.getBaseName(nextSpeclib), // name
                nextSpeclib, // file Path
                0, //nbr seq
                new Date(),  // release date
                "Unknown", //version
                0, //nbr searched seq
                Option.empty(),  // SeqDatabaseProperties
                Option.empty()));
      }
    }
    return  seqDbsList;
  }

  private List<PtmDefinition> createPtmList(List<String> diannSrcDef) {
    IPTMProvider ptmProvider= m_parserContext.getProvider(IPTMProvider.class);
    List<PtmDefinition> createdPtmDefs = new ArrayList<>();

    boolean foundErr = false;
    for(String ptmDesc : diannSrcDef){
      String[] ptmParts  = ptmDesc.split(",");
      StringBuilder errorMessage = new StringBuilder("Error parsing ptm ");

      if(ptmParts.length ==1 ) {
        if(ptmDesc.startsWith("unimod") ) { //only one word: should be such as "unimod<id>"
          errorMessage.append(ptmDesc).append(": ");
          try {
            int unimodId = Integer.parseInt(ptmParts[0].substring(6));
            PtmDefinition[] ptmDefs = ptmProvider.getUnimodPtmDefinition(unimodId);
            if (ptmDefs != null && ptmDefs.length > 0) {
              if (unimodId == 4) {
                ptmDefs = extractResidue(ptmDefs, 'C');
              } else if (unimodId == 35) {
                ptmDefs = extractResidue(ptmDefs, 'M');
              } else if (unimodId == 1) {
                ptmDefs = extractResidue(ptmDefs, '\u0000');
              } else if (unimodId == 21) {
                PtmDefinition[] ptmDefStep = extractResidue(ptmDefs, 'S');
                List<PtmDefinition> incremPtms = new ArrayList<>(List.of(ptmDefStep));
                ptmDefStep = extractResidue(ptmDefs, 'T');
                incremPtms.addAll(List.of(ptmDefStep));
                ptmDefStep = extractResidue(ptmDefs, 'Y');
                incremPtms.addAll(List.of(ptmDefStep));
                ptmDefs = incremPtms.toArray(new PtmDefinition[0]);
              }
              if (ptmDefs == null || ptmDefs.length == 0){
                errorMessage.append(" Residue filtering error.");
                foundErr = true;
              } else
                createdPtmDefs.addAll(Arrays.stream(ptmDefs).toList());
            } else {
              errorMessage.append(" Modification not found in Proline");
              foundErr = true;
            }
          } catch (NumberFormatException nfe){
            errorMessage.append("Unable to extract unimod Id.");
            foundErr = true;
          }

        } else  {
          errorMessage.append(" Not well formatted single unimod parameter");
          foundErr = true;
        }

      }  else if (ptmParts.length >= 3 ){
        // ptm description is formatted as  [name],[mass],[sites],[optional: 'label'] where name could be unimod:id
        String ptmName = ptmParts[0];
//        double ptmMass = Double.parseDouble(ptmParts[1]);
        String sites = ptmParts[2];
//        String label = ptmParts.length > 3 ? ptmParts[3] : null;
        if(ptmName.toLowerCase().startsWith("unimod:")) {
          try {
            int unimodId = Integer.parseInt(ptmParts[0].substring(7));
            PtmDefinition[] ptmDefs = ptmProvider.getUnimodPtmDefinition(unimodId);
            if(ptmDefs != null && ptmDefs.length>0) {
              if (sites.equals("*n") || sites.equals("*c")) //N|C-Term, no residue
                ptmDefs = extractResidue(ptmDefs, '\u0000');
              else {
                List<PtmDefinition> incremPtms = new ArrayList<>();
                for(char site : sites.toCharArray()) {
                  PtmDefinition[] ptmDefStep = extractResidue(ptmDefs, site);
                  incremPtms.addAll(List.of(ptmDefStep));
                }
                ptmDefs = incremPtms.toArray(new PtmDefinition[0]);
              }
              if (ptmDefs == null || ptmDefs.length == 0){
                errorMessage.append(" Residue filtering error.");
                foundErr = true;
              } else
                createdPtmDefs.addAll(Arrays.stream(ptmDefs).toList());
            } else {
              errorMessage.append(" Modification not found in Proline");
              foundErr = true;
            }
          } catch (NumberFormatException nfe){
            errorMessage.append("Unable to extract unimod Id.");
            foundErr = true;
          }
        } else  {
          errorMessage.append(" Not well formatted PTM definition parameter, should start with Unimod:<id>");
          foundErr = true;
        }
      } else {
        errorMessage.append("  Not well formatted PTM definition parameter, more than expected parts");
        foundErr = true;
      }

      if(foundErr) {
        throw new IllegalArgumentException("Error reading PTMs from diaNN result : "+errorMessage.toString());
      }
    }// End go through Diann PTM description
    return createdPtmDefs;
  }

  private Enzyme createEnzyme() {
    Enzyme enzyme = new Enzyme("Trypsin/P");
    if(m_diaNNOptions.containsKey("cut")){
      String enzymeRule = m_diaNNOptions.get("cut").toString();
      EnzymeParse enzymeParsed = EnzymeParse.fromString(enzymeRule);
      if(enzymeParsed != null) {
        enzyme = new Enzyme(enzymeParsed.m_name);
      }
    }
    return enzyme;
  }

  private PtmDefinition[] extractResidue(PtmDefinition[] ptmDefs, char residue) {
    if(ptmDefs == null || ptmDefs.length == 0)
        return ptmDefs;

    List<PtmDefinition> newPtms = Arrays.stream(ptmDefs).filter(ptmd-> ptmd.residue()==residue).toList();
    if(!newPtms.isEmpty())
      ptmDefs = newPtms.toArray(new PtmDefinition[0]);
    else
      logger.warn(" !!!!! WARNING !! Unable to retrieve specific residue {} for unimod {} ", residue, ptmDefs[0].unimodId());
    return ptmDefs;
  }

  private MSISearch createMsiSearch(String run, SearchSettings searchSettings) {
    String filePath = m_filePathsByRun.getOrDefault(run, run);
    //Create Peaklist
    Peaklist peaklist=  new Peaklist( Peaklist.generateNewId(), //id
            "raw", // fileType
            filePath, // path
            "", //rawFileIdentifier
            2, // msLevel
            "none", //spectrumDataCompression
            getOrCreatePeaklistSoftware() ,// peaklistSoftware
            Option.empty());//PeaklistProperties

    return new MSISearch(
            MSISearch.generateNewId(),
            run,
            searchSettings,
            peaklist,
            new Date(),
            run+" Search",
            m_diaNNDirPath.getAbsolutePath(),
            0,
            "",
            "",
            0,
            0,
            Option.empty()
    );
  }

  private PeaklistSoftware getOrCreatePeaklistSoftware() {

    MsiDbConnectionContext msiDbCtx = this.m_parserContext.getMSIDbConnectionContext();
    fr.proline.core.om.provider.msi.impl.SQLPeaklistSoftwareProvider msiPklSoftProvider = new fr.proline.core.om.provider.msi.impl.SQLPeaklistSoftwareProvider(msiDbCtx);
    fr.proline.core.om.provider.uds.impl.SQLPeaklistSoftwareProvider udsPklSoftProvider = new fr.proline.core.om.provider.uds.impl.SQLPeaklistSoftwareProvider(this.m_parserContext.getUDSDbConnectionContext());

    Option<PeaklistSoftware> udsPklSoftOpt = udsPklSoftProvider.getPeaklistSoftware(m_peaklistSoftwareId);
    if(udsPklSoftOpt.isEmpty())
      throw new RuntimeException("can't find a peaklist software for id = " + m_peaklistSoftwareId);

    // Try to retrieve peaklist software from the MSidb
    Option<PeaklistSoftware> msiPklSoftOpt = msiPklSoftProvider.getPeaklistSoftware(m_peaklistSoftwareId);
    PeaklistSoftware pklSoft = udsPklSoftOpt.get();
    if (msiPklSoftOpt.isEmpty()) {
      // If it doesn't exist => retrieve from the UDSdb
      JDBCWork insertWork = connection -> {

        String properties = (pklSoft.properties() != null && pklSoft.properties().isDefined()) ? ProfiJson.serialize(pklSoft.properties()) : null;

        StringBuilder sb = new StringBuilder(" INSERT INTO peaklist_software (");
        sb.append(MsiDbPeaklistSoftwareColumns.ID()).append(",");
        sb.append(MsiDbPeaklistSoftwareColumns.NAME()).append(",");
        sb.append(MsiDbPeaklistSoftwareColumns.VERSION()).append(",");
        sb.append(MsiDbPeaklistSoftwareColumns.SERIALIZED_PROPERTIES());
        sb.append(") VALUES (").append(pklSoft.id()).append(",");
        sb.append("'").append(pklSoft.name()).append("' ,");
        if(StringUtils.isEmpty(pklSoft.version()))
          sb.append((String)null).append(",");
        else
          sb.append("'").append(pklSoft.version()).append("' ,");
        if(StringUtils.isEmpty(properties))
          sb.append(properties).append(")");
        else
          sb.append("'").append(properties).append("' )");
        Statement stmt = connection.createStatement();
        logger.warn(" WILL EXECUTE {}", sb);
        stmt.execute(sb.toString());
      };

      try {
        msiDbCtx.doWork(insertWork, false);
      } catch (SQLException e) {
        throw new RuntimeException(" Error inserting peaklist software in MSI : "+e.getMessage());
      }
    }
    return pklSoft;
  }

  private Map<String,Long> storeResultSets(List<ResultSet> resultSets, InstrumentConfig instrumentConfig,/*ResultSetsDataMapper rsMapper,*/ StorerContext storerContext){
    Map<String,Long> rsIdByName = new HashMap<>();
    DatabaseConnectionContext msiDbContext = m_parserContext.getMSIDbConnectionContext();
    IRsStorer rsStorer = RsStorer.apply(msiDbContext,false);

    SQLMsiSearchWriter.insertInstrumentConfig(instrumentConfig, storerContext);

    for(ResultSet nextRS : resultSets){
      logger.debug("STORE RS {} / MSiSearch {} / SS {} ", nextRS.id(), nextRS.msiSearch().get().id(), nextRS.msiSearch().get().searchSettings().id());
      Long rsId = storeResultFile(nextRS, /*rsMapper,*/ storerContext, rsStorer);
      logger.debug(" DONE WITH  MSiSearch {} / SS {} ",  nextRS.msiSearch().get().id(), nextRS.msiSearch().get().searchSettings().id());
      rsIdByName.put(nextRS.name(), rsId);
    }

    return rsIdByName;
  }


  private Long storeResultFile(ResultSet nextRS, /*ResultSetsDataMapper rsMapper,*/ StorerContext storerContext,IRsStorer rsStorer) {
    String rsName = nextRS.name();
    logger.info("-- Storing  ResultSet {}", rsName);
    if(nextRS.peptideMatches() == null || nextRS.peptideMatches().length == 0)
      throw new RuntimeException(rsName+ " ResultSet has NO PeptideMatch");
    if(nextRS.proteinMatches() == null || nextRS.proteinMatches().length == 0)
      throw new RuntimeException(rsName+ " ResultSet has NO ProteinMatch");

    IPeaklistWriter pklWriter =	rsStorer.getOrBuildPeaklistWriter(storerContext);

    // Insert the peaklist information
    Peaklist pl = nextRS.msiSearch().get().peakList();
    long peakListId = pklWriter.insertPeaklist(pl, storerContext);
    pl.id_$eq(peakListId);

    logger.info("-- list MSQueries  ");
    // TO DO ?  Create Map Query -> List peptideMatch ?? Why VDS ?!
//	   	Map<MsQuery,List<PeptideMatch>> pepMatchesByQuery = null;
    Set<MsQuery> queries = Arrays.stream(nextRS.peptideMatches()).map( pm -> pm.msQuery()).collect(Collectors.toSet());
//    for(PeptideMatch pm : nextRS.peptideMatches()){
//      MsQuery q = pm.msQuery();
//      if(!queries.contains(q)){
//        queries.add(q);
//      }
//    }


    // Insert spectra contained in result file
    Map<Long, Spectrum> spectraById = m_spectraByIdByRsId.get(nextRS.id());
    DiaNNPeaklistContainer plContainer = new DiaNNPeaklistContainer(nextRS, spectraById);
    logger.info("Storing spectra...");
    pklWriter.insertSpectra(peakListId, plContainer, storerContext);

    //TODO : Compute PrettyRank computePrettyRanks(rs.peptideMatches, separated = true)
    logger.info("Storing ResultSet...");
    return rsStorer.storeResultSet(nextRS, JavaConverters.asScalaSet(queries).toList(), storerContext);
  }


  private Map<String, Object> extractParams(File reportLogFile) throws IOException {
    HashMap<String,Object> params = new HashMap<>();

    try (InputStream is = new FileInputStream(reportLogFile);
         BufferedReader reader = new BufferedReader(new InputStreamReader(is))){
      String rawLine = reader.readLine();
      boolean foundParams = false;

      while (rawLine != null && !foundParams) {
        if(rawLine.contains("diann.exe")){
          foundParams = true;
          String[] paramParts = rawLine.split("--");
          for(int i = 1; i< paramParts.length; i++) { //first value is diann.exe
            String nextParam = paramParts[i];
            String[] nextParamParts = nextParam.split(" ");
            String keyParam = nextParamParts[0];
            if(nextParamParts.length == 1) {
              params.put(keyParam, "");
            } else {
              String newVal = nextParamParts[1];
              if (nextParamParts.length > 2) {
                for (int indexVal = 2; indexVal < nextParamParts.length; indexVal++) {
                  newVal += nextParamParts[indexVal];
                }
              }

              if(params.containsKey(keyParam)){
                ArrayList<String> valAsList = new ArrayList<>();
                if (params.get(keyParam) instanceof ArrayList) {
                  valAsList = (ArrayList) params.get(keyParam);
                } else {
                  valAsList.add((String) params.get(keyParam));
                }
                valAsList.add(newVal);
                params.put(keyParam, valAsList);
              } else
                params.put(keyParam, newVal);
            }
          } //End go through parameters
        } else
          rawLine = reader.readLine();
      }

      return params;
    }
  }

  private ArrayList<String> getParamValueAsList(String key){
    if(!m_diaNNOptions.containsKey(key)){
      return null;
    }
    Object value = m_diaNNOptions.get(key);
    if(value instanceof ArrayList){
      return (ArrayList) value;
    } else {
      ArrayList<String> valAsList = new ArrayList<>();
      valAsList.add(value.toString());
      return valAsList;
    }
  }

  /**
   * Extract Fixed PTMs from diann Option
   *
   * @return List of DiaNN fixed PTMs
   */
  private ArrayList<String> getFixedModif(){
    ArrayList<String> ptms = new ArrayList<>();
    if(m_diaNNOptions.containsKey("fixed-mod")){
      ptms = getParamValueAsList("fixed-mod");
    }
    if(m_diaNNOptions.containsKey("unimod4")){
      ptms.add("unimod4");
    }
    return ptms;

  }

  /**
   * Extract Variable PTMs from diann Option
   *
   * @return List of DiaNN fixed PTMs
   */
  private ArrayList<String> getVarModif(){
    ArrayList<String> ptms = new ArrayList<>();
    if(m_diaNNOptions.containsKey("var-mod")){
      ptms = getParamValueAsList("var-mod");
    }
    return ptms;
  }


}
