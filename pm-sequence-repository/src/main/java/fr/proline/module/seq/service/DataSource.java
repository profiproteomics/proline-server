package fr.proline.module.seq.service;

import java.util.Date;
import java.util.List;
import java.util.Map;

import fr.proline.module.seq.dto.SEDbIdentifierWrapper;

public interface DataSource {

    public Date getLastModifiedTime();

    public Map<SEDbIdentifierWrapper, String> retrieveSequences(Map<String, List<SEDbIdentifierWrapper>> identByValues);

}