package fr.proline.module.seq.service;

import java.io.IOException;
import java.util.Date;
import java.util.List;
import java.util.Map;

import fr.proline.module.seq.dto.DDatabankProtein;

public interface DataSource {

	public Date getLastModifiedTime();

	public Map<DDatabankProtein, String> retrieveSequences(Map<String, List<DDatabankProtein>> identByValues) throws IOException;

}