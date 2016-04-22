package fr.proline.module.parser.maxquant.model;

import java.util.ArrayList;
import java.util.List;


public interface IMaxQuantParams {

	String getName();

	String getVersion();

	Boolean getIncludeContaminants();

	List<String> getFastaFiles();

	List<String> getFilePaths();

	List<String> getFixedModifications();

	List<String> getExperiments();

	ArrayList<ParameterGroup> getParameters();

	ArrayList<? extends IMsMsParameters> getMsMsParameters();

}