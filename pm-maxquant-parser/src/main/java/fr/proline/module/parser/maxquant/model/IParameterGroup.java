package fr.proline.module.parser.maxquant.model;

import java.util.List;

public interface IParameterGroup {

    Integer getMaxCharge();

    void setMaxCharge(Integer maxCharge);

    Integer getMaxMissedCleavages();

    void setMaxMissedCleavages(Integer maxMissedCleavages);

    List<String> getEnzymes();

    void setEnzymes(List<String> enzymes);

    List<String> getVariableModifications();

    void setVariableModifications(List<String> variableModifications);

    Float getMainSearchTol();

    void setMainSearchTol(Float mainSearchTol);

    Boolean getSearchTolInPpm();

    void setSearchTolInPpm(Boolean searchTolInPpm);
}
