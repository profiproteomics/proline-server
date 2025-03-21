package fr.proline.module.parser.maxquant.model.v1_5;


import fr.proline.module.parser.maxquant.model.IParameterGroup;

import javax.xml.bind.annotation.*;
import java.util.List;

@XmlRootElement(name="parameterGroup")
@XmlAccessorType(XmlAccessType.PROPERTY)
public class ParameterGroup implements IParameterGroup {

    @XmlElement(name = "maxCharge")
    private Integer m_maxCharge;

    @XmlElement(name = "maxMissedCleavages", type = Integer.class)
    private Integer m_maxMissedCleavages;


    @XmlElementWrapper(name = "enzymes")
    @XmlElement(name = "string")
    private List<String> m_enzymes;

    @XmlElementWrapper(name = "variableModifications")
    @XmlElement(name = "string")
    private List<String> m_variableModifications;


    @XmlElement(name = "mainSearchTol", type = Float.class)
    private Float m_mainSearchTol;

    @XmlElement(name = "searchTolInPpm", type = Boolean.class)
    private Boolean m_searchTolInPpm;

    public Integer getMaxCharge() {
        return m_maxCharge;
    }

    public void setMaxCharge(Integer maxCharge) {
        this.m_maxCharge = maxCharge;
    }

    public Integer getMaxMissedCleavages() {
        return m_maxMissedCleavages;
    }

    public void setMaxMissedCleavages(Integer maxMissedCleavages) {
        this.m_maxMissedCleavages = maxMissedCleavages;
    }

    public List<String> getEnzymes() {
        return m_enzymes;
    }

    public void setEnzymes(List<String> enzymes) {
        this.m_enzymes = enzymes;
    }

    public List<String> getVariableModifications() {
        return m_variableModifications;
    }

    public void setVariableModifications(List<String> variableModifications) {
        this.m_variableModifications = variableModifications;
    }

    public Float getMainSearchTol() {
        return m_mainSearchTol;
    }

    public void setMainSearchTol(Float mainSearchTol) {
        this.m_mainSearchTol = mainSearchTol;
    }

    public Boolean getSearchTolInPpm() {
        if (m_searchTolInPpm == null) //Not in param file, version 1.4 (?) always true
            return true;
        return m_searchTolInPpm;
    }

    public void setSearchTolInPpm(Boolean searchTolInPpm) {
        this.m_searchTolInPpm = searchTolInPpm;
    }

    @Override
    public String toString() {
        return "ParameterGroup [maxCharge=" + m_maxCharge + ", maxMissedCleavages=" + m_maxMissedCleavages + ", enzymes=" + m_enzymes + ", variableModifications="
                + m_variableModifications + ", mainSearchTol=" + m_mainSearchTol + ", searchTolInPpm=" + m_searchTolInPpm + "]";
    }

}