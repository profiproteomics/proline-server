package fr.proline.module.parser.maxquant.model.v1_5;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;

import fr.proline.module.parser.maxquant.model.IMaxQuantParams;
import fr.proline.module.parser.maxquant.model.IMsMsParameters;
import fr.proline.module.parser.maxquant.model.ParameterGroup;

@XmlRootElement(name = "MaxQuantParams")
@XmlAccessorType(XmlAccessType.PROPERTY)
public class MaxQuantParams implements IMaxQuantParams  {

	
	@XmlElement(name="name")
	private String m_name;
	
	@XmlElement(required = true, name="maxQuantVersion")
	private String m_maxQuantVersion;
	
	@XmlElementWrapper(name = "fastaFiles")
	@XmlElement(nillable = false, required = true, name="string")
	private List<String> m_fastaFiles;
	
	@XmlElementWrapper(name = "filePaths")
	@XmlElement(nillable = false, required = true, name="string")
	private List<String> m_filePaths;

	@XmlElementWrapper(name = "fixedModifications")
	@XmlElement(nillable = false, required = true, name="string")
	private List<String> m_fixedModifications;

	@XmlElementWrapper(name = "experiments")
	@XmlElement(nillable = false, required = true,  name="string")
	private List<String> m_experiments;

	@XmlElement(name="includeContaminants", type=Boolean.class)
	private Boolean m_includeContaminants;
	
	// XmLElementWrapper generates a wrapper element around XML representation
	@XmlElementWrapper(name = "parameterGroups")
	// XmlElement sets the name of the entities
	@XmlElement(name = "parameterGroup")
	private ArrayList<ParameterGroup> m_parameters;

	// XmLElementWrapper generates a wrapper element around XML representation
	@XmlElementWrapper(name = "msmsParamsArray")
	// XmlElement sets the name of the entities
	@XmlElement(name = "msmsParams")
	private ArrayList<MsMsParameters> m_msmsParams;

	
	@Override
	public String getName() {
		return m_name;
	}

	public void setName(String name) {
		this.m_name = name;
	}


	@Override
	public String getVersion() {
		return m_maxQuantVersion;
	}

	public void setVersion(String maxQuantVersion) {
		this.m_maxQuantVersion = maxQuantVersion;
	}

	@Override
	public Boolean getIncludeContaminants() {
		return m_includeContaminants;
	}

	public void setIncludeContaminants(Boolean includeContaminants) {
		this.m_includeContaminants = includeContaminants;
	}

	
	
	@Override
	public List<String> getFastaFiles() {
		return m_fastaFiles;
	}

	public void setFastaFiles(List<String> fastaFiles) {
		this.m_fastaFiles = fastaFiles;
	}

	@Override
	public List<String> getFilePaths() {
		return m_filePaths;
	}

	public void setFilePaths(List<String> filePaths) {
		this.m_filePaths = filePaths;
	}

	
	@Override
	public List<String> getFixedModifications() {
		return m_fixedModifications;
	}

	public void setFixedModifications(List<String> fixedModifications) {
		this.m_fixedModifications = fixedModifications;
	}

	@Override
	public List<String> getExperiments() {
		return m_experiments;
	}

	public void setExperiments(List<String> experiments) {
		this.m_experiments = experiments;
	}

	@Override
	public ArrayList<ParameterGroup> getParameters() {
		return m_parameters;
	}

	public void setParameters(ArrayList<ParameterGroup> parameters) {
		this.m_parameters = parameters;
	}

	@Override
	public ArrayList<? extends IMsMsParameters> getMsMsParameters() {
		return  m_msmsParams;
	}

	public void setMsMsParameters(ArrayList<MsMsParameters> msmsParameters) {
		this.m_msmsParams = msmsParameters;
	}

	
}
