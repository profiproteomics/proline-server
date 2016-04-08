package fr.proline.module.parser.maxquant.model;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement(name="msmsParams")
@XmlAccessorType(XmlAccessType.PROPERTY)
public class MsMsParameters {

	@XmlElement(name="MatchTolerance")
	private Integer m_msmsTolerance;
	

	@XmlAttribute(name="Name")
	private String m_instrumTypeName;
	
	@XmlAttribute(name="MatchToleranceInPpm")
	private Boolean m_msmsToleranceInPpm;

		
	public Boolean getMsmsToleranceInPpm() {
		return m_msmsToleranceInPpm;
	}

	public void setMsmsToleranceInPpm(Boolean msmsToleranceInPpm) {
		this.m_msmsToleranceInPpm = msmsToleranceInPpm;
	}

	public String getInstrumTypeName() {
		return m_instrumTypeName;
	}

	public void setInstrumTypeName(String instrumTypeName) {
		this.m_instrumTypeName = instrumTypeName;
	}

	public Integer getMatchTolerance() {
		return m_msmsTolerance;
	}
	
	public void setMatchTolerance(Integer msmsTolerance) {
		this.m_msmsTolerance = msmsTolerance;
	}


	@Override
	public String toString() {
		return "MSMS Params "+m_instrumTypeName+" [MatchTolerance=" + m_msmsTolerance + "]";
	}
	
	
}
