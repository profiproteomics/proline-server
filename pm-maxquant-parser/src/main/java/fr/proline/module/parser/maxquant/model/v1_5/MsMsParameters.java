package fr.proline.module.parser.maxquant.model.v1_5;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import fr.proline.module.parser.maxquant.model.IMsMsParameters;

@XmlRootElement(name="msmsParams")
@XmlAccessorType(XmlAccessType.PROPERTY)
public class MsMsParameters implements IMsMsParameters {

	@XmlElement(name="MatchTolerance")
	private Float m_msmsTolerance;
	

	@XmlAttribute(name="Name")
	private String m_instrumTypeName;
	
	@XmlAttribute(name="MatchToleranceInPpm")
	private Boolean m_msmsToleranceInPpm;

		
	@Override
	public Boolean getMsmsToleranceInPpm() {
		return m_msmsToleranceInPpm;
	}

	public void setMsmsToleranceInPpm(Boolean msmsToleranceInPpm) {
		this.m_msmsToleranceInPpm = msmsToleranceInPpm;
	}

	@Override
	public String getInstrumTypeName() {
		return m_instrumTypeName;
	}

	public void setInstrumTypeName(String instrumTypeName) {
		this.m_instrumTypeName = instrumTypeName;
	}

	@Override
	public Float getMatchTolerance() {
		return m_msmsTolerance;
	}
	
	public void setMatchTolerance(Float msmsTolerance) {
		this.m_msmsTolerance = msmsTolerance;
	}


	@Override
	public String toString() {
		return "MSMS Params "+m_instrumTypeName+" [MatchTolerance=" + m_msmsTolerance + "]";
	}
	
	
}
