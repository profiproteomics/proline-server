package fr.proline.module.parser.maxquant.model.v1_4;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import fr.proline.module.parser.maxquant.model.IMsMsParameters;

@XmlRootElement(name="msmsParams")
@XmlAccessorType(XmlAccessType.PROPERTY)
public class MsMsParameters implements IMsMsParameters {
	
	
	@XmlElementWrapper(name = "Tolerance")
	@XmlElements ( value = { @XmlElement(name="Value", required = true, type=Float.class),@XmlElement(required = true, name="Unit", type=String.class) })
	private List<Object> m_msmsTolerance;
	

	@XmlAttribute(name="Name")
	private String m_instrumTypeName;
//	
//	@XmlElementWrapper(name = "Tolerance")
//	@XmlElement(nillable = false, required = true, name="Unit")	
//	private String m_msmsToleranceUnit;

		
	@Override
	public Boolean getMsmsToleranceInPpm() {
		return m_msmsTolerance.get(1).toString().equals("Ppm");		
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
		return (Float)m_msmsTolerance.get(0);
	}
	
	public void setMatchTolerance(Float msmsTolerance) {
		this.m_msmsTolerance.set(0, msmsTolerance);
	}


	@Override
	public String toString() {
		return "MSMS Params "+m_instrumTypeName+" [MatchTolerance=" + m_msmsTolerance + "]";
	}
	
		
}
