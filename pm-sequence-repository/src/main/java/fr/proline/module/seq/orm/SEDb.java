package fr.proline.module.seq.orm;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.NamedQuery;
import javax.persistence.Table;

@Entity
@Table(name = "se_db")
@NamedQuery(name = "findSEDbByName", query = "SELECT sd from fr.proline.module.seq.orm.SEDb sd where sd.name = :name")
public class SEDb implements Serializable {

    public enum Alphabet {
	AA, DNA
    }

    private static final long serialVersionUID = 1L;

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private long id;

    @Column(nullable = false)
    private String name;

    // Length = 3
    @Column(nullable = false)
    @Enumerated(EnumType.STRING)
    private Alphabet alphabet;

    @ManyToOne
    @JoinColumn(name = "parsing_rule_id")
    private ParsingRule parsingRule;

    @ManyToOne
    @JoinColumn(name = "repository_id")
    private Repository repository;

    private void setId(final long pId) {
	id = pId;
    }

    public long getId() {
	return id;
    }

    public void setName(final String pName) {
	name = pName;
    }

    public String getName() {
	return name;
    }

    public void setAlphabet(final Alphabet pAlphabet) {
	alphabet = pAlphabet;
    }

    public Alphabet getAlphabet() {
	return alphabet;
    }

    public void setParsingRule(final ParsingRule pParsingRule) {
	parsingRule = pParsingRule;
    }

    public ParsingRule getParsingRule() {
	return parsingRule;
    }

    public void setRepository(final Repository pRepository) {
	repository = pRepository;
    }

    public Repository getRepository() {
	return repository;
    }

}
