package fr.proline.module.seq.dto;

import java.io.Serializable;

public class SequenceMatchWrapper implements Serializable {
	private static final long serialVersionUID = 1L;
	private final int start;
	private final int stop;
	private final Long peptidID;
	private final Long proteinMatchId;

	public SequenceMatchWrapper(int start, int stop, Long peptidID, Long proteinMatchId) {
		super();
		this.start = start;
		this.stop = stop;
		this.peptidID = peptidID;
		this.proteinMatchId = proteinMatchId;
	}

	public int getStart() {
		return start;
	}

	public int getStop() {
		return stop;
	}

	public Long getpeptidID() {
		return peptidID;
	}

	public Long getProteinMatchId() {
		return proteinMatchId;
	}
}
