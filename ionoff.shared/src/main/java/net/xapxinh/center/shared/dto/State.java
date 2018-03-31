package net.xapxinh.center.shared.dto;

public enum State {
	
    nothing_special(0), opening (1), buffering (2), playing (3), paused (4), stopped (5), ended (6), error (7);
	
	private int value;
	
	private State(int value) {
		this.value = value;
	}
	
	public int getValue() {
		return this.value;
	}
}