package net.ionoff.center.shared.dto.player;

import java.util.HashMap;

class ParamMap extends HashMap<String, String> {
	
	private static final long serialVersionUID = 1L;

	public ParamMap putParam(String param, String value) {
		put(param, value);
		return this;
	}
}
