package net.ionoff.center.shared.dto;

import java.util.List;

public class RelayGroupDto extends BaseDto {
	
	private static final long serialVersionUID = 1L;

	private List<RelayDto> relays;
	
	public List<RelayDto> getRelays() {
		return relays;
	}

	public void setRelays(List<RelayDto> relays) {
		this.relays = relays;
	}
	
}