package net.ionoff.center.shared.dto;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonTypeName;

@JsonTypeName( "ApplianceDto" ) 
public class ApplianceDto extends DeviceDto {

	private static final long serialVersionUID = 1L;

	private List<RelayDto> relays;
	
	public String getClazz() {
		return "ApplianceDto";
	}
	
	@Override
	public String getStyle() {
		return "appliance";
	}

	public ApplianceDto() {
		relays = new ArrayList<>();
	}

	public List<RelayDto> getRelays() {
		return relays;
	}

	public void setRelays(List<RelayDto> relays) {
		Collections.sort(relays);
		this.relays = relays;
	}

	public boolean hasRelay() {
		return relays != null && relays.size() > 0;
	}

	public boolean hasOneRelay() {
		return relays != null && relays.size() == 1;
	}

	public boolean hasManyRelays() {
		return relays != null && relays.size() > 1;
	}
}
