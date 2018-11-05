package net.ionoff.center.shared.dto;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonTypeName;

@JsonTypeName( "RelayLoadDto" )
public class RelayLoadDto extends DeviceDto {

	private static final long serialVersionUID = 1L;

	private List<RelayDto> relays;
	
	public String getClazz() {
		return "RelayLoadDto";
	}

	public RelayLoadDto() {
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

	@Override
	public String styleName() {
		return "radio_button_unchecked";
	}
}
