package net.ionoff.center.shared.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;

import net.ionoff.center.shared.entity.RelayAction;

@JsonTypeName("ScheduleRelayActionDto") 
public class ScheduleRelayActionDto extends ScheduleActionDto implements RelayAction  {

	private static final long serialVersionUID = 1L;

	private Long relayId;
	private String relayName;

	public String getClazz() {
		return "ScheduleRelayActionDto";
	}
	
	public Long getRelayId() {
		return relayId;
	}
	public void setRelayId(Long relayId) {
		this.relayId = relayId;
	}

	public String getRelayName() {
		return relayName;
	}
	public void setRelayName(String relayName) {
		this.relayName = relayName;
	}

}
