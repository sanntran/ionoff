package net.ionoff.center.shared.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;

import net.ionoff.center.shared.entity.RelayAction;

@JsonTypeName("SceneRelayActionDto") 
public class SceneRelayActionDto extends SceneActionDto implements RelayAction {

	private static final long serialVersionUID = 1L;
	
	private Long relayId;
	private String relayName;
	private String relayType;
	
	
	public String getClazz() {
		return "SceneRelayActionDto";
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
	
	public String getRelayType() {
		return relayType;
	}
	public void setRelayType(String relayType) {
		this.relayType = relayType;
	}
}
