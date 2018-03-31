package net.ionoff.center.shared.dto;

import java.util.ArrayList;
import java.util.List;

public class ModeSceneDto extends BaseDto {

	private static final long serialVersionUID = 1L;

	private Long modeId;
	private Long zoneId;
	private String zoneName;
	private Long sceneId;
	private String sceneName;
	private List<String> sceneNameIds;

	public ModeSceneDto() {
		sceneNameIds = new ArrayList<>();
	}

	public Long getModeId() {
		return modeId;
	}
	public void setModeId(Long modeId) {
		this.modeId = modeId;
	}

	public Long getZoneId() {
		return zoneId;
	}
	public void setZoneId(Long zoneId) {
		this.zoneId = zoneId;
	}

	public String getZoneName() {
		return zoneName;
	}
	public void setZoneName(String zoneName) {
		this.zoneName = zoneName;
	}

	public Long getSceneId() {
		return sceneId;
	}
	public void setSceneId(Long sceneId) {
		this.sceneId = sceneId;
	}

	public String getSceneName() {
		return sceneName;
	}
	public void setSceneName(String sceneName) {
		this.sceneName = sceneName;
	}

	public List<String> getSceneNameIds() {
		return sceneNameIds;
	}
	public void setSceneNameIds(List<String> sceneNameIds) {
		this.sceneNameIds = sceneNameIds;
	}
	
	@Override
	public String toString() {
		return super.toString() + ", Mode ID: " + modeId + ", Scene ID: " + sceneId; 
	}
}
