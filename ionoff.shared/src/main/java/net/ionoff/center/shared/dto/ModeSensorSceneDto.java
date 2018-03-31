package net.ionoff.center.shared.dto;

import java.util.ArrayList;
import java.util.List;

public class ModeSensorSceneDto extends BaseDto {

	private static final long serialVersionUID = 1L;

	private Long modeSensorId;
	private Boolean detected;
	private Long zoneId;
	private String zoneName;
	private Long sceneId;
	private String sceneName;
	private List<String> sceneNameIds;

	public ModeSensorSceneDto() {
		sceneNameIds = new ArrayList<>();
	}

	public Boolean getDetected() {
		return detected;
	}
	public void setDetected(Boolean detected) {
		this.detected = detected;
	}

	public Long getModeSensorId() {
		return modeSensorId;
	}
	public void setModeSensorId(Long modeSensorId) {
		this.modeSensorId = modeSensorId;
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
}
