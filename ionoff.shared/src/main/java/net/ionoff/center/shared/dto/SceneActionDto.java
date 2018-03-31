package net.ionoff.center.shared.dto;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonSubTypes.Type;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonTypeInfo.As;
import com.fasterxml.jackson.annotation.JsonTypeInfo.Id;

@JsonSubTypes({
	@Type(value= ScenePlayerActionDto.class, name="ScenePlayerActionDto"), 
	@Type(value= SceneRelayActionDto.class, name="SceneRelayActionDto")})
@JsonTypeInfo(use=Id.NAME, include=As.PROPERTY, property="clazz")
public class SceneActionDto extends BaseDto {

	private static final long serialVersionUID = 1L;
	public static String NONE = "None";
	
	private String action;
	private Long sceneId;
	private String sceneName;

	public String getAction() {
		return action;
	}
	public void setAction(String action) {
		this.action = action;
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
}
