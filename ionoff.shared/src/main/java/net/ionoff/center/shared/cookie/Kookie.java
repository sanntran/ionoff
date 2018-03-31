package net.ionoff.center.shared.cookie;

import java.io.Serializable;

import net.ionoff.center.shared.dto.UserDto;

public class Kookie implements Serializable {

	private static final long serialVersionUID = 1L;
	public static final String USER_NAME = "username";
	public static final String JWT_TOKEN = "jwtToken";
	public static final String PROJECT_ID = "projectId";
	public static final String HISTORY_TOKEN = "historyToken";
	
	private UserDto user;
	private String jwtToken;
	private Long projectId;
	private String historyToken;

	public String getJwtToken() {
		return jwtToken;
	}

	public void setJwtToken(String jwtToken) {
		this.jwtToken = jwtToken;
	}
	
	public UserDto getUser() {
		return user;
	}

	public void setUser(UserDto user) {
		this.user = user;
	}

	public void setProjectId(Long projectId) {
		this.projectId = projectId;
	}
	
	public Long getProjectId() {
		return projectId;
	}

	public String getHistoryToken() {
		return historyToken;
	}

	public void setHistoryToken(String historyToken) {
		this.historyToken = historyToken;
	}
}
