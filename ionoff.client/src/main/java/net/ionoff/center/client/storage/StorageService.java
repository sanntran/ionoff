package net.ionoff.center.client.storage;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.core.client.GWT;
import com.google.gwt.json.client.JSONArray;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONParser;
import com.google.gwt.json.client.JSONString;
import com.google.gwt.json.client.JSONValue;
import com.google.gwt.storage.client.Storage;

import net.ionoff.center.shared.cookie.Kookie;
import net.ionoff.center.shared.dto.UserDto;

public class StorageService {

	private static StorageService instance;
	
	private Kookie cookie;
	private List<ApiServer> servers;
	
	private Storage storage;
	private final String ICOOKIE = "IKookie";
	private final String ISERVER = "IServer";
	
	
	public void loadStorage() {
		storage = Storage.getLocalStorageIfSupported();
		if (storage != null) {
			String serversJson = storage.getItem(ISERVER);
			if (serversJson != null) {
				servers = toServers(serversJson);
			}
			else {
				initDefaultServer();
			}
			String cookieJson = storage.getItem(ICOOKIE);
			cookie = toKookie(cookieJson);
		}
		else {
			initDefaultServer();
			cookie = new Kookie();
		}
	}

	private List<ApiServer> toServers(String serversJson) {
		servers = new ArrayList<>();
		try {
			JSONArray jsonArr = (JSONArray) JSONParser.parseStrict(serversJson);
			for (int i = 0; i < jsonArr.size(); i++) {
				JSONObject jsObj = (JSONObject) jsonArr.get(i);
				ApiServer server = new ApiServer();
				server.setHost(jsonValueToString(jsObj.get(ApiServer.HOST)));
				server.setEnabled(jsonValueToBoolean(jsObj.get(ApiServer.ENABLED)));
				servers.add(server);
			}
		}
		catch (Exception e) {
			GWT.log("Error when create servers fron json: " + serversJson, e);
			GWT.log(e.getMessage(), e);
		}
		return servers;
	}

	private void initDefaultServer() {
		servers = new ArrayList<>();
		servers.add(new ApiServer("cloud.ionoff.net", true));
		servers.add(new ApiServer("192.168.1.252:8008", false));
	}

	public static String toString(Kookie kookie) {
		if (kookie == null) {
			return null;
		}
		JSONObject jsonObj = new JSONObject();
		String userName = null;
		if (kookie.getUser() != null) {
			userName = kookie.getUser().getName();
		}
		putJsonValue(jsonObj, Kookie.USER_NAME, String.valueOf(userName));
		putJsonValue(jsonObj, Kookie.JWT_TOKEN, kookie.getJwtToken());
		putJsonValue(jsonObj, Kookie.PROJECT_ID, String.valueOf(kookie.getProjectId()));
		putJsonValue(jsonObj, Kookie.PROJECT_ID, String.valueOf(kookie.getProjectId()));
		putJsonValue(jsonObj, Kookie.HISTORY_TOKEN, String.valueOf(kookie.getHistoryToken()));
		return jsonObj.toString();
	}
	
	public static String toString(List<ApiServer> servers) {
		if (servers == null) {
			return null;
		}
		JSONArray jsonArr = new JSONArray();
		for (int i = 0; i < servers.size(); i ++) {
			JSONObject jsonObj = new JSONObject();
			putJsonValue(jsonObj, ApiServer.HOST, String.valueOf(servers.get(i).getHost()));
			putJsonValue(jsonObj, ApiServer.ENABLED, String.valueOf(servers.get(i).isEnabled()));
			jsonArr.set(i, jsonObj);
		}
		return jsonArr.toString();
	}
	
	private static void putJsonValue(JSONObject jsonObj, String key, String value) {
		if (value != null) {
			jsonObj.put(key, new JSONString(value));
		}
	}

	public static Kookie toKookie(String json) {
		Kookie kookie = new Kookie();
		try {
			JSONObject jsonObj = (JSONObject) JSONParser.parseStrict(json);
			UserDto user = new UserDto();
			user.setName(jsonValueToString(jsonObj.get(Kookie.USER_NAME)));
			kookie.setUser(user);
			kookie.setProjectId(jsonValueToLong(jsonObj.get(Kookie.PROJECT_ID)));
			kookie.setJwtToken(jsonValueToString(jsonObj.get(Kookie.JWT_TOKEN)));
			kookie.setHistoryToken(jsonValueToString(jsonObj.get(Kookie.HISTORY_TOKEN)));
			return kookie;
		}
		catch (Exception e) {
			GWT.log("Error when create cookie fron json: " + json, e);
			return kookie;
		}
	}
	
	private static boolean jsonValueToBoolean(JSONValue jsonValue) {
		if (jsonValue != null) {
			return Boolean.parseBoolean(((JSONString)jsonValue).stringValue());
		}
		return false;
	}
	
	private static String jsonValueToString(JSONValue jsonValue) {
		if (jsonValue != null) {
			return ((JSONString)jsonValue).stringValue();
		}
		return null;
	}
	
	private static Long jsonValueToLong(JSONValue jsonValue) {
		if (jsonValue != null) {
			return Long.parseLong(((JSONString)jsonValue).stringValue());
		}
		return null;
	}
	
	public Kookie getCookie() {
		return cookie;
	}

	public void setCookie(Kookie cookie) {
		this.cookie.setUser(cookie.getUser());
		this.cookie.setProjectId(cookie.getProjectId());
		this.cookie.setJwtToken(cookie.getJwtToken());
		//this.cookie.setHistoryToken(cookie.getHistoryToken());
	}

	public void writeCookie() {
		final String cookieValue = toString(cookie);
		if (cookieValue != null && storage != null) {
			storage.setItem(ICOOKIE, cookieValue);
		}
	}

	public void detroyCookie() {
		cookie = new Kookie();
		if (storage != null) {
			storage.removeItem(ICOOKIE);
		}
	}

	public long getUserId() {
		return cookie.getUser().getId();
	}


	public List<ApiServer> getServers() {
		return this.servers;
	}
	
	public ApiServer getEnabledApiServer() {
		for (ApiServer server : servers) {
			if (server.isEnabled()) {
				return server;
			}
		}
		return null;
	}

	public void saveServers() {
		if (this.servers != null && servers.size() == 2 && storage != null) {
			String serversJson = toString(servers);
			storage.setItem(ISERVER, serversJson);
		}
	}

	public static StorageService getInstance() {
		if (instance == null) {
			instance = new StorageService();
		}
		return instance;
	}
}
