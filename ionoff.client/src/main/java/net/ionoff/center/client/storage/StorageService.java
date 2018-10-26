package net.ionoff.center.client.storage;

import com.github.nmorel.gwtjackson.client.ObjectMapper;
import com.google.gwt.core.client.GWT;
import com.google.gwt.json.client.JSONArray;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONParser;
import com.google.gwt.json.client.JSONString;
import com.google.gwt.json.client.JSONValue;
import com.google.gwt.storage.client.Storage;
import net.ionoff.center.shared.cookie.Kookie;
import net.ionoff.center.shared.cookie.Service;

import java.util.ArrayList;
import java.util.List;

public class StorageService {

	private static StorageService instance;

	public interface KookieMapper extends ObjectMapper<Kookie> {}
	public static KookieMapper kookieMapper = GWT.create( KookieMapper.class );

	private Kookie cookie;
	private List<Service> services;
	
	private Storage storage;

	private final String KOOKIE = "kookie";
	private final String SERVICES = "services";
	private final String PROJECTS = "projects";
	private final String ZONES = "zones";
	private final String DEVICES = "devices";
	private final String MODES = "modes";
	private final String SCENSES = "scenes";

	public void loadStorage() {
		storage = Storage.getLocalStorageIfSupported();
		if (storage != null) {
			String servicesJson = storage.getItem(SERVICES);
			if (servicesJson != null) {
				services = toServices(servicesJson);
			}
			else {
				initDefaultServer();
			}
			
			String cookieJson = storage.getItem(KOOKIE);
			if (cookieJson != null) {
				cookie = toKookie(cookieJson);
			}
			else {
				cookie = new Kookie();
			}
		}
		else {
			initDefaultServer();
			cookie = new Kookie();
		}
	}

	private List<Service> toServices(String serversJson) {
		services = new ArrayList<>();
		try {
			JSONArray jsonArr = (JSONArray) JSONParser.parseStrict(serversJson);
			for (int i = 0; i < jsonArr.size(); i++) {
				JSONObject jsObj = (JSONObject) jsonArr.get(i);
				Service server = new Service();
				server.setHost(jsonValueToString(jsObj.get(Service.HOST)));
				server.setEnabled(jsonValueToBoolean(jsObj.get(Service.ENABLED)));
				services.add(server);
			}
		}
		catch (Exception e) {
			GWT.log("Error when create services fron json: " + serversJson, e);
			GWT.log(e.getMessage(), e);
		}
		return services;
	}

	private void initDefaultServer() {
		services = new ArrayList<>();
		services.add(new Service("cloud.ionoff.net", true));
		services.add(new Service("192.168.1.252:8008", false));
	}

	public static String toString(Kookie kookie) {
		if (kookie == null) {
			return null;
		}
		String json = kookieMapper.write(kookie);
		return json;
	}
	
	public static String toString(List<Service> servers) {
		if (servers == null) {
			return null;
		}
		JSONArray jsonArr = new JSONArray();
		for (int i = 0; i < servers.size(); i ++) {
			JSONObject jsonObj = new JSONObject();
			putJsonValue(jsonObj, Service.HOST, String.valueOf(servers.get(i).getHost()));
			putJsonValue(jsonObj, Service.ENABLED, String.valueOf(servers.get(i).isEnabled()));
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
		Kookie cookie = kookieMapper.read( json );
		return cookie;
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
			storage.setItem(KOOKIE, cookieValue);
		}
	}

	public void detroyCookie() {
		cookie = new Kookie();
		if (storage != null) {
			storage.removeItem(KOOKIE);
		}
	}

	public long getUserId() {
		return cookie.getUser().getId();
	}


	public List<Service> getServices() {
		return this.services;
	}
	
	public Service getEnabledApiServer() {
		for (Service server : services) {
			if (server.isEnabled()) {
				return server;
			}
		}
		return null;
	}

	public void saveServers() {
		if (this.services != null && services.size() == 2 && storage != null) {
			String serversJson = toString(services);
			storage.setItem(SERVICES, serversJson);
		}
	}

	public static StorageService getInstance() {
		if (instance == null) {
			instance = new StorageService();
		}
		return instance;
	}

}
