package net.ionoff.center.client.utils;

import com.google.gwt.user.client.History;

public final class AppToken extends TokenUtil {

	private AppToken() {
		// does nothing
	}
	public static final String LIST = "list";
	public static final String TABLE = "table";
	
	public static final String LOGIN = "login";
	public static final String PROJECT = "project_";
	public static final String ZONE = "zone_";
	public static final String DASHBOARD = "dashboard";
	public static final String DEVICES = "devices";
	public static final String SCENES = "scenes";
	public static final String SCHEDULES = "schedules";
	public static final String MODES = "modes";
	public static final String DEVICE = "device_";
	public static final String NULL = "null";
	public static final String LORD = "lord";
	public static final String PROJECTS = "projects";
	public static final String DLM = "/";
	public static final String AREAS = "areas";
	public static final String ZONES = "zones";
	public static final String RELAYS = "relays";
	public static final String RELAY_DRIVERS = "relaydrivers";
	public static final String SENSORS = "sensors";
	public static final String USERS = "users";
	public static final String SYSTEM = "system";
	
	
	public static Long getProjectIdLong() {
		try {
			return Long.parseLong(getTokenId(History.getToken(), PROJECT));
		} catch (Exception e) {
			return null;
		}
	}
	
	public static String getProjectId() {
		return getTokenId(History.getToken(), PROJECT);
	}

	public static String getProjectId(String token) {
		return getTokenId(token, PROJECT);
	}

	public static String getTokenId(String token, String itemName) {
		final String[] tokenItems = getTokenItems(token);
		for (final String item : tokenItems) {
			if (item.startsWith(itemName)) {
				return item.replaceFirst(itemName, "");
			}
		}
		return "";
	}

	public static boolean isValidToken(String token) {
		if (token == null || token.isEmpty()) {
			return false;
		}
		String[] tokenItems = token.split(TOKEN_DLM);
		if (!hasTokenItem(tokenItems, PROJECT)) {
			return false;
		}
		
		if (hasTokenItem(tokenItems, LOGIN) 
				|| hasTokenItem(tokenItems, DASHBOARD)
				|| hasTokenItem(tokenItems, PROJECTS) 
				|| hasTokenItem(tokenItems, PROJECT)
				|| hasTokenItem(tokenItems, AREAS)
				|| hasTokenItem(tokenItems, ZONES)
				|| hasTokenItem(tokenItems, ZONE)
				|| hasTokenItem(tokenItems, DEVICES)
				|| hasTokenItem(tokenItems, RELAY_DRIVERS)
				|| hasTokenItem(tokenItems, SCENES) 
				|| hasTokenItem(tokenItems, MODES)
				|| hasTokenItem(tokenItems, SCHEDULES)
				|| hasTokenItem(tokenItems, USERS)
				|| hasTokenItem(tokenItems, RELAYS)
				|| hasTokenItem(tokenItems, SENSORS)
				|| hasTokenItem(tokenItems, DEVICE)) {
			
			final String projectId = getProjectId(token);
			if (!isLongNumber(projectId)) {
				return false;
			}
			if (hasTokenItem(token, ZONE)) {
				final String areaId = getTokenId(token, ZONE);
				if (!isLongNumber(areaId)) {
					return false;
				}
			}
			if (hasTokenItem(token, DEVICE)) {
				final String deviceId = getTokenId(token, DEVICE);
				if (!isLongNumber(deviceId)) {
					return false;
				}
			}
			return true;
		}
		return false;
	}

	public static boolean isLongNumber(String num) {
		try {
			Long.parseLong(num);
		}
		catch (final NumberFormatException nfe) {
			return false;
		}
		return true;
	}

	public static boolean isZoneChanged() {
		final String token = getToken();
		final String prevToken = getPrevToken();
		if (prevToken == null || prevToken.isEmpty()) {
			return true;
		}
		// token has zone OR prev token has zone
		if (hasTokenItem(token, ZONE) != hasTokenItem(prevToken, ZONE)) {
			return true;
		}

		// token has zone AND prev token has zone
		if (hasTokenItem(token, ZONE)) {
			return !getTokenId(token, ZONE).equals(getTokenId(prevToken, ZONE));
		}

		// token has no zone AND prev token has no zone
		return false;
	}
	
	public static String changeZoneId(Long newZoneId) {

		String token = History.getToken();
		token = removeTokenDevice(token);

		if (!hasTokenItem(ZONE)) {
			if (newZoneId == null) {
				return token;
			}
			String newToken = token + TOKEN_DLM + ZONE + newZoneId;
			// No table view in zone
			return newToken.replaceAll("/" + TABLE, "");
		}
		String newToken = "";
		final String[] tokenItems = getTokenItems(token);
		for (int i = 0; i < tokenItems.length; i++) {
			if (tokenItems[i].startsWith(ZONE)) {
				if (newZoneId != null) {
					if (i == 0) {
						newToken = ZONE + newZoneId;
					}
					else {
						newToken = newToken + TOKEN_DLM + ZONE + newZoneId;
					}
				}
			}
			else {
				if (i != 0) {
					newToken = newToken + TOKEN_DLM + tokenItems[i];
				}
				else {
					newToken = newToken + tokenItems[i];
				}
			}
		}
		// No table view in zone
		return newToken.replaceAll("/" + TABLE, "");
	}

	public static String newDeviceToken() {
		final String token = History.getToken();
		return replaceTokenItem(token, DEVICE);
	}

	public static String newSceneToken() {
		final String token = History.getToken();
		return replaceTokenItem(token, SCENES);
	}
	
	public static String newScheduleToken() {
		final String token = History.getToken();
		return replaceTokenItem(token, SCHEDULES);
		
	}
	
	public static String newRelayDriverToken() {
		final String token = History.getToken();
		return replaceTokenItem(token, RELAY_DRIVERS);
	}
	

	public static String newModeToken() {
		final String token = History.getToken();
		return replaceTokenItem(token, MODES);
	}

	private static String replaceTokenItem(String token, String newItem) {
		token = token.replace(DLM + TABLE, "");
		if (hasTokenItem(DEVICE)) {
			final String deviceId = getTokenId(token, DEVICE);
			if (token.startsWith(DEVICE)) {
				return token.replace((DEVICE + deviceId), newItem);
			}
			else {
				return token.replace((DEVICE + deviceId), newItem);
			}
		}
		if (hasTokenItem(DASHBOARD)) {
			return token.replace(DASHBOARD, newItem);
		}
		if (hasTokenItem(DEVICES)) {
			return token.replace(DEVICES, newItem);
		}
		if (hasTokenItem(SCENES)) {
			return token.replace(SCENES, newItem);
		}
		if (hasTokenItem(RELAYS)) {
			return token.replace(RELAYS, newItem);
		}
		if (hasTokenItem(RELAY_DRIVERS)) {
			return token.replace(RELAY_DRIVERS, newItem);
		}
		if (hasTokenItem(AREAS)) {
			return token.replace(AREAS, newItem);
		}
		if (hasTokenItem(ZONES)) {
			return token.replace(ZONES, newItem);
		}
		if (hasTokenItem(SCHEDULES)) {
			return token.replace(SCHEDULES, newItem);
		}
		if (hasTokenItem(USERS)) {
			return token.replace(USERS, newItem);
		}
		if (hasTokenItem(PROJECTS)) {
			return token.replace(PROJECTS, newItem);
		}
		if (hasTokenItem(SENSORS)) {
			return token.replace(SENSORS, newItem);
		}
		return token.replace(MODES, newItem);
	}

	private static String removeTokenDevice(String token) {
		if (hasTokenItem(DEVICE)) {
			final String deviceId = getTokenId(token, DEVICE);
			if (token.startsWith(DEVICE)) {
				return token.replace((DEVICE + deviceId + TOKEN_DLM), "");
			}
			else {
				return token.replace((TOKEN_DLM + DEVICE + deviceId), "");
			}
		}
		return token;
	}

	public static String newDashboardToken(Long projectId) {
		return DASHBOARD + TOKEN_DLM + PROJECT + projectId;
	}

	public static String newDeviceToken(long deviceId) {
		return replaceTokenItem(History.getToken(), DEVICE + deviceId);
	}

	public static String getProjectItem() {
		return getProjectItem(getToken());
	}

	public static String getProjectItem(String token) {
		if (hasTokenItem(token, DASHBOARD)) {
			return DASHBOARD;
		}
		if (hasTokenItem(token, DEVICES)) {
			return DEVICES;
		}
		if (hasTokenItem(token, SCENES)) {
			return SCENES;
		}
		if (hasTokenItem(token, MODES)) {
			return MODES;
		}
		if (hasTokenItem(token, RELAY_DRIVERS)) {
			return RELAY_DRIVERS;
		}
		if (hasTokenItem(token, RELAYS)) {
			return RELAYS;
		}
		if (hasTokenItem(token, SCHEDULES)) {
			return SCHEDULES;
		}
		if (hasTokenItem(token, SENSORS)) {
			return SENSORS;
		}
		if (hasTokenItem(token, AREAS)) {
			return AREAS;
		}
		if (hasTokenItem(token, ZONES)) {
			return ZONES;
		}
		if (hasTokenItem(token, SENSORS)) {
			return SENSORS;
		}
		if (hasTokenItem(token, DEVICE)) {
			return DEVICE;
		}
		return "";
	}

	public static String getDeviceId() {
		final String token = History.getToken();
		if (hasTokenItem(DEVICE)) {
			return getTokenId(token, DEVICE);
		}
		else {
			return null;
		}
	}

	public static boolean isItemChanged() {
		return !getProjectItem(getToken()).equals(getProjectItem(getPrevToken()));
	}

	public static Long getZoneIdLong() {
		try {
			return Long.parseLong(getTokenId(History.getToken(), ZONE));
		} catch (Exception e) {
			return null;
		}
	}

	public static String newDevicesToken() {
		final String token = History.getToken();
		return replaceTokenItem(token, DEVICES);
	}

	public static String newDashboardToken() {
		final String token = History.getToken();
		return replaceTokenItem(token, DASHBOARD);
	}
	
	public static String newZoneDashboardToken(Long zoneId) {
		return DASHBOARD + DLM + PROJECT + getTokenProjectId() + DLM + ZONE + zoneId;
	}
	
	public static String newZoneDevicesToken(Long zoneId) {
		return DEVICES + DLM + PROJECT + getTokenProjectId() + DLM + ZONE + zoneId;
	}

	public static String newRelayToken() {
		final String token = History.getToken();
		return replaceTokenItem(token, RELAYS);
	}

	public static String newUserToken() {
		final String token = History.getToken();
		return replaceTokenItem(token, USERS);
	}

	public static String newSensorToken() {
		final String token = History.getToken();
		return replaceTokenItem(token, SENSORS);
	}

	public static String newAreaToken() {
		final String token = History.getToken();
		return replaceTokenItem(token, AREAS);
	}

	public static String newZoneToken() {
		final String token = History.getToken();
		return replaceTokenItem(token, ZONES);
	}
	
	public static String newTokenProjects() {
		return SYSTEM + DLM + PROJECTS;
	}
	
	public static String newTokenUsers() {
		return SYSTEM + DLM + USERS;
	}
	
	public static String newTokenZones(String projectId) {
		return PROJECT + projectId + DLM + ZONE;
	}
	
	public static String newTokenDevices(String projectId) {
		return PROJECT + projectId + DLM + DEVICES;
	}
	
	public static String newTokenCotrollers(String projectId) {
		return PROJECT + projectId + DLM + RELAY_DRIVERS;
	}
	
	public static String newTokenRelays(String projectId) {
		return PROJECT + projectId + DLM + RELAYS;
	}
	
	public static String newTokenSensors(String projectId) {
		return PROJECT + projectId + DLM + SENSORS;
	}
	
	public static String newTokenUsers(String projectId) {
		return PROJECT + projectId + DLM + USERS;
	}

	public static String newTokenModes(String projectId) {
		return PROJECT + projectId + DLM + MODES;
	}
	
	public static String newTokenSchedules(String projectId) {
		return PROJECT + projectId + DLM + SCHEDULES;
	}

	public static String newTokenScenes(String projectId) {
		return PROJECT + projectId + DLM + SCENES;
	}
	
	public static String getTokenProjectId() {
		String token = History.getToken();
		String[] items = token.split(DLM);
		if (items.length >= 1) {
			return items[0].replace(PROJECT, "");
		}
		return NULL;
	}

	public static String getTokenEntity() {
		String token = History.getToken();
		String[] items = token.split(DLM);
		if (items.length >= 2) {
			return items[1];
		}
		return NULL;
	}

	public static String changeTokenProject(String newTokenProject) {
		String token = History.getToken();
		String[] items = token.split(DLM);
		String newToken = items[0];
		if (items.length > 1) {
			newToken = newToken + DLM + newTokenProject;
		}
		for (int i = 2; i < items.length; i++) {
			newToken = newToken + DLM + items[i];
		}
		return newToken;
	}

	public static String getToken() {
		return History.getToken();
	}

	public static String newDeviceTableToken() {
		return PROJECT + getProjectId() + TOKEN_DLM + DEVICES + TOKEN_DLM + TABLE;
	}
	
	public static String newModeTableToken() {
		return PROJECT + getProjectId() + TOKEN_DLM + MODES + TOKEN_DLM + TABLE;
	}
	
	public static String newModeListToken() {
		return PROJECT + getProjectId() + TOKEN_DLM + MODES + TOKEN_DLM;
	}
	
	public static String newSceneTableToken() {
		return PROJECT + getProjectId() + TOKEN_DLM + SCENES + TOKEN_DLM + TABLE;
	}
	
	public static String newZoneListToken() {
		return PROJECT + getProjectId() + TOKEN_DLM + ZONES;
	}
	
	public static String newZoneTableToken() {
		return PROJECT + getProjectId() + TOKEN_DLM + ZONES + TOKEN_DLM + TABLE;
	}
}
