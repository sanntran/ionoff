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
	public static final String AREAS = "areas";
	public static final String ZONES = "zones";
	public static final String RELAYS = "relays";
	public static final String CONTROLLERS = "controllers";
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
				|| hasTokenItem(tokenItems, CONTROLLERS)
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

	private static String replaceTokenItem(String token, String newItem) {
		token = token.replace(TOKEN_DLM + TABLE, "");
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
		if (hasTokenItem(CONTROLLERS)) {
			return token.replace(CONTROLLERS, newItem);
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

	public static String newDashboardToken(Long projectId) {
		return PROJECT + projectId +TOKEN_DLM + DASHBOARD;
	}

	public static String newDeviceToken(long deviceId) {
		return replaceTokenItem(History.getToken(), DEVICE + deviceId);
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

	public static Long getZoneIdLong() {
		String zoneId = getTokenId(getToken(), ZONE);
		try {
			return Long.parseLong(zoneId);
		} catch (Exception e) {
			return null;
		}
	}

	public static String newDashboardToken() {
		final String token = History.getToken();
		return replaceTokenItem(token, DASHBOARD);
	}
	
	public static String newZoneDashboardToken(Long zoneId) {
		return PROJECT + getTokenProjectId() + TOKEN_DLM + ZONE + zoneId + TOKEN_DLM + DASHBOARD;
	}
	

	public static String newTokenProjectTable() {
		return SYSTEM + TOKEN_DLM + PROJECTS;
	}
	
	public static String getTokenProjectId() {
		String token = History.getToken();
		String[] items = token.split(TOKEN_DLM);
		for (String item : items) {
			if (item.startsWith(PROJECT)) {
				return item.replace(PROJECT, "");
			}
		}
		return NULL;
	}

	public static String getTokenEntity() {
		String token = History.getToken();
		String[] items = token.split(TOKEN_DLM);
		if (items.length >= 2) {
			return items[1];
		}
		return NULL;
	}

	public static String changeTokenProject(String newTokenProject) {
		String token = History.getToken();
		String[] items = token.split(TOKEN_DLM);
		String newToken = items[0];
		if (items.length > 1) {
			newToken = newToken + TOKEN_DLM + newTokenProject;
		}
		for (int i = 2; i < items.length; i++) {
			newToken = newToken + TOKEN_DLM + items[i];
		}
		return newToken;
	}

	public static String getToken() {
		return History.getToken();
	}


	public static String newAreaTableToken() {
		return PROJECT + getProjectId() + TOKEN_DLM + AREAS + TOKEN_DLM + TABLE;
	}

	public static String newSensorTableToken() {
		return PROJECT + getProjectId() + TOKEN_DLM + SENSORS + TOKEN_DLM + TABLE;
	}

	public static String newRelayTableToken() {
		return PROJECT + getProjectId() + TOKEN_DLM + RELAYS + TOKEN_DLM + TABLE;
	}

	public static String newControllerTableToken() {
		return PROJECT + getProjectId() + TOKEN_DLM + CONTROLLERS + TOKEN_DLM + TABLE;
	}

	public static String newScheduleTableToken() {
		return PROJECT + getProjectId() + TOKEN_DLM + SCHEDULES + TOKEN_DLM + TABLE;
	}

	public static String newModeTableToken() {
		return PROJECT + getProjectId() + TOKEN_DLM + MODES + TOKEN_DLM + TABLE;
	}

	public static String newSceneTableToken() {
		return PROJECT + getProjectId() + TOKEN_DLM + SCENES + TOKEN_DLM + TABLE;
	}

	public static String newZoneTableToken() {
		return PROJECT + getProjectId() + TOKEN_DLM + ZONES + TOKEN_DLM + TABLE;
	}

	public static String newUserTableToken() {
		if (History.getToken().contains(SYSTEM)) {
			return SYSTEM + TOKEN_DLM + USERS;
		}
		return PROJECT + getProjectId() + TOKEN_DLM + USERS + TOKEN_DLM + TABLE;
	}

	public static String newDeviceTableToken() {
		return PROJECT + getProjectId() + TOKEN_DLM + DEVICES + TOKEN_DLM + TABLE;
	}
}
