package net.ionoff.center.client.mediaplayer.token;

import com.google.gwt.user.client.History;

public class PlayToken {

	public static final String NULL = "null";
	public static final String DLM = "/";
	public static final String DEVICE = "device_";
	private static Long playerId;
	
	public static String newPlayerToken(Long playerId) {
		return DEVICE + playerId;
	}
	
	public static boolean hasTokenItem(String tokenItem) {
		String token = History.getToken();
		return hasTokenItem(token, tokenItem);
	}
	
	public static boolean hasTokenItem(String token, String tokenItem) {
		if (token == null || token.isEmpty()) {
			return false;
		}
		String[] tokenItems = token.split(DLM);
		for (String item : tokenItems) {
			if (item.startsWith(tokenItem)) {
				return true;
			}
		}
		return false;
	}
	
	public static String[] getTokenItems(String token) {
		return token.split(DLM);
	}

	public static String getToken() {
		return History.getToken();
	}

	public static String getTokenPlayerId() {
		String token = History.getToken();
		if (hasTokenItem(DEVICE)) {
			return getTokenId(token, DEVICE);
		}
		else {
			return null;
		}
	}
	
	public static String getTokenId(String token, String itemName) {
		String[] tokenItems = getTokenItems(token);
		for (String item : tokenItems) {
			if (item.startsWith(itemName)) {
				return item.replaceFirst(itemName, "");
			}
		}
		return "";
	}

	public static void setPlayerId(Long id) {
		playerId = id;
	}
	
	public static Long getPlayerId() {
		return playerId;
	}
}
