package net.ionoff.center.client.utils;

import com.google.gwt.user.client.History;

public class TokenUtil {
	
	public static final String NULL = "null";
	public static final String TOKEN_DLM = "/";
	
	private static String prevToken;
	private static boolean sourceChangeTokenEvent = false;
	
	public static boolean hasTokenItem(String tokenItem) {
		String token = History.getToken();
		return hasTokenItem(token, tokenItem);
	}
	
	public static boolean hasTokenItem(String token, String tokenItem) {
		if (token == null || token.isEmpty()) {
			return false;
		}
		String[] tokenItems = token.split(TOKEN_DLM);
		return hasTokenItem(tokenItems, tokenItem);
	}
	
	public static String[] getTokenItems(String token) {
		if (token == null) {
			return new String[] {};
		}
		return token.split(TOKEN_DLM);
	}

	public static String getToken() {
		return History.getToken();
	}

	public static String getTokenId(String token, String itemName) {
		String[] tokenItems = getTokenItems(token);
		return getTokenId(tokenItems, itemName);
	}
	
	public static boolean hasTokenItem(String[] tokenItems, String tokenItem) {
		for (String item : tokenItems) {
			if (item.startsWith(tokenItem)) {
				return true;
			}
		}
		return false;
	}
	
	public static String getTokenId(String[] tokenItems, String itemName) {
		for (final String item : tokenItems) {
			if (item.startsWith(itemName)) {
				return item.replaceFirst(itemName, "");
			}
		}
		return "";
	}
	
	public static String getPrevToken() {
		return prevToken;
	}
	
	public static void setPrevToken(String token) {
		prevToken = token;
	}
	
	
	public static boolean getSourceChangeTokenEvent() {
		return sourceChangeTokenEvent;
	}
	
	public static void setSourceChangeTokenEvent(boolean sourceChangeToken) {
		sourceChangeTokenEvent = sourceChangeToken;
	}
}
