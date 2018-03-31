package net.ionoff.center.server.util;

public class CommonUtil {
	
	public static boolean isIntNumber(String str) {
		try {
			Integer.parseInt(str);
			return true;
		}
		catch (Exception e) {
			return false;
		}
	}
	
	public static String toString(String[] strArr) {
		StringBuilder builder = new StringBuilder();
		for (int i = 0; i < strArr.length; i++) {
			builder.append(strArr[i]);
			if (i < (strArr.length - 1)) {
				builder.append(",");
			}
		}
		return builder.toString();
	}
}
