package net.xapxinh.center.client.player;

public class PlayerUtil {
	
	public static String formatTimePlayer(long time) { // time is second
		String result = "";
		long hour = 0;
		long min = time / 60;
		final long second = time % 60;
		if (min >= 60) {
			hour = min / 60;
			min = min % 60;
		}
		String m = min + "";
		if (min < 10) {
			m = "0" + min;
		}
		String s = second + "";
		if (second < 10) {
			s = "0" + second;
		}

		if (hour == 0) {
			result = m + ":" + s;
		}
		else {
			String h = hour + "";
			if (hour < 10) {
				h = "0" + hour;
			}
			result = h + ":" + m + ":" + s;
		}
		return result;
	}
}
