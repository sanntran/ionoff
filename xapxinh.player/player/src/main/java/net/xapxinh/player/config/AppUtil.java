package net.xapxinh.player.config;

import java.io.File;
import java.io.IOException;

public class AppUtil {
	
	public static String getCurrentDir() {
		return System.getProperty("user.dir");
	}
	
	public static String getUserHomeDir() {
		return System.getProperty("user.home");
	}
	
	public static String getAppDataFile() throws IOException {
		String appDataFile = System.getProperty("user.home") + "/.appdata/xmp.properties";
		File file = new File(appDataFile);
		if (!file.exists()) {
			mkDir(file.getParentFile());
			file.createNewFile();
		}
		return appDataFile;
	}
	
	public static void mkDir(File dir) {
		File parent = dir.getParentFile();
		if (parent.exists()) {
			dir.mkdir();
		}
		else {
			try {
				mkDir(parent);
			}
			catch (Exception e) {
				// ignore
			}
			mkDir(dir);
		}
	}
}
