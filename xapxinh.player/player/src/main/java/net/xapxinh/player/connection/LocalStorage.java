package net.xapxinh.player.connection;

import java.io.File;
import java.net.URL;

public class LocalStorage {

	public static String getAppDir() {
		String dir = ClassLoader.getSystemClassLoader().getResource(".").getPath();
		URL source = LocalStorage.class.getProtectionDomain().getCodeSource().getLocation();
		if (dir.equals(source.getPath())) {
			return new File(dir).getParent();
		}
		return dir + File.separator + "storage";
	}
}
