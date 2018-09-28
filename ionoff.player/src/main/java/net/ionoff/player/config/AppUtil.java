package net.ionoff.player.config;

import org.apache.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.net.NetworkInterface;
import java.net.URL;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;

public class AppUtil {

	private static final Logger LOGGER = Logger.getLogger(AppUtil.class.getName());

	public static String getCurrentDir() {
		String dir = ClassLoader.getSystemClassLoader().getResource(".").getPath();
		URL source = AppUtil.class.getProtectionDomain().getCodeSource().getLocation();
		if (dir.equals(source.getPath())) {
			return new File(dir).getParent();
		}
		return dir;
	}
	
	public static String getUserHomeDir() {
		return System.getProperty("user.home");
	}
	
	public static String getAppDataFile() throws IOException {
		String appDataFile = System.getProperty("user.home") + "/.appdata/imp.properties";
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

	private static String formatMac(byte[] mac) {
		if (mac == null) {
			return "null";
		}
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < mac.length; i++) {
			sb.append(String.format("%02X%s", mac[i], (i < mac.length - 1) ? "-" : ""));
		}
		return sb.toString();
	}

	static List<String> getMacAddresses() {
		String macReg = "^([0-9A-Fa-f]{2}[:-]){5}[0-9A-Fa-f]{2}$";
		List<String> machineMacs = new ArrayList<>();
		try {
			for (Enumeration<NetworkInterface> e = NetworkInterface.getNetworkInterfaces(); e.hasMoreElements();) {
				NetworkInterface ni = e.nextElement();
				String mac =  formatMac(ni.getHardwareAddress());
				if (mac.matches(macReg)) {
					machineMacs.add(mac);
				}
			}
			return machineMacs;
		}
		catch (Exception e) {
			LOGGER.error("Error when get mac address: " + e.getClass().getSimpleName() + " " + e.getMessage(), e);
			return machineMacs;
		}
	}


	public static void main(String args[]) {
		List<String> mac = getMacAddresses();
		System.out.println(mac);
	}
}
