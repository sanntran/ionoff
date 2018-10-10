package net.ionoff.center.server.license;

import java.io.File;
import java.io.IOException;
import java.net.NetworkInterface;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class LicenseManager {
	
	private static boolean activated = false;
	private static final List<String> LICENSE_KEYS = new ArrayList<>();
	
	private static final Logger LOGGER = LoggerFactory.getLogger(LicenseManager.class.getName());
	
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
	
	private static List<String> getMacAddresses() {
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
			LOGGER.error(e.getMessage(), e);
			return machineMacs;
		}
	}
	
	private static List<String> getMachineLicenseKeys() {
		if (!LICENSE_KEYS.isEmpty()) {
			return LICENSE_KEYS;
		}
		List<String> machineMacs = getMacAddresses();
		List<String> licenseKeys = new ArrayList<>();
		for (String mac : machineMacs) {
			try {
				String key = Crypto.getInstance().encrypt(mac);
				licenseKeys.add(key);
			}
			catch (Exception e) {
				LOGGER.error(e.getMessage(), e);
			}
		}
		return licenseKeys;
	}
	
	public static boolean isValidKey(String key) {
		for (String licenseKey : getMachineLicenseKeys()) {
			if (licenseKey.equals(key)) {
				return true;
			}
		}
		return false;
	}
	
	private static List<String> loadLicenseFiles() {
		String licenseFolder = getLicenseFolder();
		List<String> licenseKeys = new ArrayList<>();
		File licenseFileFolder = new File(licenseFolder);
		if (!licenseFileFolder.exists()) {
			licenseFileFolder.mkdir();
			return licenseKeys;
		}
		File[] keyFiles = licenseFileFolder.listFiles();
		for (File file : keyFiles) {
			licenseKeys.add(file.getName());
		}
		return licenseKeys;
	}
	
	public static void writeLicenseFiles(List<String> licenseKeys) throws IOException {
		String licenseFolder = getLicenseFolder();
		File licenseFileFolder = new File(licenseFolder);
		if (!licenseFileFolder.exists()) {
			licenseFileFolder.mkdir();
		}
		else {
			for (File file : licenseFileFolder.listFiles()) {
				file.delete();
			}
		}
		if (licenseKeys == null || licenseKeys.isEmpty()) {
			return;
		}
		for (String licenseKey : licenseKeys) {
			File file = new File(licenseFolder + "/" + licenseKey);
			file.createNewFile();
		}
	}
	
	private static String getLicenseFolder() {
		String key = "key";
		String catalinaHome = System.getProperty("catalina.base");
		if (catalinaHome != null) {
			return catalinaHome + "/" + key;
		}
		String javaHome = System.getProperty("java.home");
		if (javaHome != null) {
			return javaHome + "/" + key;
		}
		if (isWinPlatform()) {
			return "C:/Home/" + key;
		}
		else {
			return "/home/" + key;
		}
	}
	
	private static boolean isWinPlatform() {
		String os = System.getProperty("os.name");
		if (os.contains("Win")) {
			return true;
		}
		else {
			return false;
		}
	}

	public static boolean checkLicense() {
		List<String> storedLicenseKeys = loadLicenseFiles();
		for (String licenseKey : storedLicenseKeys) {
			if (isValidKey(licenseKey)) {
				LOGGER.info("License key is valid. Server is activated");
				activated = true;
				return true;
			}
		}
		LOGGER.info("No license key or license key is not valid. Server is not activated");
		activated = false;
		return false;
	}
	
	public static boolean isActivated() {
		return activated;
	}

}