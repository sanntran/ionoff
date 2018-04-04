package net.ionoff.license;

import java.net.NetworkInterface;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;

public class SerialReader {
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
			e.printStackTrace();
			return machineMacs;
		}
	}
	
	public static void main(String[] args) {
		List<String> macs = getMacAddresses();
		for (String mac : macs) {
			System.out.print(mac);
		}
	}
}
