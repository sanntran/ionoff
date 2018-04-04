package net.ionoff.things.p4;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class P4Config {
	
	private List<Integer> inputTypes;
	private String srvIp;
	private String ip;
	private String gateway;
	private String subnetMask;
	private String mac;
	
	List<Integer> getInputTypes() {
		return inputTypes;
	}
	void setInputTypes(List<Integer> inputTypes) {
		this.inputTypes = inputTypes;
	}
	void setInputTypes(String inputTypes) {
		this.inputTypes = new ArrayList<>();
		for (int i = 0; i < inputTypes.length(); i++) {
			if (inputTypes.charAt(i) == '2') {
				this.inputTypes.add(2);
			}
			else {
				this.inputTypes.add(1);
			}
		}
	}
	
	String getSrvIp() {
		return srvIp;
	}
	void setSrvIp(String srvIp) {
		this.srvIp = hexIpToNumberIp(srvIp);
	}
	
	static String hexIpToNumberIp(String hexIp) {
		String ipNo = "";
		String hexes[] = hexIp.split("\\.");
		for (String hex : hexes) {
			int value = Integer.parseInt(hex, 16); 
			ipNo = ipNo + value + ".";
		}
		
		return ipNo.substring(0, ipNo.length() - 1);
	}
	
	String getMac() {
		return mac;
	}
	void setMac(String mac) {
		this.mac = mac;
	}
	String getGateway() {
		return gateway;
	}
	void setGateway(String gateway) {
		this.gateway = hexIpToNumberIp(gateway);
	}
	
	String getSubnetMask() {
		return subnetMask;
	}
	void setSubnetMask(String subnetMask) {
		this.subnetMask = hexIpToNumberIp(subnetMask);
	}
	
	String getIp() {
		return ip;
	}
	void setIp(String ip) {
		this.ip = hexIpToNumberIp(ip);;
	}

	static final String IPADDRESS_REGEX = "^([01]?\\d\\d?|2[0-4]\\d|25[0-5])\\."
			+ "([01]?\\d\\d?|2[0-4]\\d|25[0-5])\\." + "([01]?\\d\\d?|2[0-4]\\d|25[0-5])\\."
			+ "([01]?\\d\\d?|2[0-4]\\d|25[0-5])$";

	private static final Pattern IP_PATTERN = Pattern.compile(IPADDRESS_REGEX);;

	/**
	 * Validate ip address with regular expression
	 * 
	 * @param ip
	 *            ip address for validation
	 * @return true valid ip address, false invalid ip address
	 */
	static boolean validateIp(final String ip) {
		Matcher matcher = IP_PATTERN.matcher(ip);
		return matcher.matches();
	}
	
	static final String MAC_REGEX = "^([0-9A-Fa-f]{2}[:-]){5}([0-9A-Fa-f]{2})$";
	private static final Pattern MAC_PATTERN = Pattern.compile(MAC_REGEX);;
	
	static boolean validatePw(final String pw) {
		if (pw.length() != 8) {
			return false;
		}
		StringBuilder sb = new StringBuilder();
		sb.append("00-00-");
		for (int i = 0; i < pw.length(); i++) {
			sb.append(pw.charAt(i));
			if (i !=7 && i%2 != 0) {
				sb.append('-');
			}
		}
		Matcher matcher = MAC_PATTERN.matcher(sb.toString());
		return matcher.matches();
	}
	
	static boolean validateMac(String mac) {
		String realMac = "00-04-" + mac;
		Matcher matcher = MAC_PATTERN.matcher(realMac);
		return matcher.matches() && realMac.startsWith("00-04-A3");
	}
}
