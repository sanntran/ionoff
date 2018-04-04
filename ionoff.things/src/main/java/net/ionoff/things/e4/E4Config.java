package net.ionoff.things.e4;

import java.util.ArrayList;
import java.util.List;

public class E4Config {
	
	private String sn;
	private String broker;
	private String wifiId;
	private String wifiPass;
	private List<Integer> inputTypes;
	
	public List<Integer> getInputTypes() {
		return inputTypes;
	}
	public void setInputTypes(List<Integer> inputTypes) {
		this.inputTypes = inputTypes;
	}
	public void setInputTypes(String inputTypes) {
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
	
	public String getBroker() {
		return broker;
	}
	public void setBroker(String broker) {
		this.broker = broker;
	}
	
	public String getSn() {
		return sn;
	}
	public void setSn(String sn) {
		this.sn = sn;
	}
	
    public String getWifiId() {
        return wifiId;
    }
    public void setWifiId(String wifiId) {
        this.wifiId = wifiId;
    }
    
    public String getWifiPass() {
        return wifiPass;
    }
    public void setWifiPass(String wifiPass) {
        this.wifiPass = wifiPass;
    }
    
    public static boolean validateSn(final String sn) {
        if (sn.length() != 16) {
            return false;
        }
        if (!sn.startsWith("e4")) {
        	return false;
        }
        return true;
    }
    
    public static boolean validateWifiId(final String wifiId) {
        return wifiId != null && !wifiId.trim().isEmpty();
    }
    
    public static boolean validateWifiPw(final String wifiPw) {
        return wifiPw != null && !wifiPw.trim().isEmpty();
    }
}
