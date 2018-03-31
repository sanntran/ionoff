package net.ionoff.center.server.locale;

import java.util.Locale;
import java.util.ResourceBundle;

import net.xapxinh.center.server.locale.UTF8Control;

/**
 * @author Sann Tran
 */
public class Constants {

	private static Constants viConst;
	private static Constants enConst;
	
	private static final ResourceBundle VI_VN_BUNDLE = ResourceBundle.getBundle("Constants", new Locale("vi", "VN"), new UTF8Control());
	private static final ResourceBundle EN_US_BUNDLE = ResourceBundle.getBundle("Constants", new Locale("en", "US"), new UTF8Control());

	private final ResourceBundle resourceBundle;

	private Constants(ResourceBundle resourceBundle) {
		this.resourceBundle = resourceBundle;
	}

	public static Constants get(String locale) {
		if ("vi".equals(locale)) {
			if (enConst == null) {
				enConst = new Constants(EN_US_BUNDLE);
			}
			return enConst;
		}
		else {
			if (viConst == null) {
				viConst = new Constants(VI_VN_BUNDLE);
			}
			return viConst;
		}
	}

	public String area() {
		return resourceBundle.getString("area");
	}

	public String zone() {
		return resourceBundle.getString("zone");
	}

	public String device() {
		return resourceBundle.getString("device");
	}

	public String project() {
		return resourceBundle.getString("project");
	}

	public String ip() {
		return "IP";
	}
	
	public String port() {
		return "Port";
	}
	
	public String key() {
		return "Key";
	}
}
