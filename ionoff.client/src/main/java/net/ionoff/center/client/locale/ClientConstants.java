package net.ionoff.center.client.locale;

import com.google.gwt.i18n.client.Constants;

/**
 * @author Sann Tran
 */
public interface ClientConstants extends Constants {
	@DefaultStringValue("Điều khiển")
	String control();
	
	@DefaultStringValue("Quản trị")
	String administration();
	
	@DefaultStringValue("Thoát")
	String logout();
	
	@DefaultStringValue("Super admin")
	String superAdmin();
	
	@DefaultStringValue("Tiếng Việt")
	String vietnamese();
	
	@DefaultStringValue("English")
	String english();
	
	@DefaultStringValue("© 2015 iOnOff Technology Co., Ltd")
	String copyRight();

	@DefaultStringValue("Cập nhật V-")
	String upgrade();
	
	@DefaultStringValue("Kiểm tra phiên bản mới")
	String checkLatestVersion();

	@DefaultStringValue("Full screen")
	String fullScreen();
	
	@DefaultStringValue("Hệ thống")
	String system();
	
	@DefaultStringValue("Cài đặt")
	String setting();
}
