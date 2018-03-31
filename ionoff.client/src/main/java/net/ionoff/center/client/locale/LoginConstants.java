package net.ionoff.center.client.locale;

import com.google.gwt.i18n.client.Constants;

public interface LoginConstants extends Constants {
	
	@DefaultStringValue("Người dùng")
	String userName();
	
	@DefaultStringValue("Mật khẩu")
	String password();
	
	@DefaultStringValue("Nhớ mật khẩu")
	String remeberPassword();
	
	@DefaultStringValue("Đăng nhập")
	String login();
	
	@DefaultStringValue("Lưu")
	String save();
	
	@DefaultStringValue("Server")
	String server();
	
}
