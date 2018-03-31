package net.ionoff.center.client.locale;

import com.google.gwt.i18n.client.Messages;

/**
 * @author Sann Tran
 */
public interface LoginMessages extends Messages {

	@DefaultMessage("''{0}'' không hợp lệ")
	String fieldInvalid(String field);

	@DefaultMessage("Người dùng/Mật khẩu không đúng")
	String loginFailed();

	@DefaultMessage("Key bản quyền không đúng")
	String invalidLicenseKey();
	
	@DefaultMessage("Phần mềm đã được kích hoạt")
	String applicationActivated();
}
