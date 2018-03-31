package net.ionoff.center.client.locale;

import com.google.gwt.i18n.client.Messages;

/**
 * @author Sann Tran
 */
public interface ClientMessages extends Messages {
	
	@DefaultMessage("''{0}'' không hợp lệ")
	String fieldInvalid(String field);

	@DefaultMessage("Lỗi ''{0}'': ''{1}''")
	String serverErrorResponse(String status, String message);

	@DefaultMessage("Lỗi kết nối server")
	String serverConnectionError();
	
	@DefaultMessage("Đang cập nhật ''{0}''...")
	String upgradingNewVersion(String version);
	
	@DefaultMessage("Chưa có phiên bản mới")
	String currentVersionIsUptoDate();
	
	@DefaultMessage("Không có dự án. Vui lòng liên hệ nhà quản trị")
	String noProject();

	@DefaultMessage("Quyền truy cập bị giới hạn. Vui lòng liên hệ nhà quản trị")
	String accessDenied();
}
