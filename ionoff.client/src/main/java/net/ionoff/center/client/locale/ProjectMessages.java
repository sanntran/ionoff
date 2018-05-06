package net.ionoff.center.client.locale;

import com.google.gwt.i18n.client.Messages;

/**
 * @author Sann Tran
 */
public interface ProjectMessages extends Messages {
	
	@DefaultMessage("Hoàn tất kịch bản ''{0}''")
	String performedScene(String sceneName);

	@DefaultMessage("Đang tạo file báo cáo...")
	String generatingReport();
	
	@DefaultMessage("Có lỗi khi tạo file báo cáo.")
	String errorGeneratingReport();

	@DefaultMessage("Tạo file báo cáo thành công.")
	String finishGeneratingReport();
}
