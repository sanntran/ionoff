package net.ionoff.center.client.locale;

import com.google.gwt.i18n.client.Messages;

/**
 * @author Sann Tran
 */
public interface AdminMessages extends Messages {
	
	@DefaultMessage("''{0}'' không được để trống")
	String emptyInputValue(String field);
	
	@DefaultMessage("''{0}'' dài quá mức cho phép")
	String overMaximunLength(String field);

	@DefaultMessage("Đã lưu")
	String updateSuccess();
	
	@DefaultMessage("Đã xóa")
	String deleteSuccess();
	
	@DefaultMessage("''{0}'' chứa ký tự đặc biệt")
	String containsSpecialCharacters(String field);
	
	@DefaultMessage("Đối tượng mới tạo (ID: ''{0}'') chưa được lưu")
	String newRowUnsaved(long entityId);
	
	@DefaultMessage("''{0}'' không hợp lệ")
	String invalidFieldValue(String field);
	
	@DefaultMessage("''{0}'' không phải là số")
	String invalidNumberValue(String value);
	
	@DefaultMessage("Lỗi kết nối Player")
	String errorConnectPlayer();
	
	@DefaultMessage("Không có cảm biến")
	String noSensorFound();
	
	@DefaultMessage("Không đổi model bộ điều khiển đã thêm")
	String unsupportChangingRelayDriverModel();
}
