package net.ionoff.center.client.locale;

import com.google.gwt.i18n.client.Constants;

/**
 * @author Sann Tran
 */
public interface AdminConstants extends Constants {

	@DefaultStringValue("Hệ thống")
	String system();

	@DefaultStringValue("Hẹn giờ")
	String schedule();

	@DefaultStringValue("Kịch bản")
	String scene();

	@DefaultStringValue("Dự án")
	String project();

	@DefaultStringValue("Khu")
	String area();

	@DefaultStringValue("Vùng")
	String zone();

	@DefaultStringValue("Thiết bị")
	String device();

	@DefaultStringValue("Rơ-le")
	String relay();

	@DefaultStringValue("Bộ điều khiển")
	String controller();

	@DefaultStringValue("Cảm biến")
	String sensor();

	@DefaultStringValue("Người dùng")
	String user();

	@DefaultStringValue("Chế độ")
	String mode();

	@DefaultStringValue("Phân quyền")
	String userRole();

	@DefaultStringValue("Tên")
	String name();

	@DefaultStringValue("Địa chỉ")
	String address();

	@DefaultStringValue("Loại")
	String type();
	
	@DefaultStringValue("Thứ tự")
	String order();

	@DefaultStringValue("Media player")
	String mediaPlayer();

	@DefaultStringValue("Tải cho Rơle")
	String relayLoad();

	@DefaultStringValue("IP")
	String ip();

	@DefaultStringValue("Key")
	String key();

	@DefaultStringValue("Mã")
	String mac(); // MAC

	@DefaultStringValue("Model")
	String model();

	@DefaultStringValue("Mở rộng")
	String extended();

	@DefaultStringValue("Công tắc")
	String zwitch();

	@DefaultStringValue("Nút nhấn")
	String button();

	@DefaultStringValue("Chỉ số")
	String index();

	@DefaultStringValue("Input")
	String input();

	@DefaultStringValue("Mật khẩu")
	String password();

	@DefaultStringValue("Họ tên")
	String fullName();

	@DefaultStringValue("Số ĐT")
	String phoneNumber();

	@DefaultStringValue("Email")
	String email();

	@DefaultStringValue("Nhóm")
	String group();

	@DefaultStringValue("Người dùng")
	String projectUser();
	
	@DefaultStringValue("Quản trị dự án")
	String projectAdmin();
	
	@DefaultStringValue("Quản trị hệ thống")
	String systemAdmin();

	@DefaultStringValue("T2")
	String mon();

	@DefaultStringValue("T3")
	String tue();

	@DefaultStringValue("T4")
	String wed();

	@DefaultStringValue("T5")
	String thu();

	@DefaultStringValue("T6")
	String fri();

	@DefaultStringValue("T7")
	String sat();

	@DefaultStringValue("CN")
	String sun();

	@DefaultStringValue("Lặp lại")
	String repeat();

	@DefaultStringValue("Một lần")
	String once();

	@DefaultStringValue("Hàng ngày")
	String daily();

	@DefaultStringValue("Hàng tuần")
	String weekly();

	@DefaultStringValue("Thời điểm")
	String time();

	@DefaultStringValue("Ngày")
	String date();

	@DefaultStringValue("Lịch hẹn giờ")
	String scheduleTime();

	@DefaultStringValue("Kích hoạt")
	String enabled();

	@DefaultStringValue("Hành động")
	String action();

	@DefaultStringValue("Không")
	String none();

	@DefaultStringValue("Ngắt")
	String open();

	@DefaultStringValue("Đóng")
	String close();

	@DefaultStringValue("Đóng-Ngắt")
	String closeOpen();

	@DefaultStringValue("Play")
	String play();

	@DefaultStringValue("Stop")
	String stop();

	@DefaultStringValue("Âm lượng")
	String volume();

	@DefaultStringValue("Album")
	String album();

	@DefaultStringValue("Time bufer")
	String timeBuffer();

	@DefaultStringValue("Có người")
	String detectedHuman();

	@DefaultStringValue("Không có người")
	String detectedNoHuman();

	@DefaultStringValue("Chọn...")
	String select();
	
	@DefaultStringValue("Gởi SMS")
	String sendSms();
	
	@DefaultStringValue("Gởi Email")
	String sendEmail();
	
	@DefaultStringValue("Lưu")
	String save();
	
	@DefaultStringValue("Hủy")
	String cancel();

	@DefaultStringValue("Giờ")
	String hour();
	
	@DefaultStringValue("Phút")
	String minute();
	
	@DefaultStringValue("AM/PM")
	String amPm();

	@DefaultStringValue("Không")
	String no();
	
	@DefaultStringValue("Có")
	String yes();
	
	@DefaultStringValue("Cài đặt")
	String setting();
	
	@DefaultStringValue("Port")
	String port();
	
	@DefaultStringValue("Nhóm Rơ-le")
	String relayGroup();
	
	@DefaultStringValue("Thời gian (giây)")
	String durationSecond();

	@DefaultStringValue("Trạng thái")
	String status();

	@DefaultStringValue("Giá trị")
	String value();

	@DefaultStringValue("Tổng")
	String total();
	
	@DefaultStringValue("Điều kiện")
	String condition();

	@DefaultStringValue("Bộ giao tiếp")
	String gateway();

	@DefaultStringValue("Tất cả")
	String all();

	@DefaultStringValue("VD: x == 1")
	String conditionExample();

	@DefaultStringValue("Tin nhắn")
	String message();
	
	@DefaultStringValue("(IONOFF_E3 hoặc IONOFF_E4: 0, 1, 2)")
	String inputNote();
	
	@DefaultStringValue("Khóa")
	String lock();
	
	@DefaultStringValue("Tự chuyển trạng thái (giây)")
	String autoRevert();
	
}
