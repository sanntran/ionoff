package net.ionoff.center.shared.dto;

public class SwitchDto extends BaseDto {

	private static final long serialVersionUID = 1L;

	private String time;
	private Integer index;
	private Boolean status;
	private Long driverId;
	private String driverName;

	public Integer getIndex() {
		return index;
	}
	public void setIndex(Integer index) {
		this.index = index;
	}

	public Boolean getStatus() {
		return status;
	}
	public void setStatus(Boolean status) {
		this.status = status;
	}

	public Long getDriverId() {
		return driverId;
	}
	public void setDriverId(Long driverId) {
		this.driverId = driverId;
	}
	
	public String getTime() {
		if (time == null) {
			return "---";
		}
		return time;
	}
	public void setTime(String time) {
		this.time = time;
	}
	public String getDriverName() {
		return driverName;
	}
	public void setDriverName(String driverName) {
		this.driverName = driverName;
	}
}
