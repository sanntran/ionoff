package net.ionoff.center.shared.dto;

public class SensorDataDto extends BaseDto {

	private static final long serialVersionUID = 1L;

	private String time;
	private Double value;
	private Integer index;
	private String sensorName;

	public String getTime() {
		return time;
	}
	public void setTime(String time) {
		this.time = time;
	}
	
	public Double getValue() {
		return value;
	}
	public void setValue(Double value) {
		this.value = value;
	}

	public Integer getIndex() {
		return index;
	}
	public void setIndex(Integer index) {
		this.index = index;
	}

	public String getSensorName() {
		return sensorName;
	}

	public void setSensorName(String sensorName) {
		this.sensorName = sensorName;
	}
}
