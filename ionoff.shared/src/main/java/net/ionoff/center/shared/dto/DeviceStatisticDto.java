package net.ionoff.center.shared.dto;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class DeviceStatisticDto implements Serializable {

	private static final long serialVersionUID = 1L;

	private int totalOn;
	private DeviceDto deviceOn;

	public int getTotalOn() {
		return totalOn;
	}

	public void setTotalOn(int totalOn) {
		this.totalOn = totalOn;
	}

	public DeviceDto getDeviceOn() {
		return deviceOn;
	}

	public void setDeviceOn(DeviceDto deviceOn) {
		this.deviceOn = deviceOn;
	}
}
