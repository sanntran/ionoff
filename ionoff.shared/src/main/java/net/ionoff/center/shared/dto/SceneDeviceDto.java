package net.ionoff.center.shared.dto;

import java.util.List;

public class SceneDeviceDto extends BaseDto {

	private static final long serialVersionUID = 1L;
	
	private Integer order;
	private Integer duration;	
	private DeviceDto device;
	private List<SceneActionDto> actions;
	
	public Integer getOrder() {
		return order;
	}
	public void setOrder(Integer order) {
		this.order = order;
	}
	
	public Integer getDuration() {
		return duration;
	}
	public void setDuration(Integer duration) {
		this.duration = duration;
	}
	
	public DeviceDto getDevice() {
		return device;
	}
	public void setDevice(DeviceDto device) {
		this.device = device;
	}

	public List<SceneActionDto> getActions() {
		return actions;
	}
	public void setActions(List<SceneActionDto> actions) {
		this.actions = actions;
	}
	
	public boolean hasManyActions() {
		return actions != null && actions.size() > 1;
	}
	public boolean hasAction() {
		return actions != null && !actions.isEmpty();
	}
}
