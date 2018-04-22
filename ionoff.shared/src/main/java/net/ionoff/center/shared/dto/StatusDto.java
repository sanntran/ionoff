package net.ionoff.center.shared.dto;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class StatusDto implements Serializable {

	private static final long serialVersionUID = 1L;
	
	private Long id;
	private Boolean value;
	private String state;
	private String track;
	private String time;
	private Integer position;
	private String latestValue;
	private List<StatusDto> children;
	
	public Long getId() {
		return id;
	}
	public void setId(Long id) {
		this.id = id;
	}
	
	public Boolean getValue() {
		return value;
	}
	public void setValue(Boolean value) {
		this.value = value;
	}
	
	public String getState() {
		return state;
	}
	public void setState(String state) {
		this.state = state;
	}
	
	public String getTrack() {
		return track;
	}
	public void setTrack(String track) {
		this.track = track;
	}
	
	public String getTime() {
		if (time == null) {
			time = "---";
		}
		return time;
	}
	public void setTime(String time) {
		this.time = time;
	}
	
	public List<StatusDto> getChildren() {
		if (children == null) {
			children = new ArrayList<>();
		}
		return children;
	}
	public void setChildren(List<StatusDto> children) {
		this.children = children;
	}
	
	public Integer getPosition() {
		if (position == null) {
			position = 0;
		}
		return position;
	}
	public void setPosition(Integer position) {
		this.position = position;
	}

	public String getLatestValue() {
		return latestValue;
	}

	public void setLatestValue(String latestValue) {
		this.latestValue = latestValue;
	}
}
