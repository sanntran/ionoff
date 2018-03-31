package net.ionoff.center.shared.dto;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonSubTypes.Type;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonTypeInfo.As;
import com.fasterxml.jackson.annotation.JsonTypeInfo.Id;

@JsonSubTypes({
	@Type(value=SchedulePlayerActionDto.class, name="SchedulePlayerActionDto"), 
	@Type(value=ScheduleRelayActionDto.class, name="ScheduleRelayActionDto")})
@JsonTypeInfo(use=Id.NAME, include=As.PROPERTY, property="clazz")
public class ScheduleActionDto extends BaseDto {

	private static final long serialVersionUID = 1L;
	
	public static final String NONE = "None";
	
	private String action;
	private Long scheduleId;
	private String scheduleName;
	
	public String getAction() {
		return action;
	}
	public void setAction(String action) {
		this.action = action;
	}
	
	public Long getScheduleId() {
		return scheduleId;
	}
	public void setScheduleId(Long scheduleId) {
		this.scheduleId = scheduleId;
	}
	
	public String getScheduleName() {
		return scheduleName;
	}
	public void setScheduleName(String scheduleName) {
		this.scheduleName = scheduleName;
	}	
}
