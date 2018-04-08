package net.ionoff.center.shared.dto;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonSubTypes.Type;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonTypeInfo.As;
import com.fasterxml.jackson.annotation.JsonTypeInfo.Id;

@JsonSubTypes({
	@Type(value=LightDto.class, name="LightDto"), 
	@Type(value=PlayerDto.class, name="PlayerDto"),
	@Type(value=WeighScaleDto.class, name="WeighScaleDto"),
	@Type(value=ApplianceDto.class, name="ApplianceDto")})
@JsonTypeInfo(use=Id.NAME, include=As.PROPERTY, property="clazz")
public class DeviceDto extends BaseDto {

	private static final long serialVersionUID = 1L;
	
	public static final String ORDER = "order";
	public static final String ZONE = "zone";
	public static final Integer DEFAULT_ORDER = 1;
	
	private Integer order;
	private Long zoneId;
	private Long projectId;
	private String zoneName;
	private String areaName;
	private StatusDto status;

	public String getClazz() {
		return "DeviveDto";
	}

	public Integer getOrder() {
		return order;
	}
	public void setOrder(Integer order) {
		this.order = order;
	}
	
	public StatusDto getStatus() {
		return status;
	}
	public void setStatus(StatusDto status) {
		this.status = status;
	}
	
	public Long getZoneId() {
		return zoneId;
	}
	public void setZoneId(Long zoneId) {
		this.zoneId = zoneId;
	}
	
	public String getZoneName() {
		return zoneName;
	}
	public void setZoneName(String zoneName) {
		this.zoneName = zoneName;
	}
	
	public String getPathNameId() {
		return formatNameID(zoneName, zoneId) + " > " + formatNameID(getName(), getId());
	}
	public String getAreaName() {
		return areaName;
	}
	public void setAreaName(String areaName) {
		this.areaName = areaName;
	}
	
	public String getStyle() {
		return "device";
	}
	
	@Override
	public String toString() {
		StringBuilder sBuilder = new StringBuilder();
		sBuilder.append(super.toString())
				.append(", Clazz: ").append(getClazz());
		return sBuilder.toString();
		
	}

	public Long getProjectId() {
		return projectId;
	}

	public void setProjectId(Long projectId) {
		this.projectId = projectId;
	}
}
