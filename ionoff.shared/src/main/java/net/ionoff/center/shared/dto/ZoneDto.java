package net.ionoff.center.shared.dto;

import java.util.ArrayList;
import java.util.List;


public class ZoneDto extends BaseDto {

	private static final long serialVersionUID = 1L;
	
	public static final String AREA = "area";
	public static final String ORDER = "order";
	
	private Integer order;
	private Long areaId;
	private Boolean lighting;
	private Integer devicesCount;
	private String areaName;
	private List<DeviceDto> devices;

	public Integer getOrder() {
		return order;
	}

	public void setOrder(Integer order) {
		this.order = order;
	}

	public ZoneDto() {
		devices = new ArrayList<>();
	}

	public Long getAreaId() {
		return areaId;
	}
	public void setAreaId(Long areaId) {
		this.areaId = areaId;
	}

	public String getAreaName() {
		return areaName;
	}
	public void setAreaName(String areaName) {
		this.areaName = areaName;
	}

	public List<DeviceDto> getDevices() {
		return devices;
	}
	public void setDevices(List<DeviceDto> devices) {
		this.devices = devices;
	}

	public String getPathName() {
		return getAreaName() + " >> " + getName();
	}

	public String getPathNameId() {
		return formatNameID(areaName, areaId) + " >> " + formatNameID(this);
	}
	
	public boolean hasDevice() {
		return devices != null && !devices.isEmpty();
	}
	
	@Override
	public int compareTo(BaseDto entity) {
		if (entity instanceof ZoneDto) {
			ZoneDto zoneDto = (ZoneDto) entity;
			String myAreaNameId = BaseDto.formatNameID(areaName, areaId);
			String areaNameId = BaseDto.formatNameID(zoneDto.getAreaName(), zoneDto.getAreaId());
			if (areaNameId.equals(myAreaNameId)) {
				return super.compareTo(entity);
			}
			return myAreaNameId.compareTo(areaNameId);
		}
		return super.compareTo(entity);
	}

	public Integer getDevicesCount() {
		return devicesCount;
	}

	public void setDevicesCount(Integer devicesCount) {
		this.devicesCount = devicesCount;
	}

	public Boolean getLighting() {
		return lighting;
	}

	public void setLighting(Boolean lighting) {
		this.lighting = lighting;
	}
}
