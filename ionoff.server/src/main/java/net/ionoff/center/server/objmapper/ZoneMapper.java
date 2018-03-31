package net.ionoff.center.server.objmapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;

import net.ionoff.center.server.entity.Area;
import net.ionoff.center.server.entity.Device;
import net.ionoff.center.server.entity.Zone;
import net.ionoff.center.server.persistence.service.IAreaService;
import net.ionoff.center.shared.dto.DeviceDto;
import net.ionoff.center.shared.dto.ZoneDto;

public class ZoneMapper {
	
	@Autowired
	private DeviceMapper deviceMapper;
	
	@Autowired
	private IAreaService areaService;


	public List<ZoneDto> createZoneDtoList(List<Zone> zones, boolean includingDevice) {
		final List<ZoneDto> zoneDtos = new ArrayList<ZoneDto>();
		for (final Zone zone : zones) {
			zoneDtos.add(createZoneDto(zone, includingDevice));
		}
		return zoneDtos;
	}
	
	public Zone createZone(ZoneDto zoneDto) {
		final Zone zone = new Zone();
		final Area area = areaService.findById(zoneDto.getAreaId());
		zone.setArea(area);
		zone.setProject(area.getProject());
		updateZone(zone, zoneDto);
		return zone;
	}
	
	public Zone updateZone(Zone zone, ZoneDto zoneDto) {
		zone.setName(zoneDto.getName());
		zone.setOrder(zoneDto.getOrder());
		return zone;
	}
	
	public ZoneDto createZoneDto(Zone zone, boolean includingDevice) {
		final ZoneDto zoneDto = new ZoneDto();
		zoneDto.setId(zone.getId());
		zoneDto.setName(zone.getName());
		zoneDto.setOrder(zone.getOrder());
		zoneDto.setAreaId(zone.getArea().getId());
		zoneDto.setAreaName(zone.getArea().getName());
		if (zone.hasDevices()) {
			zoneDto.setDevicesCount(zone.getDevices().size());
		}
		else {
			zoneDto.setDevicesCount(0);
		}
		if (includingDevice && zone.getDevices() != null) {
			List<DeviceDto> deviceDtos = new ArrayList<>();
			for (final Device device : zone.getDevices()) {
				final DeviceDto deviceDto = deviceMapper.createDeviceDto(device);
				deviceDtos.add(deviceDto);
			}
			zoneDto.setDevices(deviceDtos);
		}
		return zoneDto;
	}
}
