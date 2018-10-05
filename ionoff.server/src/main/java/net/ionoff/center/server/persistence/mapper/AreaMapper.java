package net.ionoff.center.server.persistence.mapper;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;

import net.ionoff.center.server.entity.Area;
import net.ionoff.center.server.entity.Project;
import net.ionoff.center.server.entity.Zone;
import net.ionoff.center.server.persistence.service.IProjectService;
import net.ionoff.center.shared.dto.AreaDto;
import net.ionoff.center.shared.dto.ZoneDto;

public class AreaMapper implements ObjMapper<Area, AreaDto> {
	
	@Autowired
	private ZoneMapper zoneMapper;
	
	@Override
	public AreaDto createDto(Area area) {
		final AreaDto areaDto = new AreaDto();
		areaDto.setId(area.getId());
		areaDto.setName(area.getName());
		areaDto.setOrder(area.getOrder());
		areaDto.setProjectId(area.getProject().getId());
		return areaDto;
	}

	public AreaDto createAreaDto(Area area, boolean includingZone, boolean includingDevice) {
		final AreaDto areaDto = createDto(area);
		if (includingZone) {
			final List<ZoneDto> zoneDtos = new ArrayList<ZoneDto>();
			if (area.getZones() != null) {
				for (final Zone zone : area.getZones()) {
					zoneDtos.add(zoneMapper.createZoneDto(zone, includingDevice));
				}
			}
			areaDto.setZones(zoneDtos);
		}
		return areaDto;
	}

	public List<AreaDto> createArrayListAreaDto(Collection<Area> areas) {
		final List<AreaDto> areaDtos = new ArrayList<AreaDto>();
		for (final Area area : areas) {
			areaDtos.add(createDto(area));
		}
		return areaDtos;
	}
	
	public Area createArea(AreaDto areaDto, IProjectService projectService) {
		Area area = new Area();
		final Project project = projectService.findById(areaDto.getProjectId());
		area.setProject(project);
		area.setName(areaDto.getName());
		area.setOrder(areaDto.getOrder());
		return area;
	}
	
	public Area updateArea(Area area, AreaDto areaDto) {
		area.setName(areaDto.getName());
		area.setOrder(areaDto.getOrder());
		return area;
	}
	
	public List<AreaDto> createArrayListAreaDto(List<Area> areas, boolean includingZone, boolean includingDevice) {
		final List<AreaDto> areaDtos = new ArrayList<AreaDto>();
		for (final Area area : areas) {
			areaDtos.add(createAreaDto(area, includingZone, includingDevice));
		}
		return areaDtos;
	}

}
