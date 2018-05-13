package net.ionoff.center.server.persistence.service.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Area;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.exception.DeleteEntityException;
import net.ionoff.center.server.locale.Constants;
import net.ionoff.center.server.locale.Messages;
import net.ionoff.center.server.objmapper.AreaMapper;
import net.ionoff.center.server.persistence.dao.IAreaDao;
import net.ionoff.center.server.persistence.service.IAreaService;
import net.ionoff.center.server.persistence.service.IProjectService;
import net.ionoff.center.shared.dto.AreaDto;

@Transactional
public class AreaServiceImpl extends AbstractGenericService<Area, AreaDto> implements IAreaService {
	
	private IAreaDao areaDao;
	
	@Autowired
	private AreaMapper areaMapper;
	
	@Autowired 
	private IProjectService projectService;
	
	public AreaServiceImpl(IAreaDao areaDao) {
		this.areaDao = areaDao;
	}
	
	@Override
	protected IAreaDao getDao() {
		return areaDao;
	}
	
	@Override
	public AreaDto insertDto(User user, AreaDto areaDto) {
		final Area area = areaMapper.createArea(areaDto, projectService);
		insert(area);
		return areaMapper.createDto(area);
	}

	@Override
	public AreaDto updateDto(User user, AreaDto areaDto) {
		Area area = requireById(areaDto.getId());
		areaMapper.updateArea(area, areaDto);
		update(area);
		return areaMapper.createDto(area);
	}
	
	@Override
	public List<Area> findByProjectId(long projectId) {
		List<Area> areas = getDao().findByProjectId(projectId);
		return areas;
	}
	
	@Override
	public List<AreaDto> findDtoByProjectId(long projectId) {
		List<Area> areas = getDao().findByProjectId(projectId);
		return areaMapper.createArrayListAreaDto(areas);
	}

	@Override
	public List<AreaDto> findByProjectId(Long projectId, Boolean includingZone, Boolean includingDevice) {
		List<Area> areas = getDao().findByProjectId(projectId);
		return areaMapper.createArrayListAreaDto(areas, includingZone, includingDevice);
	}

	@Override
	public AreaDto requireDtoById(long id) {
		return areaMapper.createDto(requireById(id));
	}

	@Override
	public void deleteDtoById(User user, long id) {
		Area area = requireById(id);
		if (area.getZones() != null && !area.getZones().isEmpty()) {
			final String entity = Constants.get(user.getLanguage()).area();
			final String ownedEntity = Constants.get(user.getLanguage()).zone();
			final String message = Messages.get(user.getLanguage()).errorDeleteNotEmptyEntity(entity, ownedEntity);
			throw new DeleteEntityException(message);
		}
		delete(area);
	}

	@Override
	protected List<AreaDto> createDtoList(List<Area> entities) {
		return areaMapper.createArrayListAreaDto(entities);
	}

}
