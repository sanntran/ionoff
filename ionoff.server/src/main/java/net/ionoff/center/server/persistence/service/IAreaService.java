package net.ionoff.center.server.persistence.service;

import net.ionoff.center.server.entity.Area;
import net.ionoff.center.server.entity.AreaCell;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.exception.DeleteEntityException;
import net.ionoff.center.server.locale.Constants;
import net.ionoff.center.server.locale.Messages;
import net.ionoff.center.server.persistence.dao.IAreaDao;
import net.ionoff.center.server.persistence.mapper.AreaMapper;
import net.ionoff.center.server.persistence.service.impl.AbstractGenericService;
import net.ionoff.center.shared.dto.AreaDto;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
@Transactional
public class IAreaService extends AbstractGenericService<Area, AreaDto> {

	private IAreaDao areaDao;

	@Autowired
	private AreaMapper areaMapper;

	@Autowired
	private IProjectService projectService;

	@Autowired
	public IAreaService(IAreaDao areaDao) {
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

	public List<Area> findByProjectId(long projectId) {
		List<Area> areas = getDao().findByProjectId(projectId);
		return areas;
	}

	public List<AreaDto> findDtoByProjectId(long projectId) {
		List<Area> areas = getDao().findByProjectId(projectId);
		return areaMapper.createArrayListAreaDto(areas);
	}

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

	public List<AreaCell> findForGridInProject(Long projectId) {
		return getDao().findForGridInProject(projectId);
	}

}
