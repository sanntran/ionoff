package net.ionoff.center.server.persistence.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Relay;
import net.ionoff.center.server.entity.RelayGroup;
import net.ionoff.center.server.entity.RelayGroupRelay;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.exception.EntityNotFoundException;
import net.ionoff.center.server.persistence.mapper.RelayMapper;
import net.ionoff.center.server.persistence.dao.IRelayGroupDao;
import net.ionoff.center.server.persistence.dao.IRelayGroupRelayDao;
import net.ionoff.center.server.persistence.service.IRelayGroupService;
import net.ionoff.center.server.persistence.service.IRelayService;
import net.ionoff.center.shared.dto.RelayDto;
import net.ionoff.center.shared.dto.RelayGroupDto;

@Transactional
public class RelayGroupServiceImpl extends AbstractGenericService<RelayGroup, RelayGroupDto> implements IRelayGroupService {

	private IRelayGroupDao relayGroupDao;
	
	@Autowired
	private IRelayService relayService;
	
	@Autowired
	private IRelayGroupRelayDao relayGroupRelayDao;
	
	@Autowired
	private RelayMapper relayMapper;
	
	public RelayGroupServiceImpl(IRelayGroupDao relayGroupDao) {
		this.relayGroupDao = relayGroupDao;
	}

	@Override
	protected IRelayGroupDao getDao() {
		return relayGroupDao;
	}

	@Override
	public RelayGroupDto requireDtoById(long id) {
		throw new UnsupportedOperationException();
	}

	@Override
	public RelayGroupDto insertDto(User user, RelayGroupDto dto) {
		throw new UnsupportedOperationException();
	}

	@Override
	public RelayGroupDto updateDto(User user, RelayGroupDto dto) {
		throw new UnsupportedOperationException();
	}

	@Override
	public void deleteDtoById(User user, long id) {
		RelayGroup relayGroup = requireById(id);
		super.delete(relayGroup);
	}

	@Override
	protected List<RelayGroupDto> createDtoList(List<RelayGroup> entities) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<RelayGroupDto> findByRelayId(Long relayId) {
		List<RelayGroup> relayGroups = relayGroupDao.findByRelayId(relayId);
		List<RelayGroupDto> relayGroupDtos = new ArrayList<>();
		for (RelayGroup relayGroup : relayGroups) {
			relayGroupDtos.add(relayMapper.createRelayGroupDto(relayGroup));
		}
		return relayGroupDtos;
	} 
	
	@Override
	public RelayGroupDto addRelayToGroup(Long groupId, RelayDto relayDto) {
		RelayGroup relayGroup = requireById(groupId);
		Relay relayToAdd = relayService.findById(relayDto.getId());
		if (relayToAdd == null) {
			throw new EntityNotFoundException("Relay id " + relayDto.getId() + " is not found");
		}
		RelayGroupRelay relayGroupRelay = new RelayGroupRelay();
		relayGroupRelay.setGroup(relayGroup);
		relayGroupRelay.setRelay(relayToAdd);
		relayGroupRelayDao.insert(relayGroupRelay);
		relayGroup.getGroupRelays().add(relayGroupRelay);
		return relayMapper.createRelayGroupDto(relayGroup);
	}
	
	@Override
	public RelayGroupDto removeRelayFromGroup(Long groupId, Long relayId) {
		RelayGroupRelay relayGroupRelay = relayGroupRelayDao.findByRelayIdGroupId(relayId, groupId);
		if (relayGroupRelay == null) {
			throw new EntityNotFoundException(relayId + " - " + groupId, 
					RelayGroupRelay.class.getSimpleName());
		}
		relayGroupRelayDao.delete(relayGroupRelay);
		RelayGroup relayGroup = relayGroupDao.findById(groupId);
		if (relayGroup.getGroupRelays() != null && relayGroup.getGroupRelays().contains(relayGroupRelay)) {
			relayGroup.getGroupRelays().remove(relayGroupRelay);
		}
		return relayMapper.createRelayGroupDto(relayGroup);
	}

	@Override
	public RelayGroupDto createRelayGroup(User user, Long relayId) {
		
		Relay relayToAdd = relayService.findById(relayId);
		if (relayToAdd == null) {
			throw new EntityNotFoundException("Relay id " + relayId + " is not found");
		}
		RelayGroup relayGroup = new RelayGroup();
		relayGroup.setProject(relayToAdd.getDriver().getProject());
		relayGroupDao.insert(relayGroup);
		
		if (relayToAdd != null) {
			RelayGroupRelay relayGroupRelay = new RelayGroupRelay();
			relayGroupRelay.setGroup(relayGroup);
			relayGroupRelay.setRelay(relayToAdd);
			relayGroupRelayDao.insert(relayGroupRelay);
			if (relayGroup.getGroupRelays() == null) {
				relayGroup.setGroupRelays(new ArrayList<>());
			}
			relayGroup.getGroupRelays().add(relayGroupRelay);
		}
		return relayMapper.createRelayGroupDto(relayGroup);
	}

	@Override
	public void updateRelayLeader(User user, Long groupId, Long relayId, Boolean isLeader) {
		RelayGroupRelay groupRelay = relayGroupRelayDao.findByRelayIdGroupId(relayId, groupId);
		if (groupRelay == null) {
			throw new EntityNotFoundException("Relay-Id " + relayId + " - Group-Id " + groupId + " is not found");
		}
		groupRelay.setIsLeader(isLeader);
		relayGroupRelayDao.update(groupRelay);
	}
}
