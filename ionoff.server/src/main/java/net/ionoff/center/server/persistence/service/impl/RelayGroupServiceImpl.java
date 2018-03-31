package net.ionoff.center.server.persistence.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Relay;
import net.ionoff.center.server.entity.RelayGroup;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.exception.EntityNotFoundException;
import net.ionoff.center.server.objmapper.RelayMapper;
import net.ionoff.center.server.persistence.dao.IRelayGroupDao;
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
		throw new UnsupportedOperationException();
	}

	@Override
	protected List<RelayGroupDto> createDtoList(List<RelayGroup> entities) {
		throw new UnsupportedOperationException();
	}

	@Override
	public RelayGroupDto findByRelayId(Long relayId) {
		Relay relay = relayService.findById(relayId);
		if (relay == null) {
			throw new EntityNotFoundException("Relay id " + relayId + " is not found");
		}
		if (relay.getGroup() == null) {
			throw new EntityNotFoundException("No group contains relay " + relayId);
		}
		return relayMapper.createRelayGroupDto(relay.getGroup());
	} 
	
	@Override
	public RelayGroupDto addRelayToGroup(Long relayId, RelayDto relayDto) {
		Relay relay = relayService.findById(relayId);
		if (relay == null) {
			throw new EntityNotFoundException("Relay id " + relayId + " is not found");
		}
		
		RelayGroup relayGroup = relay.getGroup();
		Relay relayToAdd = relayService.findById(relayDto.getId());
		if (relayToAdd == null) {
			throw new EntityNotFoundException("Relay id " + relayDto.getId() + " is not found");
		}
		
		if (relayGroup == null) {
			if (relayToAdd.getGroup() != null) {
				relayGroup = relayToAdd.getGroup();
				relay.setGroup(relayGroup);
				relayService.update(relay);
				if (relayGroup.getRelays() == null) {
					relayGroup.setRelays(new ArrayList<>());
				}
				relayGroup.getRelays().add(relay);
			}
			else {
				relayGroup = new RelayGroup();
				relayGroup.setProject(relay.getController().getProject());
				insert(relayGroup);
				
				relay.setGroup(relayGroup);
				relayService.update(relay);
				if (relayGroup.getRelays() == null) {
					relayGroup.setRelays(new ArrayList<>());
				}
				relayGroup.getRelays().add(relay);
			}
		}
		relayToAdd.setGroup(relayGroup);
		if (!relayGroup.getRelays().contains(relayToAdd)) {
			relayGroup.getRelays().add(relayToAdd);
		}
		relayService.update(relayToAdd);
		update(relayGroup);
		return relayMapper.createRelayGroupDto(relay.getGroup());
	}
	
	@Override
	public RelayGroupDto removeRelayFromGroup(Long relayId, Long relayIdToRemove) {
		Relay relay = relayService.findById(relayId);
		
		if (relay == null) {
			throw new EntityNotFoundException("Relay id " + relayId + " is not found");
		}
		RelayGroup relayGroup = relay.getGroup();
		if (relayGroup == null) {
			throw new EntityNotFoundException("No group contains relay " + relayId);
		}
		Relay relayToRemove = relayService.findById(relayIdToRemove);
		if (relayToRemove == null) {
			throw new EntityNotFoundException("Relay id " + relayIdToRemove + " is not found");
		}
		relayToRemove.setGroup(null);
		relayService.update(relayToRemove);
		
		if (relayGroup.getRelays() != null) {
			relayGroup.getRelays().remove(relayToRemove);
			if (relayGroup.getRelays().size() == 1) {
				relayGroup.getRelays().get(0).setGroup(null);
				relayService.update(relayGroup.getRelays().get(0));
				relayGroup.getRelays().remove(0);
			}
			update(relayGroup);
		}
		
		return relayMapper.createRelayGroupDto(relayGroup);
	}
	
}
