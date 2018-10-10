package net.ionoff.center.server.persistence.service.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Switch;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.persistence.dao.ISwitchDao;
import net.ionoff.center.server.persistence.service.ISwitchService;
import net.ionoff.center.shared.dto.SwitchDto;

@Service
@Transactional
public class SwitchServiceImpl extends AbstractGenericService<Switch, SwitchDto> implements ISwitchService {

	private ISwitchDao switchDao;

	@Autowired
	public SwitchServiceImpl(ISwitchDao switchDao) {
		this.switchDao = switchDao;
	}

	@Override
	protected ISwitchDao getDao() {
		return switchDao;
	}

	@Override
	public SwitchDto requireDtoById(long id) {
		throw new UnsupportedOperationException();
	}

	@Override
	public SwitchDto insertDto(User user, SwitchDto dto) {
		throw new UnsupportedOperationException();
	}

	@Override
	public SwitchDto updateDto(User user, SwitchDto dto) {
		throw new UnsupportedOperationException();
	}

	@Override
	public void deleteDtoById(User user, long id) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Switch findByDriverId(Long driverId, Integer index) {
		if (driverId == null) {
			return null;
		}
		return switchDao.findByDriverId(driverId, index);
	}

	@Override
	protected List<SwitchDto> createDtoList(List<Switch> entities) {
		throw new UnsupportedOperationException();
	}
	
}
