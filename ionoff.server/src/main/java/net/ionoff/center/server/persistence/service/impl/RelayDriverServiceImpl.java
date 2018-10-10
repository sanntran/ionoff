package net.ionoff.center.server.persistence.service.impl;

import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Relay;
import net.ionoff.center.server.entity.RelayDriver;
import net.ionoff.center.server.entity.Sensor;
import net.ionoff.center.server.entity.SensorStatus;
import net.ionoff.center.server.entity.Switch;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.exception.DeleteEntityException;
import net.ionoff.center.server.exception.UpdateEntityException;
import net.ionoff.center.server.locale.Constants;
import net.ionoff.center.server.locale.Messages;
import net.ionoff.center.server.persistence.mapper.RelayDriverMapper;
import net.ionoff.center.server.persistence.dao.IProjectDao;
import net.ionoff.center.server.persistence.dao.IRelayDao;
import net.ionoff.center.server.persistence.dao.IRelayDriverDao;
import net.ionoff.center.server.persistence.dao.ISensorDao;
import net.ionoff.center.server.persistence.dao.ISensorStatusDao;
import net.ionoff.center.server.persistence.dao.ISwitchDao;
import net.ionoff.center.server.persistence.service.IRelayDriverService;
import net.ionoff.center.shared.dto.RelayDriverDto;

@Service
@Transactional
public class RelayDriverServiceImpl extends AbstractGenericService<RelayDriver, RelayDriverDto> 
		implements IRelayDriverService {
	
	private static final Logger logger = LoggerFactory.getLogger(RelayDriverServiceImpl.class.getName());
	
	private IRelayDriverDao relayDriverDao;
	
	@Autowired
	private RelayDriverMapper relayDriverMapper;
	
	@Autowired
	private IRelayDao relayDao;
	
	@Autowired
	private ISwitchDao switchDao;
	
	@Autowired
	private ISensorDao sensorDao;
	
	@Autowired
	private ISensorStatusDao sensorStatusDao;
	
	@Autowired
	private IProjectDao projectDao;

	@Autowired
	public RelayDriverServiceImpl(IRelayDriverDao relayDriverDao) {
		this.relayDriverDao = relayDriverDao;
	}

	@Override
	protected IRelayDriverDao getDao() {
		return relayDriverDao;
	}
	
	private IRelayDao getRelayDao() {
		return relayDao;
	}
	
	@Override
	public RelayDriver insert(RelayDriver relayDriver) {
		super.insert(relayDriver);
		if (relayDriver.isLazy()) {
			relayDriver.overrideKey();
			update(relayDriver);
		}
		insertRelays(relayDriver);
		insertSWitchs(relayDriver);
		return relayDriver;
	}

	private void insertSWitchs(RelayDriver relayDriver) {
		for (int i = 0; i < relayDriver.getInput(); i++) {
			insertSwitch(relayDriver, i);
		}
	}

	private void insertSwitch(RelayDriver relayDriver, int index) {
		Switch zwitch = new Switch();
		zwitch.setIndex(index);
		zwitch.setDriver(relayDriver);
		switchDao.insert(zwitch);
	}

	@Override
	public RelayDriverDto insertDto(User user, RelayDriverDto relayDriverDto) {
		final RelayDriver relayDriver = relayDriverMapper
				.createRelayDriver(relayDriverDto, projectDao.findById(relayDriverDto.getProjectId()));
		validateRelayDriver(relayDriver, user.getLanguage());
		insert(relayDriver);
		return relayDriverMapper.createRelayDriverDto(relayDriver);
	}

	@Override
	public RelayDriverDto updateDto(User user, RelayDriverDto relayDriverDto) {
		RelayDriver relayDriver = requireById(relayDriverDto.getId());
		relayDriverMapper.updateRelayDriver(relayDriver, relayDriverDto, 
				projectDao.findById(relayDriverDto.getProjectId()));
		validateRelayDriver(relayDriver, user.getLanguage());
		update(relayDriver);
		return relayDriverMapper.createRelayDriverDto(relayDriver);
	}
	

	private void validateRelayDriver(RelayDriver relayDriver, String locale) throws UpdateEntityException {
		if (relayDriver.getProtocol().equals("http")) {
			if (relayDriver.getPort() == null || relayDriver.getPort().intValue() == 0) {
				String message = Messages.get(locale).fieldInvalid(
									Constants.get(locale).port(), relayDriver.getPort() + "");
				throw new UpdateEntityException(message);
			}
			if (relayDriver.getIp() == null || relayDriver.getIp().isEmpty()) {
				throw new UpdateEntityException(Messages.get(locale).fieldInvalid(
												Constants.get(locale).ip(), relayDriver.getIp()));
			}
			if (checkDuplicatedIp(relayDriver)) {
				throw new UpdateEntityException(Messages.get(locale).relayDriverIpDuplicated(relayDriver.getIp()));
			}
		}
		else if (!relayDriver.isValidKey()) {
			String message = Messages.get(locale).fieldInvalid(Constants.get(locale).key(), relayDriver.getKey());
			throw new UpdateEntityException(message);
		}
	}

	private Relay insertRelay(RelayDriver relayDriver, int index) {
		Relay relay = new Relay();
		relay.setVersion(0L);
		relay.setStatus(false);
		relay.setDriver(relayDriver);
		relay.setName("Relay " + (index + 1));
		relay.setIndex(index);
		relay.setAutoRevert(0);
		getRelayDao().insert(relay);
		
		logger.info("Insert relay: " + relay.getName() + ", index: " + relay.getIndex() 
				+ ", relayDriver: " + relayDriver.getId());
		return relay;
	}
	
	private Set<Relay> insertRelays(RelayDriver relayDriver) {
		Set<Relay> relays = new HashSet<Relay>();
		for (int i = 0; i < relayDriver.getOutput(); i++) {
			Relay relay = insertRelay(relayDriver, i);
			relays.add(relay);
		}
		return relays;
	}
	
	@Override
	public void deleteDtoById(User user, long entityId) {
		final RelayDriver relayDriver = findById(entityId);
		if (relayDriver != null) {
			if (relayDriver.getProject().getRelayDrivers().size() < 2) {
				throw new DeleteEntityException(relayDriver.toString());
			}
		}
		delete(relayDriver);
	}


	@Override
	public List<RelayDriver> findByIsLazy() {
		return relayDriverDao.findByIsLazy();
	}

	@Override
	public List<RelayDriver> findByProjectId(long projectId) {
		return getDao().findByProjectId(projectId);
	}

	@Override
	public List<RelayDriver> findByMac(String mac) {
		return getDao().findByMac(mac);
	}

	@Override
	public List<RelayDriver> findByIp(String ip) {
		return getDao().findByIp(ip);
	}

	private boolean checkDuplicatedIp(RelayDriver relayDriver) {
		List<RelayDriver> relayDrivers = findByIp(relayDriver.getIp());
		for (final RelayDriver ctrl : relayDrivers) {
			if (ctrl.getId() != relayDriver.getId() && ctrl.getIp().equals(relayDriver.getIp()) 
					&& ctrl.getPort().equals(ctrl.getPort())) {
				return true;
			}
		}
		return false;
	}

	@Override
	public RelayDriverDto requireDtoById(long id) {
		return relayDriverMapper.createRelayDriverDto(requireById(id));
	}

	@Override
	public List<RelayDriverDto> findDtoByProjectId(Long projectId) {
		List<RelayDriver> relayDrivers = findByProjectId(projectId);
		return relayDriverMapper.createRelayDriverDtoList(relayDrivers);
	}

	@Override
	protected List<RelayDriverDto> createDtoList(List<RelayDriver> entities) {
		return relayDriverMapper.createRelayDriverDtoList(entities);
	}

	@Override
	public void insertSwitches(RelayDriver relayDriver) {
		if (relayDriver.getSwitchs() != null && !relayDriver.getSwitchs().isEmpty()) {
			return;
		}
		for (int i = 0; i < relayDriver.getInput(); i++) {
			Switch zwitch = new Switch();
			zwitch.setDriver(relayDriver);
			zwitch.setIndex(i);
			zwitch.setName("Switch-" + (i + 1));
		}
	}

	@Override
	public Switch updateSwitch(Switch zwitch) {
		switchDao.update(zwitch);
		List<Sensor> sensors = sensorDao.findBySwitchId(zwitch.getId());
		if (sensors == null || sensors.isEmpty()) {
			return zwitch;
		}
		for (Sensor sensor : sensors) {
			SensorStatus status = sensor.getStatus();
			if (status != null) {
				status.setTime(new Date());
				status.setValue(Boolean.TRUE.equals(zwitch.getStatus()) ? 1.0 : 0.0);
				sensorStatusDao.update(status);
			}
		}
		return zwitch;
	}

}
