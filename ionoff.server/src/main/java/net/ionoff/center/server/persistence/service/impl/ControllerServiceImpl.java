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
import net.ionoff.center.server.entity.Controller;
import net.ionoff.center.server.entity.Sensor;
import net.ionoff.center.server.entity.SensorStatus;
import net.ionoff.center.server.entity.Switch;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.exception.DeleteEntityException;
import net.ionoff.center.server.exception.UpdateEntityException;
import net.ionoff.center.server.locale.Constants;
import net.ionoff.center.server.locale.Messages;
import net.ionoff.center.server.persistence.mapper.ControllerMapper;
import net.ionoff.center.server.persistence.dao.IProjectDao;
import net.ionoff.center.server.persistence.dao.IRelayDao;
import net.ionoff.center.server.persistence.dao.IControllerDao;
import net.ionoff.center.server.persistence.dao.ISensorDao;
import net.ionoff.center.server.persistence.dao.ISensorStatusDao;
import net.ionoff.center.server.persistence.dao.ISwitchDao;
import net.ionoff.center.server.persistence.service.IControllerService;
import net.ionoff.center.shared.dto.ControllerDto;

@Service
@Transactional
public class ControllerServiceImpl extends AbstractGenericService<Controller, ControllerDto>
		implements IControllerService {
	
	private static final Logger logger = LoggerFactory.getLogger(ControllerServiceImpl.class.getName());
	
	private IControllerDao controllerDao;
	
	@Autowired
	private ControllerMapper controllerMapper;
	
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
	public ControllerServiceImpl(IControllerDao controllerDao) {
		this.controllerDao = controllerDao;
	}

	@Override
	protected IControllerDao getDao() {
		return controllerDao;
	}
	
	private IRelayDao getRelayDao() {
		return relayDao;
	}
	
	@Override
	public Controller insert(Controller controller) {
		super.insert(controller);
		if (controller.isLazy()) {
			controller.overrideKey();
			update(controller);
		}
		insertRelays(controller);
		insertSWitchs(controller);
		return controller;
	}

	private void insertSWitchs(Controller controller) {
		for (int i = 0; i < controller.getInput(); i++) {
			insertSwitch(controller, i);
		}
	}

	private void insertSwitch(Controller controller, int index) {
		Switch zwitch = new Switch();
		zwitch.setIndex(index);
		zwitch.setDriver(controller);
		switchDao.insert(zwitch);
	}

	@Override
	public ControllerDto insertDto(User user, ControllerDto controllerDto) {
		final Controller controller = controllerMapper
				.createController(controllerDto, projectDao.findById(controllerDto.getProjectId()));
		validateController(controller, user.getLanguage());
		insert(controller);
		return controllerMapper.createControllerDto(controller);
	}

	@Override
	public ControllerDto updateDto(User user, ControllerDto controllerDto) {
		Controller controller = requireById(controllerDto.getId());
		controllerMapper.updateController(controller, controllerDto,
				projectDao.findById(controllerDto.getProjectId()));
		validateController(controller, user.getLanguage());
		update(controller);
		return controllerMapper.createControllerDto(controller);
	}
	

	private void validateController(Controller controller, String locale) throws UpdateEntityException {
		if (controller.getProtocol().equals("http")) {
			if (controller.getPort() == null || controller.getPort().intValue() == 0) {
				String message = Messages.get(locale).fieldInvalid(
									Constants.get(locale).port(), controller.getPort() + "");
				throw new UpdateEntityException(message);
			}
			if (controller.getIp() == null || controller.getIp().isEmpty()) {
				throw new UpdateEntityException(Messages.get(locale).fieldInvalid(
												Constants.get(locale).ip(), controller.getIp()));
			}
			if (checkDuplicatedIp(controller)) {
				throw new UpdateEntityException(Messages.get(locale).controllerIpDuplicated(controller.getIp()));
			}
		}
		else if (!controller.isValidKey()) {
			String message = Messages.get(locale).fieldInvalid(Constants.get(locale).key(), controller.getKey());
			throw new UpdateEntityException(message);
		}
	}

	private Relay insertRelay(Controller controller, int index) {
		Relay relay = new Relay();
		relay.setVersion(0L);
		relay.setStatus(false);
		relay.setDriver(controller);
		relay.setName("Relay " + (index + 1));
		relay.setIndex(index);
		relay.setAutoRevert(0);
		getRelayDao().insert(relay);
		
		logger.info("Insert relay: " + relay.getName() + ", index: " + relay.getIndex() 
				+ ", controller: " + controller.getId());
		return relay;
	}
	
	private Set<Relay> insertRelays(Controller controller) {
		Set<Relay> relays = new HashSet<Relay>();
		for (int i = 0; i < controller.getOutput(); i++) {
			Relay relay = insertRelay(controller, i);
			relays.add(relay);
		}
		return relays;
	}
	
	@Override
	public void deleteDtoById(User user, long entityId) {
		final Controller controller = findById(entityId);
		if (controller != null) {
			if (controller.getProject().getControllers().size() < 2) {
				throw new DeleteEntityException(controller.toString());
			}
		}
		delete(controller);
	}


	@Override
	public List<Controller> findByIsLazy() {
		return controllerDao.findByIsLazy();
	}

	@Override
	public List<Controller> findByProjectId(long projectId) {
		return getDao().findByProjectId(projectId);
	}

	@Override
	public List<Controller> findByMac(String mac) {
		return getDao().findByMac(mac);
	}

	@Override
	public List<Controller> findByIp(String ip) {
		return getDao().findByIp(ip);
	}

	private boolean checkDuplicatedIp(Controller controller) {
		List<Controller> controllers = findByIp(controller.getIp());
		for (final Controller ctrl : controllers) {
			if (ctrl.getId() != controller.getId() && ctrl.getIp().equals(controller.getIp())
					&& ctrl.getPort().equals(ctrl.getPort())) {
				return true;
			}
		}
		return false;
	}

	@Override
	public ControllerDto requireDtoById(long id) {
		return controllerMapper.createControllerDto(requireById(id));
	}

	@Override
	public List<ControllerDto> findDtoByProjectId(Long projectId) {
		List<Controller> controllers = findByProjectId(projectId);
		return controllerMapper.createControllerDtoList(controllers);
	}

	@Override
	protected List<ControllerDto> createDtoList(List<Controller> entities) {
		return controllerMapper.createControllerDtoList(entities);
	}

	@Override
	public void insertSwitches(Controller controller) {
		if (controller.getSwitchs() != null && !controller.getSwitchs().isEmpty()) {
			return;
		}
		for (int i = 0; i < controller.getInput(); i++) {
			Switch zwitch = new Switch();
			zwitch.setDriver(controller);
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
