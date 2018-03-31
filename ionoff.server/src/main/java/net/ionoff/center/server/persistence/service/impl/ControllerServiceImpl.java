package net.ionoff.center.server.persistence.service.impl;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Controller;
import net.ionoff.center.server.entity.Relay;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.exception.DeleteEntityException;
import net.ionoff.center.server.exception.UpdateEntityException;
import net.ionoff.center.server.locale.Constants;
import net.ionoff.center.server.locale.Messages;
import net.ionoff.center.server.objmapper.ControllerMapper;
import net.ionoff.center.server.persistence.dao.IControllerDao;
import net.ionoff.center.server.persistence.dao.IRelayDao;
import net.ionoff.center.server.persistence.service.IControllerService;
import net.ionoff.center.shared.dto.ControllerDto;
import net.ionoff.center.shared.entity.ControllerModel;

@Transactional
public class ControllerServiceImpl extends AbstractGenericService<Controller, ControllerDto> 
		implements IControllerService {
	
	private static final Logger logger = Logger.getLogger(ControllerServiceImpl.class.getName());
	
	private IControllerDao controllerDao;
	
	@Autowired
	private ControllerMapper controllerMapper;
	
	@Autowired
	private IRelayDao relayDao;
	
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
		insertRelays(controller);
		return controller;
	}
	

	@Override
	public ControllerDto insertDto(User user, ControllerDto controllerDto) {
		final Controller controller = controllerMapper.createController(controllerDto);
		validateController(controller, user.getLanguage());
		insert(controller);
		return controllerMapper.createControllerDto(controller);
	}

	@Override
	public ControllerDto updateDto(User user, ControllerDto controllerDto) {
		Controller controller = requireById(controllerDto.getId());
		controllerMapper.updateController(controller, controllerDto);
		validateController(controller, user.getLanguage());
		update(controller);
		return controllerMapper.createControllerDto(controller);
	}
	

	private void validateController(Controller controller, String locale) throws UpdateEntityException {
		
		if (ControllerModel.IONOFF_E4.toString().equals(controller.getModel().toString()) ||
				ControllerModel.IONOFF_P4.toString().equals(controller.getModel().toString()) ||
				ControllerModel.IONOFF_P8.toString().equals(controller.getModel().toString())) {
			// does not validate ip and port
			if (!controller.isValidKey()) {
				String message = Messages.get(locale).fieldInvalid(Constants.get(locale).key(), controller.getKey() + "");
				throw new UpdateEntityException(message);
			}
		}
		else {
			if (controller.getPort() == null || controller.getPort().intValue() == 0) {
				String message = Messages.get(locale).fieldInvalid(Constants.get(locale).port(), controller.getPort() + "");
				throw new UpdateEntityException(message);
			}
			if (controller.getIp() == null || controller.getIp().isEmpty()) {
				throw new UpdateEntityException(Messages.get(locale).fieldInvalid(Constants.get(locale).ip(), controller.getIp()));
			}
			if (checkDuplicatedIp(controller)) {
				throw new UpdateEntityException(Messages.get(locale).controllerIpDuplicated(controller.getIp()));
			}
		}
	}

	private Relay insertRelay(Controller controller, int index) {
		Relay relay = new Relay();
		relay.setVersion(0L);
		relay.setStatus(false);
		relay.setController(controller);
		relay.setName("Relay " + (index + 1));
		relay.setIndex(index);
		relay.setType(Relay.SWITCH);
		getRelayDao().insert(relay);
		
		logger.info("Insert relay: " + relay.getName() + ", index: " + relay.getIndex() 
				+ ", controller: " + controller.getId());
		return relay;
	}
	
	private Set<Relay> insertRelays(Controller controller) {
		Set<Relay> relays = new HashSet<Relay>();
		for (int i = 0; i < controller.getModelObj().getRelayOutput(); i++) {
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
		super.delete(controller);
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
}
