package net.ionoff.center.server.persistence.mapper;

import java.util.ArrayList;
import java.util.List;

import net.ionoff.center.server.entity.Project;
import net.ionoff.center.server.entity.Relay;
import net.ionoff.center.server.entity.Controller;
import net.ionoff.center.shared.dto.ControllerDto;
import net.ionoff.center.shared.dto.RelayDto;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public class ControllerMapper {

	private static final Logger LOGGER = LoggerFactory.getLogger(ControllerMapper.class.getName());

	
	public Controller updateController(Controller controller,
                                        ControllerDto controllerDto, Project project) {
		controller.setName(controllerDto.getName());
		controller.setIp(controllerDto.getIp());
		controller.setPort(controllerDto.getPort());
		controller.setKey(controllerDto.getKey());
		controller.setProject(project);
		return controller;
	}
	
	public Controller createController(ControllerDto controllerDto, Project project) {
		Controller controller = newController(controllerDto.getModel());
		updateController(controller, controllerDto, project);
		return controller;
	}

	private Controller newController(String model) {
		Class clazz = Controller.MODELS.get(model);
		if (clazz == null) {
			throw new RuntimeException("No relay driver class map with model " + model);
		}
		else {
			try {
				Controller controller = (Controller) clazz.newInstance();
				return controller;
			} catch (Exception e) {
				LOGGER.error("Error create instanse for class " + clazz.getName());
				throw new RuntimeException("Error create instanse for class " + clazz.getName());
			}
		}
	}

	public List<ControllerDto> createControllerDtoList(List<Controller> controllers) {
		final List<ControllerDto> controllerDtos = new ArrayList<ControllerDto>();
		for (final Controller controller : controllers) {
			ControllerDto controllerDto = createControllerDto(controller);
			controllerDto.setRelays(new ArrayList<>());
			for (Relay relay : controller.getRelays()) {
				RelayDto relayDto = new RelayDto();
				relayDto.setId(relay.getId());
				relayDto.setName(relay.getName());
				if (relay.getDevice() != null) {
					relayDto.setDeviceId(relay.getDevice().getId());
					relayDto.setDeviceName(relay.getDevice().getName());
				}
				controllerDto.getRelays().add(relayDto);
			}
			controllerDtos.add(controllerDto);
			
		}
		return controllerDtos;
	}
	
	public ControllerDto createControllerDto(Controller controller) {
		final ControllerDto controllerDto = new ControllerDto();
		controllerDto.setId(controller.getId());
		controllerDto.setName(controller.getName());
		controllerDto.setIp(controller.getIp());
		controllerDto.setPort(controller.getPort());
		controllerDto.setKey(controller.getKey());
		controllerDto.setModel(controller.getModel());
		controllerDto.setProjectId(controller.getProject().getId());
		controllerDto.setIsOnline(controller.isConnected());
		return controllerDto;
	}
}
