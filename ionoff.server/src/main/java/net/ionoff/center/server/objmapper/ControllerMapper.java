package net.ionoff.center.server.objmapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;

import net.ionoff.center.server.entity.Controller;
import net.ionoff.center.server.entity.Relay;
import net.ionoff.center.server.persistence.service.IProjectService;
import net.ionoff.center.shared.dto.ControllerDto;
import net.ionoff.center.shared.dto.RelayDto;

public class ControllerMapper {
	
	@Autowired
	private IProjectService projectService;
	
	@Autowired
	private RelayMapper relayMapper;
	
	public Controller updateController(Controller controller, ControllerDto controllerDto) {
		controller.setName(controllerDto.getName());
		controller.setIp(controllerDto.getIp());
		controller.setPort(controllerDto.getPort());
		controller.setKey(controllerDto.getKey());
		controller.setModel(controllerDto.getModel().toString());
		controller.setProject(projectService.findById(controllerDto.getProjectId()));
		return controller;
	}
	
	public Controller createController(ControllerDto controllerDto) {
		Controller controller = new Controller();
		updateController(controller, controllerDto);
		return controller;
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
		controllerDto.setModel(controller.getModelObj());
		controllerDto.setProjectId(controller.getProject().getId());
		controllerDto.setIsOnline(controller.isConnected());
		return controllerDto;
	}
}
