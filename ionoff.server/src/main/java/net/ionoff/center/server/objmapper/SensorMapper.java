package net.ionoff.center.server.objmapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;

import net.ionoff.center.server.entity.Sensor;
import net.ionoff.center.server.exception.UpdateEntityException;
import net.ionoff.center.server.persistence.service.IControllerService;
import net.ionoff.center.server.persistence.service.IProjectService;
import net.ionoff.center.shared.dto.SensorDto;

public class SensorMapper {

	@Autowired
	private IControllerService controllerService;
	
	@Autowired
	private IProjectService projectService;
	
	public List<SensorDto> createSerialDtoList(List<Sensor> sensors) {
		final List<SensorDto> sensorDtos = new ArrayList<SensorDto>();
		for (final Sensor sensor : sensors) {
			sensorDtos.add(createSensorDto(sensor));
		}
		return sensorDtos;
	}
	
	public Sensor createSensor(SensorDto sensorDto) {
		Sensor sensor = new Sensor();
		sensor.setProject(projectService.findById(sensorDto.getProjectId()));
		updateSensor(sensor, sensorDto);
		return sensor;
	}
	
	public Sensor updateSensor(Sensor sensor , SensorDto sensorDto) throws UpdateEntityException {
		if (sensorDto.getControllerInput() == null 
				&& sensorDto.getControllerInput().intValue() == SensorDto.NULL_INPUT) {
			sensor.setControllerInput(sensorDto.getControllerInput());
		}
		else {
			sensor.setControllerInput(sensorDto.getControllerInput() - 1);
		}
		
		sensor.setName(sensorDto.getName());
		if (sensorDto.getControllerId() != null) {
			sensor.setController(controllerService.findById(sensorDto.getControllerId()));
		}
		else {
			sensor.setController(null);
		}
		return sensor;
	}

	public SensorDto createSensorDto(Sensor sensor) {
		final SensorDto sensorDto = new SensorDto();
		sensorDto.setId(sensor.getId());
		sensorDto.setName(sensor.getName());
		if (sensor.getController() != null) {
			sensorDto.setControllerId(sensor.getController().getId());
			sensorDto.setControllerName(sensor.getController().getName());
		}
		if (sensor.getControllerInput() != null && sensor.getControllerInput().intValue() != Sensor.NULL_INPUT) {
			sensorDto.setControllerInput(sensor.getControllerInput() + 1);
		}
		else {
			sensorDto.setControllerInput(sensor.getControllerInput());
		}
		sensorDto.setProjectId(sensor.getProject().getId());
		return sensorDto;
	}
}
