package net.ionoff.center.server.objmapper;

import java.util.ArrayList;
import java.util.List;

import net.ionoff.center.server.entity.SensorData;
import net.ionoff.center.server.util.DateTimeUtil;
import net.ionoff.center.shared.dto.SensorDataDto;
import org.springframework.beans.factory.annotation.Autowired;

import net.ionoff.center.server.entity.Controller;
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
		sensor.setType(sensorDto.getType());
		sensor.setName(sensorDto.getName());
		if (sensorDto.getDriverId() != null) {
			Controller driver = controllerService.findById(sensorDto.getDriverId());
			driver.getSwitchs().get(sensorDto.getIndex());
			sensor.setProject(driver.getProject());
		}
		else {
			sensor.setZwitch(null);
		}
		return sensor;
	}

	public SensorDto createSensorDto(Sensor sensor) {
		final SensorDto sensorDto = new SensorDto();
		sensorDto.setId(sensor.getId());
		sensorDto.setName(sensor.getName());
		if (sensor.getDevice() != null) {
			sensorDto.setDriverId(sensor.getDevice().getId());
			sensorDto.setDriverName(sensor.getDevice().getName());
		}
		if (sensor.getZwitch() != null) {
			sensorDto.setDriverId(sensor.getZwitch().getDriver().getId());
			sensorDto.setDriverName(sensor.getZwitch().getDriver().getName());
			sensorDto.setIndex(sensor.getZwitch().getIndex() + 1);
		}
		else {
			sensorDto.setIndex(null);
		}
		sensorDto.setProjectId(sensor.getProject().getId());
		return sensorDto;
	}

    public List<SensorDataDto> createSensorDataDtoList(List<SensorData> sensorDataList) {
		List<SensorDataDto> sensorDataDtos = new ArrayList<>();
		for (SensorData data : sensorDataList) {
			sensorDataDtos.add(createSensorDataDto(data));
		}
		return sensorDataDtos;
    }

	public SensorDataDto createSensorDataDto(SensorData sensorData) {
		final SensorDataDto sensorDataDto = new SensorDataDto();
		sensorDataDto.setId(sensorData.getId());
		sensorDataDto.setName(sensorData.getName());
		if (sensorData.getTime() != null) {
			sensorDataDto.setTime(DateTimeUtil.yyyyMMddHHmmssFormatter.format(sensorData.getTime()));
		}
		sensorDataDto.setValue(sensorData.getValue());
		sensorDataDto.setIndex(sensorData.getIndex());

		return sensorDataDto;
	}
}
