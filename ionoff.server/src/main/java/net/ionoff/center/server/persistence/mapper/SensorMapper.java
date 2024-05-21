package net.ionoff.center.server.persistence.mapper;

import java.util.ArrayList;
import java.util.List;

import net.ionoff.center.server.entity.Project;
import net.ionoff.center.server.entity.Sensor;
import net.ionoff.center.server.entity.SensorData;
import net.ionoff.center.server.util.DateTimeUtil;
import net.ionoff.center.shared.dto.SensorDataDto;
import net.ionoff.center.shared.dto.SensorDto;
import org.springframework.stereotype.Component;

@Component
public class SensorMapper {

	public List<SensorDto> createSerialDtoList(List<Sensor> sensors) {
		final List<SensorDto> sensorDtos = new ArrayList<SensorDto>();
		for (final Sensor sensor : sensors) {
			sensorDtos.add(createSensorDto(sensor));
		}
		return sensorDtos;
	}
	
	public Sensor createSensor(SensorDto sensorDto, Project project) {
		Sensor sensor = new Sensor();
		updateSensor(sensor, sensorDto);
		sensor.setProject(project);
		return sensor;
	}
	
	public Sensor updateSensor(Sensor sensor, SensorDto sensorDto) {
		sensor.setOrder(sensorDto.getOrder());
		sensor.setType(sensorDto.getType());
		sensor.setUnit(sensorDto.getUnit());
		sensor.setName(sensorDto.getName());
		return sensor;
	}

	public SensorDto createSensorDto(Sensor sensor) {
		final SensorDto sensorDto = new SensorDto();
		sensorDto.setId(sensor.getId());
		sensorDto.setName(sensor.getName());
		sensorDto.setOrder(sensor.getOrder());
		if (sensor.getDevice() != null) {
			sensorDto.setIndex(null);
			sensorDto.setDeviceId(sensor.getDevice().getId());
			sensorDto.setDeviceName(sensor.getDevice().getName());
		}
		if (sensor.getZone() != null) {
			sensorDto.setZoneId(sensor.getZone().getId());
			sensorDto.setZoneName(sensor.getZone().getName());
			sensorDto.setAreaId(sensor.getZone().getArea().getId());
			sensorDto.setAreaName(sensor.getZone().getArea().getName());
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
