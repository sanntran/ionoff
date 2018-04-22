package net.ionoff.center.server.persistence.service;

import java.util.List;

import net.ionoff.center.server.entity.SensorData;
import net.ionoff.center.shared.dto.QueryCriteriaDto;
import net.ionoff.center.shared.dto.SensorDataDto;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Sensor;
import net.ionoff.center.server.entity.SensorStatus;
import net.ionoff.center.shared.dto.SensorDto;

@Transactional
public interface ISensorService extends IGenericService<Sensor, SensorDto> {
	
	List<Sensor> findByProjectId(long projectId);

	List<Sensor> findBySwitchId(long switchId);
	
	List<Sensor> findByDeviceId(long deviceId);

	List<SensorDto> findDtoByProjectId(Long projectId);

	void updateStatus(SensorStatus sensorStatus);

	SensorData insertSensorData(SensorStatus newStatus);

	void insertSensorStatus(SensorStatus sensorStatus);

    Long countDataByCriteria(QueryCriteriaDto criteriaDto);

	List<SensorDataDto> searchDataByCriteria(QueryCriteriaDto criteriaDto);
}
