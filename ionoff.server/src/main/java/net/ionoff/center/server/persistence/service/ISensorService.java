package net.ionoff.center.server.persistence.service;

import net.ionoff.center.server.entity.Sensor;
import net.ionoff.center.server.entity.SensorData;
import net.ionoff.center.server.entity.SensorStatus;
import net.ionoff.center.shared.dto.QueryCriteriaDto;
import net.ionoff.center.shared.dto.SensorDataDto;
import net.ionoff.center.shared.dto.SensorDto;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Transactional
public interface ISensorService extends IGenericService<Sensor, SensorDto> {
	
	List<Sensor> findByProjectId(long projectId);
	
	List<Sensor> findByDeviceId(long deviceId);

	List<SensorDto> findDtoByProjectId(Long projectId);

	void updateStatus(SensorStatus sensorStatus);

	SensorData insertSensorData(SensorStatus newStatus);

	void insertSensorStatus(SensorStatus sensorStatus);

    Long countDataByCriteria(QueryCriteriaDto criteriaDto);

	List<SensorDataDto> searchDataByCriteria(QueryCriteriaDto criteriaDto);

	List<SensorDataDto> loadDataByDay(QueryCriteriaDto criteriaDto);
}
