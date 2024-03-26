package net.ionoff.center.server.persistence.service.impl;

import net.ionoff.center.server.entity.*;
import net.ionoff.center.server.exception.UpdateEntityException;
import net.ionoff.center.server.persistence.dao.*;
import net.ionoff.center.server.persistence.mapper.QueryCriteriaMapper;
import net.ionoff.center.server.persistence.mapper.SensorMapper;
import net.ionoff.center.server.persistence.service.ISensorService;
import net.ionoff.center.shared.dto.QueryCriteriaDto;
import net.ionoff.center.shared.dto.SensorDataDto;
import net.ionoff.center.shared.dto.SensorDto;
import net.ionoff.center.shared.entity.SensorType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
@Transactional
public class SensorServiceImpl extends AbstractGenericService<Sensor, SensorDto> implements ISensorService {

	private ISensorDao sensorDao;
	
	@Autowired
	private SensorMapper sensorMapper;
	
	@Lazy
	@Autowired 
	private IProjectDao projectDao;
	
	@Autowired
	private ISensorDataDao sensorDataDao;
	
	@Autowired
	private ISensorStatusDao sensorStatusDao;

	@Autowired
	public SensorServiceImpl(ISensorDao sensorDao) {
		this.sensorDao = sensorDao;
	}

	@Override
	protected ISensorDao getDao() {
		return sensorDao;
	}
	
	@Override
	public Sensor insert(Sensor sensor) {
		SensorStatus status = new SensorStatus();
		status.setSensor(sensor);
		sensor.setStatus(status);
		super.insert(sensor);
		insertSensorStatus(status);
		return sensor;
	}

	@Override
	public List<Sensor> findByProjectId(long projectId) {
		return getDao().findByProjectId(projectId);
	}

	@Override
	public SensorDto requireDtoById(long id) {
		return sensorMapper.createSensorDto(requireById(id));
	}

	@Override
	public SensorDto insertDto(User user, SensorDto dto) {
		validateSensor(dto, user.getLanguage());
		Project project = projectDao.findById(dto.getProjectId());
		Sensor sensor = sensorMapper.createSensor(dto, project);
		insert(sensor);
		return sensorMapper.createSensorDto(sensor);
	}

	@Override
	public SensorDto updateDto(User user, SensorDto dto) {
		validateSensor(dto, user.getLanguage());
		Sensor sensor = requireById(dto.getId());
		sensorMapper.updateSensor(sensor, dto);
		update(sensor);
		return sensorMapper.createSensorDto(sensor);
	}

	@Override
	public void deleteDtoById(User user, long id) {
		deleteById(id);
	}

	private void validateSensor(SensorDto sensorDto, String locale) throws UpdateEntityException {
		if (!SensorType.DIGITAL.toString().equals(sensorDto.getType())) {
			return;
		}
	}

	@Override
	public List<SensorDto> findDtoByProjectId(Long projectId) {
		List<Sensor> sensors = findByProjectId(projectId);
		return sensorMapper.createSerialDtoList(sensors);
	}

	@Override
	protected List<SensorDto> createDtoList(List<Sensor> entities) {
		return sensorMapper.createSerialDtoList(entities);
	}

	@Override
	public List<Sensor> findByDeviceId(long deviceId) {
		return sensorDao.findByDeviceId(deviceId);
	}

	@Override
	public void updateStatus(SensorStatus sensorStatus) {
		sensorStatusDao.update(sensorStatus);
	}

	@Override
	public SensorData insertSensorData(SensorStatus newStatus) {
		SensorData sensorData = new SensorData();
		sensorData.setSensor(newStatus.getSensor());
		sensorData.setTime(newStatus.getTime());
		sensorData.setValue(newStatus.getValue());
        sensorData.setIndex(newStatus.getIndex());
		sensorDataDao.insert(sensorData);
		return sensorData;
	}

	@Override
	public void insertSensorStatus(SensorStatus sensorStatus) {
		sensorStatusDao.insert(sensorStatus);
	}

	@Override
	public Long countDataByCriteria(QueryCriteriaDto criteriaDto) {
		return sensorDataDao.countByCriteria(QueryCriteriaMapper.toQueryCriteria(criteriaDto));
	}

	@Override
	public List<SensorDataDto> searchDataByCriteria(QueryCriteriaDto criteriaDto) {
		List<SensorData> sensorDataList = sensorDataDao.findByCriteria(QueryCriteriaMapper.toQueryCriteria(criteriaDto));
		return sensorMapper.createSensorDataDtoList(sensorDataList);
	}

	@Override
	public List<SensorDataDto> loadDataByDay(QueryCriteriaDto criteriaDto) {
		List<SensorData> sensorDataList = sensorDataDao.findByDay(QueryCriteriaMapper.toQueryCriteria(criteriaDto));
		return sensorMapper.createSensorDataDtoList(sensorDataList);
	}

}
