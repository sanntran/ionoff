package net.ionoff.center.server.persistence.service.impl;

import java.util.List;

import net.ionoff.center.server.objmapper.QueryCriteriaMapper;
import net.ionoff.center.shared.dto.QueryCriteriaDto;
import net.ionoff.center.shared.dto.SensorDataDto;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Mode;
import net.ionoff.center.server.entity.ModeSensor;
import net.ionoff.center.server.entity.Project;
import net.ionoff.center.server.entity.Sensor;
import net.ionoff.center.server.entity.SensorData;
import net.ionoff.center.server.entity.SensorStatus;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.exception.UpdateEntityException;
import net.ionoff.center.server.locale.Messages;
import net.ionoff.center.server.objmapper.SensorMapper;
import net.ionoff.center.server.persistence.dao.ISensorDao;
import net.ionoff.center.server.persistence.dao.ISensorDataDao;
import net.ionoff.center.server.persistence.dao.ISensorStatusDao;
import net.ionoff.center.server.persistence.service.IModeSensorService;
import net.ionoff.center.server.persistence.service.ISensorService;
import net.ionoff.center.shared.dto.SensorDto;
import net.ionoff.center.shared.entity.SensorType;

@Transactional
public class SensorServiceImpl extends AbstractGenericService<Sensor, SensorDto> implements ISensorService {

	private ISensorDao sensorDao;
	
	@Autowired
	private SensorMapper sensorMapper;
	
	@Autowired
	private ISensorDataDao sensorDataDao;
	
	@Autowired
	private ISensorStatusDao sensorStatusDao;
	
	@Autowired
	private IModeSensorService modeSensorService;
	
	public SensorServiceImpl(ISensorDao sensorDao) {
		this.sensorDao = sensorDao;
	}

	@Override
	protected ISensorDao getDao() {
		return sensorDao;
	}
	
	@Override
	public Sensor insert(Sensor sensor) {
		super.insert(sensor);
		return sensor;
	}

	private void insertModeSensors(Sensor sensor) {
		Project project = sensor.getProject();
		if (project.getModes() == null) {
			return;
		}
		for (Mode mode : project.getModes()) {
			insertModeSensor(mode, sensor);
		}
	}

	private void insertModeSensor(Mode mode, Sensor sensor) {
		ModeSensor modeSensor = new ModeSensor();
		modeSensor.setMode(mode);
		modeSensor.setSensor(sensor);
		modeSensorService.insert(modeSensor);
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
		Sensor sensor = sensorMapper.createSensor(dto);
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
		if (sensorDto.getDriverId() == null || sensorDto.getIndex() == null) {
			final String message = Messages.get(locale).errorSensorDriverIndex();
			throw new UpdateEntityException(message);
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
	public List<Sensor> findBySwitchId(long switchId) {
		return sensorDao.findBySwitchId(switchId);
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

	@Override
	public List<SensorDataDto> getSumDataByDay(QueryCriteriaDto criteriaDto) {
		List<SensorData> sensorDataList = sensorDataDao.getSumByDay(QueryCriteriaMapper.toQueryCriteria(criteriaDto));
		return sensorMapper.createSensorDataDtoList(sensorDataList);
	}
}
