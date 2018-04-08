package net.ionoff.center.server.persistence.service.impl;

import java.util.List;

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
		insertModeSensors(sensor);
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
	public void updateStatus(SensorStatus status) {
		sensorStatusDao.update(status);
		SensorData sensorData = newSensorData(status);
		sensorDataDao.insert(sensorData);
	}

	private SensorData newSensorData(SensorStatus status) {
		SensorData sensorData = new SensorData();
		sensorData.setSensor(status.getSensor());
		sensorData.setTime(status.getTime());
		sensorData.setValue(status.getValue());
		sensorData.setTotal(status.getTotal());
		return sensorData;
	}

	public ISensorStatusDao getSensorStatusDao() {
		return sensorStatusDao;
	}

	public void setSensorStatusDao(ISensorStatusDao sensorStatusDao) {
		this.sensorStatusDao = sensorStatusDao;
	}
}
