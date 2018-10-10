package net.ionoff.center.server.persistence.service.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Project;
import net.ionoff.center.server.entity.Sensor;
import net.ionoff.center.server.entity.SensorData;
import net.ionoff.center.server.entity.SensorStatus;
import net.ionoff.center.server.entity.Switch;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.exception.UpdateEntityException;
import net.ionoff.center.server.locale.Messages;
import net.ionoff.center.server.persistence.mapper.QueryCriteriaMapper;
import net.ionoff.center.server.persistence.mapper.SensorMapper;
import net.ionoff.center.server.persistence.dao.IProjectDao;
import net.ionoff.center.server.persistence.dao.ISensorDao;
import net.ionoff.center.server.persistence.dao.ISensorDataDao;
import net.ionoff.center.server.persistence.dao.ISensorStatusDao;
import net.ionoff.center.server.persistence.dao.ISwitchDao;
import net.ionoff.center.server.persistence.service.ISensorService;
import net.ionoff.center.shared.dto.QueryCriteriaDto;
import net.ionoff.center.shared.dto.SensorDataDto;
import net.ionoff.center.shared.dto.SensorDto;
import net.ionoff.center.shared.entity.SensorType;

@Service
@Transactional
public class SensorServiceImpl extends AbstractGenericService<Sensor, SensorDto> implements ISensorService {

	private ISensorDao sensorDao;
	
	@Autowired
	private SensorMapper sensorMapper;

	@Lazy
	@Autowired 
	private ISwitchDao switchDao;
	
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
		Switch zwitch =  null;
		if (dto.getDriverId() != null) {
			zwitch = switchDao.findByDriverId(dto.getDriverId(), dto.getIndex());
		}
		Sensor sensor = sensorMapper.createSensor(dto, project, zwitch);
		insert(sensor);
		return sensorMapper.createSensorDto(sensor);
	}

	@Override
	public SensorDto updateDto(User user, SensorDto dto) {
		validateSensor(dto, user.getLanguage());
		Sensor sensor = requireById(dto.getId());
		Switch zwitch =  null;
		if (dto.getDriverId() != null) {
			zwitch = switchDao.findByDriverId(dto.getDriverId(), dto.getIndex());
		}
		sensorMapper.updateSensor(sensor, dto, zwitch);
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

}
