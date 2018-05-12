package net.ionoff.center.server.persistence.service.impl;

import java.io.File;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Sensor;
import net.ionoff.center.server.entity.SensorData;
import net.ionoff.center.server.entity.SensorStatus;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.exception.UpdateEntityException;
import net.ionoff.center.server.locale.Messages;
import net.ionoff.center.server.objmapper.QueryCriteriaMapper;
import net.ionoff.center.server.objmapper.SensorMapper;
import net.ionoff.center.server.persistence.dao.ISensorDao;
import net.ionoff.center.server.persistence.dao.ISensorDataDao;
import net.ionoff.center.server.persistence.dao.ISensorStatusDao;
import net.ionoff.center.server.persistence.service.IModeSensorService;
import net.ionoff.center.server.persistence.service.ISensorService;
import net.ionoff.center.server.util.DateTimeUtil;
import net.ionoff.center.shared.dto.QueryCriteriaDto;
import net.ionoff.center.shared.dto.SensorDataDto;
import net.ionoff.center.shared.dto.SensorDto;
import net.ionoff.center.shared.entity.SensorType;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JasperCompileManager;
import net.sf.jasperreports.engine.JasperExportManager;
import net.sf.jasperreports.engine.JasperFillManager;
import net.sf.jasperreports.engine.JasperPrint;
import net.sf.jasperreports.engine.JasperReport;
import net.sf.jasperreports.engine.data.JRBeanCollectionDataSource;
import net.sf.jasperreports.engine.design.JasperDesign;
import net.sf.jasperreports.engine.export.ooxml.JRDocxExporter;
import net.sf.jasperreports.engine.export.ooxml.JRXlsxExporter;
import net.sf.jasperreports.engine.xml.JRXmlLoader;
import net.sf.jasperreports.export.SimpleExporterInput;
import net.sf.jasperreports.export.SimpleOutputStreamExporterOutput;
import net.sf.jasperreports.export.SimpleXlsxReportConfiguration;
import net.sf.jasperreports.export.XlsxReportConfiguration;

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


	@Override
	public String exportDataToReport(QueryCriteriaDto criteriaDto, String fileType) throws JRException {
    	List<SensorData> sensorDataList = getSensorDataListDailyAlalyitic(criteriaDto); 
    			
		InputStream resourceAsStream = Thread.currentThread().getContextClassLoader()
				.getResourceAsStream("ScaleData.jrxml");
		
		// get report file and then load into jasperDesign
		JasperDesign jasperDesign = JRXmlLoader.load(resourceAsStream);
		// compile the jasperDesign
		JasperReport jasperReport = JasperCompileManager.compileReport(jasperDesign);

		StringBuilder sb = new StringBuilder();
		sb.append(System.getProperty("catalina.base"))
				.append(File.separator)
				.append("webapps")
				.append(File.separator)
				.append("ireport");

		File reportFolder = new File(sb.toString());
		if (!reportFolder.exists()) {
			reportFolder.mkdirs();
		}
		
		StringBuilder fileNameBuilder = new StringBuilder();
		fileNameBuilder.append("SensorData_")
				.append(DateTimeUtil.yyyyMMdd_HHmmssFormatter.format(new Date()))
				.append(".")
				.append(fileType);

		sb.append(File.separator).append(fileNameBuilder.toString());
		String outputFileName = sb.toString();

		// fill the ready report with data and parameter
		Map<String, Object> params = new HashMap<String, Object>();
		params.put("FROM_DATE_TO_DATE", criteriaDto.getSearchKey());
		
		JasperPrint jasperPrint = JasperFillManager.fillReport(jasperReport, params,
				new JRBeanCollectionDataSource(sensorDataList));

		if ("pdf".equals(fileType)) {
			JasperExportManager.exportReportToPdfFile(jasperPrint, outputFileName);
		} 		
		else if ("xlsx".equals(fileType)) {
			JRXlsxExporter xlsxExporter = new JRXlsxExporter();
			xlsxExporter.setExporterInput(new SimpleExporterInput(jasperPrint));
			xlsxExporter.setExporterOutput(new SimpleOutputStreamExporterOutput(new File(outputFileName)));

			XlsxReportConfiguration config = new SimpleXlsxReportConfiguration();
			// config.setFlexibleRowHeight(true); //Set desired configuration
			xlsxExporter.setConfiguration(config);

			xlsxExporter.exportReport();
		} 		
		else if ("docx".equals(fileType)) {
			JRDocxExporter exporterDocx = new JRDocxExporter();
			exporterDocx.setExporterInput(new SimpleExporterInput(jasperPrint));
			exporterDocx.setExporterOutput(new SimpleOutputStreamExporterOutput(new File(outputFileName)));

			exporterDocx.exportReport();
		} 
		else {
			throw new IllegalArgumentException("fileType: " + fileType);
		}
		
		return "ireport/" + fileNameBuilder.toString();

	}

	private List<SensorData> getSensorDataListDailyAlalyitic(QueryCriteriaDto criteriaDto) {
		List<SensorData> sensorDataList = sensorDataDao.findByDay(QueryCriteriaMapper.toQueryCriteria(criteriaDto));
    	
    	List<SensorData> sensorDataDailyAnalytic = new ArrayList<>(sensorDataList.size());
    	Calendar cal = Calendar.getInstance();
    	int day = -1;
    	for (int i = 0; i < sensorDataList.size(); i++) {
    		cal.setTime(sensorDataList.get(i).getTime());
    		int d = cal.get(Calendar.DAY_OF_YEAR);
    		if (i == 0) {
    			day = d;
    		}
    		if ((day != d && !sensorDataDailyAnalytic.isEmpty()) || i == sensorDataList.size() - 1) {
    			day = d;
    			if (i == sensorDataList.size() - 1) {
    				sensorDataDailyAnalytic.add(sensorDataList.get(i));
    			}
    			SensorData sensorData = new SensorData();
    			sensorData.setId(0L);
    			sensorData.setIndex(null);
    			sensorData.setTime(sensorDataDailyAnalytic.get(sensorDataDailyAnalytic.size() - 1).getTime());
    			Double total = 0D;
    			for (int j = sensorDataDailyAnalytic.size() - 1 ; j >= 0; j--) {
    				if (sensorDataDailyAnalytic.get(j).getIndex() == null) {
    					break;
    				}
    				else {
    					total = total + sensorDataDailyAnalytic.get(j).getValue();
    				}
    			}
    			sensorData.setValue(total);
    			sensorData.setSensor(sensorDataDailyAnalytic.get(sensorDataDailyAnalytic.size() - 1).getSensor());
    			sensorDataDailyAnalytic.add(sensorData);
    		}
    		if (i != sensorDataList.size() - 1) {
    			sensorDataDailyAnalytic.add(sensorDataList.get(i));    		
    		}
    	}
		return sensorDataDailyAnalytic;

	}
}
