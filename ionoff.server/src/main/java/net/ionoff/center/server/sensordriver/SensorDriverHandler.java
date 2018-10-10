package net.ionoff.center.server.sensordriver;

import net.ionoff.center.server.entity.Sensor;
import net.ionoff.center.server.entity.SensorDriver;
import net.ionoff.center.server.message.SensorStatusNotifier;
import net.ionoff.center.server.persistence.dao.ISensorDao;
import net.ionoff.center.server.persistence.service.IDeviceService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import java.util.Date;

@Component
public class SensorDriverHandler {

	private static final Logger LOGGER = LoggerFactory.getLogger(SensorDriverHandler.class.getName());

	@Autowired
	private ISensorDao sensorDao;

	@Autowired
	private IDeviceService deviceService;

	@Lazy // very important to be lazy here due to cyclic dependency
	@Autowired
	private SensorStatusNotifier sensorStatusNotifier;

	public void onMessageArrived(String payload) {
		ExSensorStatus data = new ExSensorStatus(payload);
		SensorDriver sensorDriver = deviceService.findSensorDriverByMac(data.getId());
		if (sensorDriver == null) {
			LOGGER.info("Found no sensor-relaydriver by id: " + data.getId());
			return;
		}
		Date now = new Date();
		sensorDriver.setTime(now);
		if (sensorDriver.getSensors() == null || sensorDriver.getSensors().isEmpty()) {
			deviceService.update(sensorDriver);
			return;
		}

		if (data.getValue() == null || data.getIndex() == null) {
			LOGGER.info("Invalid message format");
			deviceService.update(sensorDriver);
			return;
		}

		Sensor sensor = sensorDriver.getSensors().get(0);
		sensor.getStatus().setTime(now);
		sensor.getStatus().setValue(data.getValue());
		sensor.getStatus().setIndex(data.getIndex());

		deviceService.updateSensorStatus(sensor);
		if (ExSensorStatus.CHANGED.equals(data.getCode())) {
			deviceService.onSensorStatusChanged(sensor);
		}
	}
	
}
