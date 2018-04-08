package net.ionoff.center.server.notifier.handler;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;

import net.ionoff.center.server.control.IControlService;
import net.ionoff.center.server.email.service.EmailService;
import net.ionoff.center.server.entity.Mode;
import net.ionoff.center.server.entity.ModeSensor;
import net.ionoff.center.server.entity.Sensor;
import net.ionoff.center.server.persistence.service.IModeSensorService;
import net.ionoff.center.server.sms.service.SmsService;

public class SensorStatusChangedHandler {
	
	private static Logger logger = Logger.getLogger(SensorStatusChangedHandler.class.getName());
	
	@Autowired
	private IControlService controlService;
	
	@Autowired
	private IModeSensorService modeSensorService;
	
	@Autowired
	private SmsService smsService;
	
	@Autowired
	private EmailService emailService;

	@Async	
	public void onSensorStatusChanged(Sensor sensor) {
		logger.info("Sensor " + sensor.getSId() + " status changed. New status: " + sensor.getStatus());
		
		ModeSensor modeSensor = getModeSensor(sensor);
		if (modeSensor == null) {
			return;
		}
		
		modeSensor.setResetTime(System.currentTimeMillis());
		getModeSensorService().update(modeSensor);
		
		if (!modeSensor.isEnabled()) {
			logger.info("Activated mode: " + modeSensor.getMode().getSId() + ". Sensor " + sensor.getSId() + " is disabled.");
			return;
		}
		Double newSensorValue = sensor.getStatus().getValue();
		if (isMatchConditon(modeSensor, newSensorValue)) {
			activateScenes(sensor, modeSensor);
			notifyUsers(sensor, modeSensor);
		}
	}

	private boolean isMatchConditon(ModeSensor modeSensor, Double newSensorValue) {
		// TODO check here
		return false;
	}

	protected ModeSensor getModeSensor(Sensor sensor) {
		if (sensor.getProject() == null) {
			return null;
		}
		Mode mode = sensor.getProject().getActivatedMode();
		if (mode == null) {
			return null;
		}
		if (mode.getSensors() == null) {
			return null;
		}
		for (ModeSensor modeSensor : mode.getSensors()) {
			if (modeSensor.getSensor().getId() == sensor.getId()) {
				return modeSensor;
			}
		}
		return null;
	}

	public IModeSensorService getModeSensorService() {
		return modeSensorService;
	}
	
	protected void activateScenes(Sensor sensor, ModeSensor modeSensor) {
		if (!modeSensor.hasScene()) {
			return;
		}
		getLogger().info("Mode " + modeSensor.getMode().getSId() 
				+ ", sensor " + sensor.getSId() + " is starting new thread to activate scenes");
		new ModeSensorScenesActivator(modeSensor, controlService).start();
	}
	
	protected void notifyUsers(Sensor sensor, ModeSensor modeSensor) {
		if (!modeSensor.hasUser()) {
			return;
		}
		getLogger().info("Mode " + modeSensor.getMode().getSId() 
				+ ", sensor " + sensor.getSId() + " is starting new thread to notify users");
		new ModeSensorUserActivator(modeSensor, emailService, smsService).start();
	}
	
	protected Logger getLogger() {
		return logger;
	}
}
