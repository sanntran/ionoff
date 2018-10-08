package net.ionoff.center.server.message.handler;

import net.ionoff.center.server.control.IControlService;
import net.ionoff.center.server.entity.ModeSensor;
import net.ionoff.center.server.entity.Sensor;
import net.ionoff.center.server.notify.INotificationService;
import net.ionoff.center.server.persistence.dao.IModeSensorDao;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import java.util.Collections;
import java.util.List;

public class SensorStatusChangedHandler {
	
	private static Logger logger = Logger.getLogger(SensorStatusChangedHandler.class.getName());
	
	@Autowired
	private IControlService controlService;
	
	@Autowired
	private IModeSensorDao modeSensorDao;
	
	@Autowired
	private INotificationService notificationService;

	public void onSensorStatusChanged(Sensor sensor) {
		logger.info("Sensor " + sensor.getSId() + " status changed. New status: " + sensor.getStatus());
		
		List<ModeSensor> modeSensors = getEnabledModeSensors(sensor);
		if (modeSensors == null || modeSensors.isEmpty()) {
			logger.info("There is no mode-sensor handle for sensor status changed");
			return;
		}
		for (ModeSensor modeSensor : modeSensors) {
			modeSensor.setResetTime(System.currentTimeMillis());
			modeSensorDao.update(modeSensor);
		}
		Double newSensorValue = sensor.getStatus().getValue();
		for (ModeSensor modeSensor : modeSensors) {
			if (isMatchConditon(modeSensor, newSensorValue)) {
				activateScenes(sensor, modeSensor);
				notifyUsers(sensor, modeSensor);
			}
		}
	}

	private boolean isMatchConditon(ModeSensor modeSensor, Double newSensorValue) {
		Boolean result = resolveConditionExpression(modeSensor.getCondition(), newSensorValue);
		getLogger().info("Condition expression result " + result + ", x = " + newSensorValue 
				+ ", expression: " + modeSensor.getCondition());
		return result;
	}
	
	public boolean resolveConditionExpression(String expression, Double sensorVale) {
		if (expression == null || sensorVale == null) {
			return false;
		}
		try {
			String exp = expression.replaceAll(
					ModeSensor.CONDITION_VARIABLE, sensorVale + "");
			ScriptEngineManager mgr = new ScriptEngineManager();
		    ScriptEngine engine = mgr.getEngineByName("JavaScript"); 
		    return (Boolean) engine.eval(exp);
		}
		catch (Exception e) {
			logger.error(e.getMessage(), e);
			return false;
		}
	}

	protected List<ModeSensor> getEnabledModeSensors(Sensor sensor) {
		if (sensor.getProject() == null) {
			Collections.emptyList();
		}
		return modeSensorDao.findOnSensorStatusChanged(sensor);
	}

	protected void activateScenes(Sensor sensor, ModeSensor modeSensor) {
		if (!modeSensor.hasScene()) {
			return;
		}
		getLogger().info(modeSensor.toString() + " is starting new thread to activate scenes");
		new ModeSensorScenesActivator(modeSensor, controlService).start();
	}
	
	protected void notifyUsers(Sensor sensor, ModeSensor modeSensor) {
		if (!modeSensor.hasUser()) {
			return;
		}
		getLogger().info(modeSensor.toString() + " is starting new thread to notify users");
		new ModeSensorUserActivator(modeSensor, notificationService).start();
	}
	
	protected Logger getLogger() {
		return logger;
	}
}
