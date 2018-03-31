package net.ionoff.center.server.notifier.handler;

import org.apache.log4j.Logger;

import net.ionoff.center.server.control.IControlService;
import net.ionoff.center.server.entity.ModeSensor;
import net.ionoff.center.server.entity.ModeSensorScene;

class ModeSensorScenesActivator extends Thread {
	private static Logger logger = Logger.getLogger(ModeSensorScenesActivator.class.getName());
	
	private boolean detected;
	private final ModeSensor modeSensor;
	private final IControlService controlService;
	private final String threadName;
	
	ModeSensorScenesActivator(ModeSensor modeSensor, boolean detected, IControlService controlService) {
		this.detected  = detected;
		this.modeSensor = modeSensor;
		this.controlService = controlService;
		threadName = "Thread of mode:" + modeSensor.getMode().getSId() 
				+ " - sensor:" + modeSensor.getSensor().getSId() 
				+ " - detected:" + detected;
	}
	
	@Override
	public void run() {
		if (modeSensor.hasScene()) {
			for (ModeSensorScene sensorScene : modeSensor.getScenes()) {
				if (sensorScene.getDetected() == detected) {
					activateScene(sensorScene);
				}
			}
			logger.info(threadName + " has finised activating scenes");
		}
	}
	
	private void activateScene(ModeSensorScene sensorScene) {
		try {
			String message = threadName + " is activating scene in zone " + sensorScene.getZone().getSId();
			
			if (sensorScene.getScene() == null) {
				message = message + ", scene: null";
				logger.info(message);
				return;
			}
			message = message + ", scene: " + sensorScene.getScene().getSId();
			logger.info(message);
			controlService.playScene(sensorScene.getScene());
		}
		catch (Exception e) {
			logger.error(e.getMessage(), e);
		}
	}
}
