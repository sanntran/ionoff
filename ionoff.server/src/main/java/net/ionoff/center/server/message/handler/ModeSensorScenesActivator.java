package net.ionoff.center.server.message.handler;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.ionoff.center.server.service.IControlService;
import net.ionoff.center.server.entity.ModeSensor;
import net.ionoff.center.server.entity.ModeSensorScene;

class ModeSensorScenesActivator extends Thread {
	private static Logger logger = LoggerFactory.getLogger(ModeSensorScenesActivator.class.getName());
	
	private final ModeSensor modeSensor;
	private final IControlService controlService;
	private final String threadName;
	
	ModeSensorScenesActivator(ModeSensor modeSensor, IControlService controlService) {
		this.modeSensor = modeSensor;
		this.controlService = controlService;
		threadName = "Thread of " + modeSensor.toString();
	}
	
	@Override
	public void run() {
		if (modeSensor.hasScene()) {
			for (ModeSensorScene sensorScene : modeSensor.getScenes()) {
				activateScene(sensorScene);
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
			message = message + ", scene: " + sensorScene.getScene().getNameId();
			logger.info(message);
			controlService.playScene(sensorScene.getScene());
		}
		catch (Exception e) {
			logger.error(e.getMessage(), e);
		}
	}
}
