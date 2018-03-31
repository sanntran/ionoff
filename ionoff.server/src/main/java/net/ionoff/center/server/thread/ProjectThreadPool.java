package net.ionoff.center.server.thread;

import java.util.ArrayList;
import java.util.List;

import net.ionoff.center.server.control.IControlService;
import net.ionoff.center.server.entity.Controller;
import net.ionoff.center.server.persistence.service.IControllerService;
import net.ionoff.center.server.persistence.service.IDeviceService;
import net.ionoff.center.server.persistence.service.IModeService;
import net.ionoff.center.server.persistence.service.IRelayService;
import net.ionoff.center.server.persistence.service.IScheduleService;
import net.ionoff.center.shared.entity.ControllerModel;

public class ProjectThreadPool {
	
	private final ProjectSchedulesExecutor projectSchedulesExecutor;
	private final List<Ec100ControllerThread> ec100ControllerThreads;
	private final List<Ep2ControllerThread> ep2ControllerThreads;
	
	public ProjectThreadPool(Long projectId, 
			IControllerService controllerService,
			IScheduleService scheduleService,
			IModeService modeService,
			IControlService controlService,
			IRelayService relayService,
			IDeviceService deviceService) {
		
		projectSchedulesExecutor = new ProjectSchedulesExecutor(projectId, scheduleService, modeService, controlService);
		ep2ControllerThreads = new ArrayList<>();
		ec100ControllerThreads = new ArrayList<>();
		
		if (projectId != null) {
			List<Controller> controllers = controllerService.findByProjectId(projectId);
			if (controllers != null && !controllers.isEmpty()) {
				for (Controller controller : controllers) {
					if (ControllerModel.HBQ_EC100.equals(controller.getModelObj())) {
						ec100ControllerThreads.add(new Ec100ControllerThread(controller.getId(), controllerService, controlService, relayService));
					}
					else if (ControllerModel.HLAB_EP2.equals(controller.getModelObj())) {
						ep2ControllerThreads.add(new Ep2ControllerThread(controller.getId(), controllerService, controlService, relayService));
					}
				}
			}
		}
	}
	
	public void start() {
		projectSchedulesExecutor.start();
		for (Ec100ControllerThread ec100ControllerThread : ec100ControllerThreads) {
			ec100ControllerThread.start();
		}
		for (Ep2ControllerThread ep2ControllerThread : ep2ControllerThreads) {
			ep2ControllerThread.start();
		}
	}
	
	public void shutdown() {
		try {
			projectSchedulesExecutor.shutdown();
			for (Ec100ControllerThread ec100ControllerThread : ec100ControllerThreads) {
				ec100ControllerThread.terminate();
			}
			for (Ep2ControllerThread ep2ControllerThread : ep2ControllerThreads) {
				ep2ControllerThread.terminate();
			}
		}
		catch (Exception e) {
			// ignore
		}
	}
}
