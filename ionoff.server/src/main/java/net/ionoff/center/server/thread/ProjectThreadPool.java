package net.ionoff.center.server.thread;

import java.util.ArrayList;
import java.util.List;

import net.ionoff.center.server.control.IControlService;
import net.ionoff.center.server.entity.RelayDriver;
import net.ionoff.center.server.persistence.service.IRelayDriverService;
import net.ionoff.center.server.persistence.service.IDeviceService;
import net.ionoff.center.server.persistence.service.IModeService;
import net.ionoff.center.server.persistence.service.IRelayService;
import net.ionoff.center.server.persistence.service.IScheduleService;
import net.ionoff.center.shared.entity.RelayDriverModel;

public class ProjectThreadPool {
	
	private final ProjectSchedulesExecutor projectSchedulesExecutor;
	private final List<Ec100RelayDriverThread> ec100RelayDriverThreads;
	private final List<Ep2RelayDriverThread> ep2RelayDriverThreads;
	
	public ProjectThreadPool(Long projectId, 
			IRelayDriverService relayDriverService,
			IScheduleService scheduleService,
			IModeService modeService,
			IControlService controlService,
			IRelayService relayService,
			IDeviceService deviceService) {
		
		projectSchedulesExecutor = new ProjectSchedulesExecutor(projectId, scheduleService, modeService, controlService);
		ep2RelayDriverThreads = new ArrayList<>();
		ec100RelayDriverThreads = new ArrayList<>();
		
		if (projectId != null) {
			List<RelayDriver> relayDrivers = relayDriverService.findByProjectId(projectId);
			if (relayDrivers != null && !relayDrivers.isEmpty()) {
				for (RelayDriver relayDriver : relayDrivers) {
					if (RelayDriverModel.HBQ_EC100.equals(relayDriver.getModelObj())) {
						ec100RelayDriverThreads.add(new Ec100RelayDriverThread(relayDriver.getId(), relayDriverService, controlService, relayService));
					}
					else if (RelayDriverModel.HLAB_EP2.equals(relayDriver.getModelObj())) {
						ep2RelayDriverThreads.add(new Ep2RelayDriverThread(relayDriver.getId(), relayDriverService, controlService, relayService));
					}
				}
			}
		}
	}
	
	public void start() {
		projectSchedulesExecutor.start();
		for (Ec100RelayDriverThread ec100RelayDriverThread : ec100RelayDriverThreads) {
			ec100RelayDriverThread.start();
		}
		for (Ep2RelayDriverThread ep2RelayDriverThread : ep2RelayDriverThreads) {
			ep2RelayDriverThread.start();
		}
	}
	
	public void shutdown() {
		try {
			projectSchedulesExecutor.shutdown();
			for (Ec100RelayDriverThread ec100RelayDriverThread : ec100RelayDriverThreads) {
				ec100RelayDriverThread.terminate();
			}
			for (Ep2RelayDriverThread ep2RelayDriverThread : ep2RelayDriverThreads) {
				ep2RelayDriverThread.terminate();
			}
		}
		catch (Exception e) {
			// ignore
		}
	}
}
