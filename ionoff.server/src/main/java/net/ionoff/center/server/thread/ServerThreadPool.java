package net.ionoff.center.server.thread;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

import net.ionoff.center.server.control.IControlService;
import net.ionoff.center.server.entity.Project;
import net.ionoff.center.server.persistence.service.IControllerService;
import net.ionoff.center.server.persistence.service.IDeviceService;
import net.ionoff.center.server.persistence.service.IModeService;
import net.ionoff.center.server.persistence.service.IProjectService;
import net.ionoff.center.server.persistence.service.IRelayService;
import net.ionoff.center.server.persistence.service.IScheduleService;
import net.ionoff.center.server.xapxinh.PlayerConnectionPool;

public class ServerThreadPool {
	private static final Logger LOGGER = Logger.getLogger(ServerThreadPool.class.getName());
	@Autowired
	private IProjectService projectService;
	@Autowired
	private IControllerService controllerService;
	@Autowired
	private IScheduleService scheduleService;
	@Autowired
	private IModeService modeService;
	@Autowired
	private IControlService controlService;
	@Autowired
	private IRelayService relayService;
	@Autowired 
	private ControllerConnectionPool controllerConnectionPool;
	@Autowired
	private PlayerConnectionPool playerConnectionPool;
	@Autowired
	private IDeviceService deviceService;
	
	private Map<Long, ProjectThreadPool> projectThreadPoolMap;
	
	private boolean started;
	
	private boolean shutdown;
	
	public ServerThreadPool() {
		started = false;
		shutdown = false;
		projectThreadPoolMap = new HashMap<Long, ProjectThreadPool>();
	}
	
	public void start() {
	
		if (started) {
			return;
		}
		started = true;
		playerConnectionPool.start();
		controllerConnectionPool.start();
		startAllProjectThreadPools();
	}
	
	private void startAllProjectThreadPools() {
		List<Project> projects = projectService.loadAll();
		for (Project project : projects) {
			startNewProjectThreadPool(project.getId());
		}
	}
	
	public void removeProjectThreadPool(Long projectId) {
		if (projectThreadPoolMap.containsKey(projectId)) {
			projectThreadPoolMap.get(projectId).shutdown();
		}
		projectThreadPoolMap.remove(projectId);
	}
	
	public void startNewProjectThreadPool(Long projectId) {
		if (projectId == null || projectId.longValue() == Project.DEFAULT_ID) {
			LOGGER.info("Project id is not allowed to create new thread: " + projectId);
			return;
		}
		if (projectThreadPoolMap.containsKey(projectId)) {
			return;
		}
		ProjectThreadPool projectThreadPool = new ProjectThreadPool(projectId, 
				controllerService, 
				scheduleService, modeService, 
				controlService, relayService, deviceService
				);
		projectThreadPoolMap.put(projectId, projectThreadPool);
		projectThreadPool.start();
	}

	public void shutdown() {
		this.shutdown = true;
		// iupdator is updating...
		LOGGER.info("PlayerConnectionPool is shutting down...");
		playerConnectionPool.shutdown();
		// iupdator is updating...
		LOGGER.info("ControllerConnectionPool is shutting down...");
		controllerConnectionPool.shutdown();
		
		for (Entry<Long, ProjectThreadPool> entry : projectThreadPoolMap.entrySet()) {
			entry.getValue().shutdown();
		}
	}

	public boolean isShutdown() {
		return shutdown;
	}
}
