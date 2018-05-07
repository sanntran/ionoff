package net.ionoff.center.server.restapi;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.stereotype.Component;

import net.ionoff.center.server.entity.RelayDriver;
import net.ionoff.center.server.entity.Version;
import net.ionoff.center.server.license.LicenseManager;
import net.ionoff.center.server.persistence.service.IRelayDriverService;
import net.ionoff.center.server.persistence.service.IVersionService;
import net.ionoff.center.server.thread.ServerThreadPool;

@Component
public class ApplicationStartupListener implements ApplicationListener<ContextRefreshedEvent> {
	  
	@Autowired
	private ServerThreadPool serverThreadPool;
	
	@Autowired
	private IVersionService versionService;
	
	@Autowired
	private IRelayDriverService relayDriverService;
	
	@Override
	public void onApplicationEvent(final ContextRefreshedEvent event) {
		if (LicenseManager.checkLicense()) {
			serverThreadPool.start();
		}
		
		checkRelayDriverToInsertSwitches();
	}

	private void checkRelayDriverToInsertSwitches() {
		List<Version> versions = versionService.loadAll();
		if (versions.isEmpty()) {
			return;
		}
		if ("201804081230".compareTo(versions.get(0).getDateTime()) > 0) {
			insertSwitchesToRelayDrivers();
		}
	}

	private void insertSwitchesToRelayDrivers() {
		List<RelayDriver> relayDrivers = relayDriverService.loadAll();
		for (RelayDriver driver : relayDrivers) {
			relayDriverService.insertSwitches(driver);
		}
	}
}