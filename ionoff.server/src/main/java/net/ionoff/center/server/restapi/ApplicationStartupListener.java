package net.ionoff.center.server.restapi;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.stereotype.Component;

import net.ionoff.center.server.license.LicenseManager;
import net.ionoff.center.server.thread.ServerThreadPool;

@Component
public class ApplicationStartupListener implements ApplicationListener<ContextRefreshedEvent> {
	  
	@Autowired
	private ServerThreadPool serverThreadPool;
	
	
	@Override
	public void onApplicationEvent(final ContextRefreshedEvent event) {
		if (LicenseManager.checkLicense()) {
			serverThreadPool.start();
		}
	}
}