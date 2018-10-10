package net.ionoff.center.server.controller;

import net.ionoff.center.server.broker.MqttConnection;
import net.ionoff.center.server.license.LicenseManager;
import net.ionoff.center.server.message.RelayStatusNotifier;
import net.ionoff.center.server.message.SensorStatusNotifier;
import net.ionoff.center.server.message.handler.RelayStatusChangedHandler;
import net.ionoff.center.server.message.handler.SensorStatusChangedHandler;
import net.ionoff.center.server.message.listener.RelayStatusChangedListener;
import net.ionoff.center.server.message.listener.SensorStatusChangedListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEvent;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextClosedEvent;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.context.event.ContextStoppedEvent;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;
import org.springframework.stereotype.Component;

import java.sql.Driver;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Enumeration;

@Component
public class ApplicationEventListener implements ApplicationListener<ApplicationEvent> {
	
	private final Logger logger = LoggerFactory.getLogger(ApplicationEventListener.class.getName());

	@Autowired
	private ThreadPoolTaskExecutor taskExecutor;
	
    @Autowired 
    private ThreadPoolTaskScheduler taskScheduler;

    @Autowired
    private RelayStatusChangedListener relayStatusChangedListener;
    
    @Autowired
    private RelayStatusChangedHandler relayStatusChangedHandler;
    
    @Autowired
    private SensorStatusChangedListener sensorStatusChangedListener;

    @Autowired
    private SensorStatusChangedHandler sensorStatusChangedHandler;

	@Autowired
	private SensorStatusNotifier sensorStatusNotifier;
	
	@Autowired
	private RelayStatusNotifier relayStatusNotifier;

	@Autowired
	private MqttConnection mqttConnection;

	@Override
	public void onApplicationEvent(ApplicationEvent event) {
		if (event instanceof ContextRefreshedEvent) {
			LicenseManager.checkLicense();
			mqttConnection.start();
		}
		else if (event instanceof ContextStoppedEvent || event instanceof  ContextClosedEvent) {
			// ... Then close any DB connection pools ...
		    // Now deregister JDBC drivers in this context's ClassLoader:
		    // Get the webapp's ClassLoader
		    ClassLoader cl = Thread.currentThread().getContextClassLoader();
		    // Loop through all drivers
		    Enumeration<Driver> drivers = DriverManager.getDrivers();
		    while (drivers.hasMoreElements()) {
		        Driver driver = drivers.nextElement();
		        if (driver.getClass().getClassLoader() == cl) {
		            // This relaydriver was registered by the webapp's ClassLoader, so deregister it:
		            try {
		                logger.info("Deregistering JDBC relaydriver " + driver);
		                DriverManager.deregisterDriver(driver);
		            } catch (SQLException ex) {
		            	logger.error("Error deregistering JDBC relaydriver " + driver, ex);
		            }
		        } else {
		            // relaydriver was not registered by the webapp's ClassLoader and may be in use elsewhere
		        	logger.info("Not deregistering JDBC relaydriver as it does not belong to this webapp's ClassLoader");
		        }
		    }
	    	taskExecutor.shutdown();
	    	taskScheduler.shutdown();
		}
	}

}