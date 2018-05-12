package net.ionoff.center.server.restapi;

import java.sql.Driver;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Enumeration;

import org.apache.log4j.Logger;
import org.hibernate.SessionFactory;
import org.hibernate.c3p0.internal.C3P0ConnectionProvider;
import org.hibernate.engine.jdbc.connections.spi.ConnectionProvider;
import org.hibernate.internal.SessionFactoryImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEvent;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextClosedEvent;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.context.event.ContextStoppedEvent;
import org.springframework.orm.hibernate4.LocalSessionFactoryBean;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;
import org.springframework.stereotype.Component;

import net.ionoff.center.server.license.LicenseManager;
import net.ionoff.center.server.persistence.service.IVersionService;
import net.ionoff.center.server.thread.ServerThreadPool;

@Component
public class ApplicationEventListener implements ApplicationListener<ApplicationEvent> {
	
	private final Logger logger = Logger.getLogger(ApplicationEventListener.class.getName());
	
	@Autowired
	private ServerThreadPool serverThreadPool;
	
	@Autowired
	private IVersionService versionService;
	
	@Autowired 
	private ThreadPoolTaskExecutor taskExecutor;
	
    @Autowired 
    private ThreadPoolTaskScheduler taskScheduler;
    
    @Autowired
	private LocalSessionFactoryBean sessionFactory;
	

	@Override
	public void onApplicationEvent(ApplicationEvent event) {
		if (event instanceof ContextRefreshedEvent) {
			if (LicenseManager.checkLicense()) {
				serverThreadPool.start();
			}
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
		            // This driver was registered by the webapp's ClassLoader, so deregister it:
		            try {
		                logger.info("Deregistering JDBC driver " + driver);
		                DriverManager.deregisterDriver(driver);
		            } catch (SQLException ex) {
		            	logger.error("Error deregistering JDBC driver " + driver, ex);
		            }
		        } else {
		            // driver was not registered by the webapp's ClassLoader and may be in use elsewhere
		        	logger.info("Not deregistering JDBC driver as it does not belong to this webapp's ClassLoader");
		        }
		    }
		    serverThreadPool.shutdown();
	    	taskExecutor.shutdown();
	    	taskScheduler.shutdown();
	    	closeSessionFactory();  
		}
	}
    	
	private boolean closeSessionFactory() {
	    boolean done = false;
	    SessionFactory factory = sessionFactory.getObject();
	    if(factory instanceof SessionFactoryImpl) {
	        SessionFactoryImpl sf = (SessionFactoryImpl)factory;
	        ConnectionProvider conn = sf.getConnectionProvider();
	        if(conn instanceof C3P0ConnectionProvider) { 
	            ((C3P0ConnectionProvider)conn).stop();
	            try {
	                Thread.sleep(2000); //Let give it time...it is enough...probably
	            } catch (InterruptedException e) {
	                // ignore
	            }
	            done = true;
	        }
	        factory.close();
	    }
	    return done;
	}

}