package net.ionoff.center.client.service;

import com.google.gwt.user.client.Window;

import net.ionoff.center.client.storage.ApiServer;
import net.ionoff.center.client.storage.StorageService;

public class ApiServiceUrl {
	
	public static String get() {
    	String protocol = Window.Location.getProtocol();
    	if (protocol.contains("http")) {
    		return protocol 
        			+ "//" + Window.Location.getHostName() 
        			+ ":" + Window.Location.getPort() + "/iserver";
    	}
    	ApiServer server = StorageService.getInstance().getEnabledApiServer();
		if (server == null) {
			return "http://cloud.ionoff.net/iserver"; 	
		}
		if (server.getHost().startsWith("http")) {
			return server.getHost() + "/iserver";  
		}
		return "http://" + server.getHost() + "/iserver";  
    }
}
