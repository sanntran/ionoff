package net.ionoff.center.client;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.dispatcher.DispatcherFilter;

import com.google.gwt.http.client.RequestBuilder;

import net.ionoff.center.client.storage.StorageService;


public final class AuthDispatcherFilter implements DispatcherFilter {

    public static final String AUTHORIZATION_HEADER = "Authorization";

    
    public AuthDispatcherFilter() {
    }
    
    @Override
    public boolean filter(Method method, RequestBuilder builder) {
	    String authHeaderValue = StorageService.getInstance().getCookie().getJwtToken();
	    if (authHeaderValue != null) {
	    	builder.setHeader(AUTHORIZATION_HEADER, authHeaderValue);
	    }
	    return true;
    } 
}