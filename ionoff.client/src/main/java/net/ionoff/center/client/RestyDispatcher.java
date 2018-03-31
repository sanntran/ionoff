package net.ionoff.center.client;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.dispatcher.DefaultFilterawareDispatcher;

import com.google.gwt.http.client.Request;
import com.google.gwt.http.client.RequestBuilder;
import com.google.gwt.http.client.RequestException;

public class RestyDispatcher extends DefaultFilterawareDispatcher {
	
    public RestyDispatcher() {
	    addFilter(new AuthDispatcherFilter());
    }

    @Override
    public Request send(Method method, RequestBuilder builder) throws RequestException {
	    return super.send(method, builder);
    }
}
