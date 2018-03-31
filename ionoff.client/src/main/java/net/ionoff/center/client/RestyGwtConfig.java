package net.ionoff.center.client;

import com.google.inject.Inject;

public class RestyGwtConfig {
	
	private final RestyDispatcher dispatcher;
	
	@Inject
    public RestyGwtConfig() {
	    dispatcher = new RestyDispatcher();
	    
    }

	public RestyDispatcher getDispatcher() {
		return dispatcher;
	}
}