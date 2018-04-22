package net.ionoff.center.client.schedule;

import com.google.gwt.event.shared.HandlerManager;

import net.ionoff.center.client.base.AbstractPresenter;
import net.ionoff.center.shared.dto.BaseDto;

public abstract class SceneActionPresenter extends AbstractPresenter {
	
	public SceneActionPresenter(HandlerManager eBus) {
		super(eBus);
	}

	public abstract void save();

	public abstract BaseDto getTarget();

}
