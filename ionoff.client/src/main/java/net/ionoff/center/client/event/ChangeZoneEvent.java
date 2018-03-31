package net.ionoff.center.client.event;

import com.google.gwt.event.shared.GwtEvent;

import net.ionoff.center.shared.dto.ZoneDto;

public class ChangeZoneEvent extends GwtEvent<ChangeZoneEventHandler> {
	public static Type<ChangeZoneEventHandler> TYPE = new Type<ChangeZoneEventHandler>();

	private final ZoneDto zone;
	
	public ChangeZoneEvent(ZoneDto zone) {
		this.zone = zone;
	}

	public ZoneDto getZone() {
		return zone;
	}

	@Override
	public Type<ChangeZoneEventHandler> getAssociatedType() {
		return TYPE;
	}

	@Override
	protected void dispatch(ChangeZoneEventHandler handler) {
		handler.onChangeZone(this);
	}
}
