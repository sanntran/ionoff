package net.ionoff.center.client.zone;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.HTMLPanel;
import com.google.gwt.user.client.ui.HasWidgets;

import gwt.material.design.client.ui.MaterialIcon;
import gwt.material.design.client.ui.MaterialLabel;
import gwt.material.design.client.ui.MaterialLink;
import gwt.material.design.client.ui.MaterialPanel;
import net.ionoff.center.client.base.AbstractPresenter;
import net.ionoff.center.client.event.ChangeTokenEvent;
import net.ionoff.center.client.event.ChangeZoneEvent;
import net.ionoff.center.client.locale.ProjectLocale;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.AppToken;
import net.ionoff.center.shared.dto.ZoneDto;

public class ZonePresenter extends AbstractPresenter {
	
	public interface Display {
		HTMLPanel asPanel();
		MaterialIcon getIcon();
		MaterialLabel getLblName();
		MaterialLabel getLblDevices();
		MaterialLabel getLblArea();
		MaterialPanel getZoneCard();
	}
	
	private Display display;
	private final IRpcServiceProvider rpcService;
	private final ZoneDto zone;
	private boolean locked; // lock when sending request of control 

	public ZonePresenter(IRpcServiceProvider rpcService, HandlerManager eventBus, ZoneDto zone, Display view) {
		super(eventBus);
		this.setLocked(false);
		this.rpcService = rpcService;
		this.zone = zone;
		this.display = view;
	}

	@Override
	public void go() {
		bind();
	}
	
	public void bind() {
		display.getLblName().setText(zone.getName());
		display.getLblArea().setText(zone.getAreaName());
		display.getLblDevices().setText(zone.getDevicesCount() + " " + ProjectLocale.getProjectConst().device());
		display.asPanel().removeStyleName("lighting");
		if (Boolean.TRUE.equals(zone.getLighting())) {
			display.asPanel().addStyleName("lighting");
		}
		display.getZoneCard().addClickHandler(event -> {
			String token = AppToken.newZoneDashboardToken(zone.getId());
			eventBus.fireEvent(new ChangeTokenEvent(token));
			eventBus.fireEvent(new ChangeZoneEvent(zone));
		});
	}

	@Override
	public void show(HasWidgets container) {
		container.add(display.asPanel());
	}

	protected IRpcServiceProvider getRpcProvider() {
		return rpcService;
	}

	public ZoneDto getZone() {
		return zone;
	}
	
	public void updateZone(ZoneDto zoneDto) {
		zone.setLighting(zoneDto.getLighting());
		zone.setDevicesCount(zoneDto.getDevicesCount());
		display.getLblDevices().setText(zone.getDevicesCount() + " " + ProjectLocale.getProjectConst().device());
		display.asPanel().removeStyleName("lighting");
		display.getIcon().removeStyleName("lighting");
		if (Boolean.TRUE.equals(zoneDto.getLighting())) {
			display.asPanel().addStyleName("lighting");
			display.getIcon().addStyleName("lighting");
		}
	}

	public boolean isLocked() {
		return locked;
	}

	public void setLocked(boolean lock) {
		locked = lock;
	}
}
