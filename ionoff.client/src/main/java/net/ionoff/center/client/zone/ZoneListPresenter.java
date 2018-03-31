package net.ionoff.center.client.zone;


import java.util.ArrayList;
import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.Panel;

import gwt.material.design.client.ui.MaterialIcon;
import gwt.material.design.client.ui.MaterialRow;
import net.ionoff.center.client.common.AbstractPresenter;
import net.ionoff.center.client.event.ChangeTokenEvent;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.AppToken;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.ZoneDto;

public class ZoneListPresenter extends AbstractPresenter {

	public interface Display {
		Panel asPanel();
		MaterialRow getWrapper();
		MaterialIcon getIconSetting();
	}
	
	private Timer timer;
	private Display display;
	private IRpcServiceProvider rpcProvider;
	private final List<ZonePresenter> scenePresenters;
	
	public ZoneListPresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus, Display view) {		
		super(eventBus);
		this.display = view;
		this.rpcProvider = rpcProvider;
		scenePresenters = new ArrayList<>();
	}
	
	private void bind() {
		timer = new Timer() {
			@Override
			public void run() {
				if (!isVisible()) {
					timer.cancel();
				}
				else {
					rpcSyncStatus();
				}
			}
		};
		display.getIconSetting().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				String token = AppToken.newZoneTableToken();
				eventBus.fireEvent(new ChangeTokenEvent(token));
			}
		});
	}
	
	private void scheduleSyncStatus() {
		timer.scheduleRepeating(5000);
	}
	
	private void rpcSyncStatus() {
		rpcProvider.getZoneService().findByProjectId(AppToken.getProjectIdLong(), 
				new MethodCallback<List<ZoneDto>>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, List<ZoneDto> response) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				showZoneList(response);
			}
		});
	}
	
	private void showZoneList(List<ZoneDto> scenes) {
		for (final ZonePresenter scenePresenter : scenePresenters) {
			updateZoneStatus(scenePresenter, scenes);
		}
	}

	private void updateZoneStatus(ZonePresenter zonePresenter, List<ZoneDto> zoneDtos) {
		if (zonePresenter.isLocked()) {
			return;
		}
		for (final ZoneDto zone : zoneDtos) {
			if (zone.getId() == zonePresenter.getZone().getId()) {
				zonePresenter.updateZone(zone);
			}
		}
	}

	private void rpcLoadZonesByUser() {
		rpcGetZonesByProject(AppToken.getProjectIdLong());
	}
	
	private void rpcGetZonesByProject(long projectId) {
		rpcProvider.getZoneService().findByProjectId(projectId, new MethodCallback<List<ZoneDto>>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, List<ZoneDto> response) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				showZones(response);
			}
		});
	}
	
	private void showZones(List<ZoneDto> zones) {
		for (final ZoneDto zone : zones) {
			ZoneView sceneView = new ZoneView();
			ZonePresenter zonePresenter = new ZonePresenter(rpcProvider, eventBus, zone, sceneView);
			zonePresenter.go();
			scenePresenters.add(zonePresenter);
			zonePresenter.show(display.getWrapper());
		}
		scheduleSyncStatus();
	}

	protected boolean isVisible() {
		return AppToken.hasTokenItem(AppToken.ZONES) && !AppToken.hasTokenItem(AppToken.TABLE);
	}
	
	@Override
	public void go() {
		bind();
	}
	
	@Override
	public void show(HasWidgets container) {
		container.clear();
		container.add(display.asPanel());
		scenePresenters.clear();
		display.getWrapper().clear();
		rpcLoadZonesByUser();
	}
}
