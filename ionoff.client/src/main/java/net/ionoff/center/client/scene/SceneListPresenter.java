package net.ionoff.center.client.scene;


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
import net.ionoff.center.shared.dto.SceneDto;

public class SceneListPresenter extends AbstractPresenter {

	public interface Display {
		Panel asPanel();
		MaterialRow getWrapper();
		MaterialIcon getIconSetting();
	}
	
	private Timer timer;
	private Display display;
	private IRpcServiceProvider rpcProvider;
	private final List<ScenePresenter> scenePresenters;
	
	public SceneListPresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus, Display view) {		
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
				String token = AppToken.newSceneTableToken();
				eventBus.fireEvent(new ChangeTokenEvent(token));
			}
		});
	}
	
	private void scheduleSyncStatus() {
		timer.scheduleRepeating(5000);
	}
	
	private void rpcSyncStatus() {
		if (AppToken.hasTokenItem(AppToken.ZONE)) {
			rpcProvider.getSceneService().findByZoneId(AppToken.getZoneIdLong(), 
					new MethodCallback<List<SceneDto>>() {
				@Override
				public void onFailure(Method method, Throwable exception) {
					ClientUtil.handleRpcFailure(method, exception, eventBus);
				}
				@Override
				public void onSuccess(Method method, List<SceneDto> response) {
					eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
					updateScenesStatus(response);
				}
			});
		}
		else {
			rpcProvider.getSceneService().findByProjectId(AppToken.getProjectIdLong(), 
					new MethodCallback<List<SceneDto>>() {
				@Override
				public void onFailure(Method method, Throwable exception) {
					ClientUtil.handleRpcFailure(method, exception, eventBus);
				}
				@Override
				public void onSuccess(Method method, List<SceneDto> response) {
					eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
					updateScenesStatus(response);
				}
			});
		}
	}
	
	private void updateScenesStatus(List<SceneDto> scenes) {
		for (final ScenePresenter scenePresenter : scenePresenters) {
			updateSceneStatus(scenePresenter, scenes);
		}
	}

	private void updateSceneStatus(ScenePresenter scenePresenter, List<SceneDto> sceneDtos) {
		if (scenePresenter.isLocked()) {
			return;
		}
		for (final SceneDto scene : sceneDtos) {
			if (scene.getId() == scenePresenter.getScene().getId()) {
				scenePresenter.updateScene(scene);
			}
		}
	}

	private void rpcLoadScenesByUser() {
		if (AppToken.hasTokenItem(AppToken.ZONE)) {
			rpcGetScenesByZone(AppToken.getZoneIdLong());
		}
		else {
			rpcGetScenesByProject(AppToken.getProjectIdLong());
		}
	}
	
	private void rpcGetScenesByProject(long projectId) {
		rpcProvider.getSceneService().findByProjectId(projectId, new MethodCallback<List<SceneDto>>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, List<SceneDto> response) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				showScenes(response);
			}
		});
	}

	private void rpcGetScenesByZone(long zoneId) {
		rpcProvider.getSceneService().findByZoneId(zoneId, new MethodCallback<List<SceneDto>>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, List<SceneDto> response) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				showScenes(response);
			}
		});
	}

	
	private void showScenes(List<SceneDto> scenees) {
		for (final SceneDto scene : scenees) {
			SceneView sceneView = new SceneView();
			ScenePresenter scenePresenter = new ScenePresenter(rpcProvider, eventBus, scene, sceneView);
			scenePresenter.go();
			scenePresenters.add(scenePresenter);
			scenePresenter.show(display.getWrapper());
		}
		scheduleSyncStatus();
	}

	protected boolean isVisible() {
		return AppToken.hasTokenItem(AppToken.SCENES) && !AppToken.hasTokenItem(AppToken.TABLE);
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
		rpcLoadScenesByUser();
	}
}
