package net.ionoff.center.client.scene;

import java.util.ArrayList;
import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.HasWidgets;

import gwt.material.design.client.ui.MaterialCollapsibleHeader;
import gwt.material.design.client.ui.MaterialCollapsibleItem;
import gwt.material.design.client.ui.MaterialIntegerBox;
import net.ionoff.center.client.common.AbstractPresenter;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.schedule.PlayerActionView;
import net.ionoff.center.client.schedule.RelayActionView;
import net.ionoff.center.client.schedule.SceneActionPresenter;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.SceneActionDto;
import net.ionoff.center.shared.dto.SceneDeviceDto;
import net.ionoff.center.shared.dto.ScenePlayerActionDto;
import net.ionoff.center.shared.dto.SceneRelayActionDto;


public class SceneDevicePresenter extends AbstractPresenter {
	
	private SceneDeviceDto sceneDevice;
	
	public interface Display {
		MaterialCollapsibleItem asPanel();
		MaterialCollapsibleHeader getCollapsibleHeader();
		HasWidgets getDeviceActionsContainer();
		MaterialIntegerBox getTntBoxOrder();
		MaterialIntegerBox getIntBoxDuration();
	}
	
	private boolean isLoaded;
	private final Display display;
	private final IRpcServiceProvider rpcProvider;
	private final List<SceneActionPresenter> actionPresenters;
	
	public SceneDevicePresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus,
			SceneDeviceDto sceneDevice, Display view) {
		super(eventBus);
		this.display = view;
		this.rpcProvider = rpcProvider;
		actionPresenters = new ArrayList<>();
		isLoaded = false;
		this.sceneDevice = sceneDevice;
	}

	@Override
	public void go() {
		bind();
	}

	private void bind() {
		display.getCollapsibleHeader().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (!isLoaded) {
					loadSceneActions();
				}
			}
		});
	}

	private void loadSceneActions() {
		rpcProvider.getSceneActionService().findBySceneDevice(sceneDevice.getId(), 
				new MethodCallback<List<SceneActionDto>>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, List<SceneActionDto> result) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				isLoaded = true;
				showSceneActions(result);
			}
		});
	}

	private void showSceneActions(List<SceneActionDto> sceneActions) {
		display.getDeviceActionsContainer().clear();
		for (final SceneActionDto sceneAction : sceneActions) {
			if (sceneAction instanceof SceneRelayActionDto) {
				RelayActionView actionView = new RelayActionView();
				SceneRelayActionPresenter actionPresenter = new SceneRelayActionPresenter(rpcProvider, eventBus, actionView);
				actionPresenter.setTarget((SceneRelayActionDto)sceneAction);
				actionPresenter.go();
				display.getDeviceActionsContainer().add(actionView);
				actionPresenters.add(actionPresenter);
			}
			else if (sceneAction instanceof ScenePlayerActionDto) {
				PlayerActionView actionView = new PlayerActionView();
				ScenePlayerActionPresenter actionPresenter = new ScenePlayerActionPresenter(rpcProvider, eventBus, actionView);
				actionPresenter.setTarget((ScenePlayerActionDto)sceneAction);
				actionPresenter.go();
				display.getDeviceActionsContainer().add(actionView);
				actionPresenters.add(actionPresenter);
			}
		}
	}
	
	public void save(MethodCallback<Integer> callback) {
		if (!isLoaded) {
			return;
		}
		sceneDevice.setOrder(display.getTntBoxOrder().getValue());
		sceneDevice.setDuration(display.getIntBoxDuration().getValue());
		
		List<SceneActionDto> sceneActions = new ArrayList<>();
		for (SceneActionPresenter action : actionPresenters) {
			action.save();
			sceneActions.add((SceneActionDto) action.getTarget());
		}
		
		sceneDevice.setActions(sceneActions);
		
		rpcProvider.getSceneService().saveSceneDevice(sceneDevice.getId(), sceneDevice, 
				new MethodCallback<SceneDeviceDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, SceneDeviceDto result) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				callback.onSuccess(method, 1);
			}
		});
	}
	
	@Override
	public void show(HasWidgets container) {
		//does nothing
	}

	public boolean isEdited() {
		return isLoaded;
	}
}
