package net.ionoff.center.client.sensor;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.logical.shared.ValueChangeEvent;
import com.google.gwt.event.logical.shared.ValueChangeHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.Widget;

import gwt.material.design.client.ui.MaterialListBox;
import net.ionoff.center.client.base.AbstractPresenter;
import net.ionoff.center.client.event.ShowMessageEvent;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.ModeSensorSceneDto;

public class ModeSensorScenePresenter extends AbstractPresenter {
	
	public interface Display  {
		Widget asWidget();
		Label getLblZoneName();
		MaterialListBox getListBoxScenes();
	}
	
	private final IRpcServiceProvider rpcProvider;
	private final Display display;
	private ModeSensorSceneDto modeSensorScene;
	
	public ModeSensorScenePresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus,
			Display view) {
		super(eventBus);
		this.rpcProvider = rpcProvider;
		this.display = view;
	}
	
	public void setModeSensorScene(ModeSensorSceneDto modeSensorScene) {
		this.modeSensorScene = modeSensorScene;
		display.getLblZoneName().setText(BaseDto.formatNameID(modeSensorScene.getZoneName(), modeSensorScene.getZoneId()));
		display.getListBoxScenes().clear();
		display.getListBoxScenes().addItem(AdminLocale.getAdminConst().none());
		if (modeSensorScene.getSceneNameIds() == null) {
			return;
		}
		String sceneNameId = BaseDto.formatNameID(modeSensorScene.getSceneName(), modeSensorScene.getSceneId());
		int sceneNameIdsSize = modeSensorScene.getSceneNameIds().size();
		
		for (int i = 0; i < sceneNameIdsSize; i++) {
			String scene = modeSensorScene.getSceneNameIds().get(i);
			display.getListBoxScenes().addItem(scene);
			if (sceneNameId.equals(scene)) {
				display.getListBoxScenes().setSelectedIndex(i + 1);
			}
		}
	}
	
	@Override
	public void go() {
		bind();
	}

	private void bind() {
		display.getListBoxScenes().addValueChangeHandler(new ValueChangeHandler<String>() {
			@Override
			public void onValueChange(ValueChangeEvent<String> event) {
				onChaneSensorScene();
			}
		});
	}

	private void onChaneSensorScene() {
		
		if (modeSensorScene == null) {
			return;
		}
		
		int selectedIndex = display.getListBoxScenes().getSelectedIndex();
		if (selectedIndex == 0) {
			modeSensorScene.setSceneId(null);
			modeSensorScene.setSceneName(null);
		}
		else {
			String selectedSensorSceneNameId = display.getListBoxScenes().getItemText(selectedIndex);
			modeSensorScene.setSceneId(BaseDto.parseIdFromFormattedNameID(selectedSensorSceneNameId));
			modeSensorScene.setSceneName(BaseDto.parseNameFromFormattedNameID(selectedSensorSceneNameId));
		}
		
		rpcProvider.getModeSensorSceneService().update(modeSensorScene.getId(), 
				modeSensorScene, new MethodCallback<ModeSensorSceneDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, ModeSensorSceneDto result) {
				eventBus.fireEvent(new ShowMessageEvent(AdminLocale.getAdminMessages().updateSuccess(),
						ShowMessageEvent.SUCCESS));
			}
		});
	}

	@Override
	public void show(HasWidgets container) {
		// does nothing
	}
}
