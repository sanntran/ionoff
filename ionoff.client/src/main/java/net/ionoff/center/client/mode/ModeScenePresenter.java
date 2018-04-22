package net.ionoff.center.client.mode;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.logical.shared.ValueChangeEvent;
import com.google.gwt.event.logical.shared.ValueChangeHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.Label;

import gwt.material.design.client.ui.MaterialListBox;
import net.ionoff.center.client.base.AbstractPresenter;
import net.ionoff.center.client.event.ShowMessageEvent;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.ModeSceneDto;

public class ModeScenePresenter extends AbstractPresenter {
	
	public interface Display  {
		FlowPanel asPanel();
		Label getLblAreaName();
		MaterialListBox getListBoxScenes();
	}
	
	private final IRpcServiceProvider rpcProvider;
	private final Display display;
	private ModeSceneDto modeScene;
	
	public ModeScenePresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus,
			Display view) {
		super(eventBus);
		this.rpcProvider= rpcProvider;
		this.display = view;
	}
	
	public void setModeScene(ModeSceneDto modeScene) {
		this.modeScene = modeScene;
		display.getLblAreaName().setText(BaseDto.formatNameID(modeScene.getZoneName(), modeScene.getZoneId()));
		display.getListBoxScenes().clear();
		display.getListBoxScenes().addItem(AdminLocale.getAdminConst().none());
		if (modeScene.getSceneNameIds() == null) {
			return;
		}
		String sceneNameId = BaseDto.formatNameID(modeScene.getSceneName(), modeScene.getSceneId());
		int sceneNameIdsSize = modeScene.getSceneNameIds().size();
		
		for (int i = 0; i < sceneNameIdsSize; i++) {
			String scene = modeScene.getSceneNameIds().get(i);
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
				save();
			}
		});
	}

	public void save() {
		if (modeScene == null) {
			return;
		}
		int selectedIndex = display.getListBoxScenes().getSelectedIndex();
		if (selectedIndex == 0) {
			modeScene.setSceneId(null);
			modeScene.setSceneName(null);
		}
		else {
			String selectedSceneNameId = display.getListBoxScenes().getItemText(selectedIndex);
			modeScene.setSceneId(BaseDto.parseIdFromFormattedNameID(selectedSceneNameId));
			modeScene.setSceneName(BaseDto.parseNameFromFormattedNameID(selectedSceneNameId));
		}
		
		rpcProvider.getModeSceneService().save(modeScene.getId(), modeScene, new MethodCallback<ModeSceneDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, ModeSceneDto result) {
				eventBus.fireEvent(new ShowMessageEvent(AdminLocale.getAdminMessages().updateSuccess(),
						ShowMessageEvent.SUCCESS));
			}
		});
	}

	@Override
	public void show(HasWidgets container) {
		container.add(display.asPanel());
	}
}
