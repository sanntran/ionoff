package net.ionoff.center.client.sensor;

import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.dom.client.KeyCodes;
import com.google.gwt.event.dom.client.KeyUpEvent;
import com.google.gwt.event.dom.client.KeyUpHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.HasWidgets;

import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.ui.MaterialCollapsible;
import gwt.material.design.client.ui.MaterialIcon;
import gwt.material.design.client.ui.MaterialLabel;
import gwt.material.design.client.ui.MaterialTextBox;
import net.ionoff.center.client.base.AbstractPresenter;
import net.ionoff.center.client.event.ShowMessageEvent;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.ModeSensorDto;
import net.ionoff.center.shared.dto.ModeSensorSceneDto;
import net.ionoff.center.shared.dto.ModeSensorUserDto;

public class ModeSensorPresenter extends AbstractPresenter {
	
	public interface Display  {
		MaterialCollapsible asPanel();
		
		MaterialIcon getIconEnabled();
		MaterialIcon getBtnDelete();
		MaterialLabel getLblModeName();
		MaterialLabel getLblCondition();
		
		MaterialTextBox getTextBoxCondition();
		FlowPanel getModeSensorScenesPanel();		
		FlowPanel getModeSensorUsersPanel();

		MaterialTextBox getTextBoxMesage();
	}

	private enum EventType {
		CLICK_ENABLED, TEXTBOX_ENTER;
	}
	private boolean isDirty;
	private final Display display;
	private ModeSensorDto modeSensor;
	private final IRpcServiceProvider rpcProvider;
	
	public ModeSensorPresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus,
			ModeSensorDto modeSensor, Display view) {
		super(eventBus);
		this.display = view;
		this.modeSensor = modeSensor;
		this.rpcProvider= rpcProvider;
	}
	
	@Override
	public void go() {
		bind();
		isDirty = false;
	}

	private void bind() {
		display.getLblModeName().setText(modeSensor.getModeName() == null ?
				AdminLocale.getAdminConst().all() : 
				BaseDto.formatNameID(modeSensor.getModeName(), modeSensor.getModeId()));
		if (Boolean.TRUE.equals(modeSensor.getEnabled())) {
			display.getIconEnabled().setIconType(IconType.CHECK);
		}
		else {
			display.getIconEnabled().setIconType(IconType.CHECK_BOX_OUTLINE_BLANK);
		}
		display.getLblCondition().setText(modeSensor.getCondition());
		display.getTextBoxCondition().setText(modeSensor.getCondition());
		display.getTextBoxMesage().setText(modeSensor.getMessage());
		
		display.getIconEnabled().addClickHandler(e -> {
			isDirty = true;
			if (Boolean.TRUE.equals(modeSensor.getEnabled())) {
				modeSensor.setEnabled(false);
			}
			else {
				modeSensor.setEnabled(true);
			}
			updateModeSensor(EventType.CLICK_ENABLED);
		});
		display.getTextBoxCondition().addKeyUpHandler(new KeyUpHandler() {
			@Override
			public void onKeyUp(KeyUpEvent event) {
				isDirty = true;
				if (event.getNativeKeyCode() == KeyCodes.KEY_ENTER) {
					updateModeSensor(EventType.TEXTBOX_ENTER);
				}
			}
		});
		display.getTextBoxMesage().addKeyUpHandler(new KeyUpHandler() {
			@Override
			public void onKeyUp(KeyUpEvent event) {
				isDirty = true;
				if (event.getNativeKeyCode() == KeyCodes.KEY_ENTER) {
					updateModeSensor(EventType.TEXTBOX_ENTER);
				}
			}
		});
		
		showModeSensorScenes(modeSensor.getScenes());
		showModeSensorUsers(modeSensor.getUsers());
	}

	private void showModeSensorUsers(List<ModeSensorUserDto> modeSensorUsers) {
		display.getModeSensorUsersPanel().clear();
		if (modeSensorUsers == null) {
			return;
		}
		for (ModeSensorUserDto modeSensorUserDto : modeSensorUsers) {
			ModeSensorUserView modeSensorUserView = new ModeSensorUserView(modeSensorUserDto);
			ModeSensorUserPresenter modeSensorUserPresenter = 
					new ModeSensorUserPresenter(modeSensorUserDto, rpcProvider, eventBus, modeSensorUserView);
			modeSensorUserPresenter.go();
			display.getModeSensorUsersPanel().add(modeSensorUserView);
		}
	}

	private void showModeSensorScenes(List<ModeSensorSceneDto> modeSensorScenes) {
		
		display.getModeSensorScenesPanel().clear();
		if (modeSensorScenes == null) {
			return;
		}
		for (ModeSensorSceneDto modeSensorSceneDto : modeSensorScenes) {
			ModeSensorSceneView zoneSceneView = new ModeSensorSceneView();
			ModeSensorScenePresenter areaScenePresenter = 
					new ModeSensorScenePresenter(rpcProvider, eventBus, zoneSceneView);
			areaScenePresenter.setModeSensorScene(modeSensorSceneDto);
			areaScenePresenter.go();
			display.getModeSensorScenesPanel().add(zoneSceneView);
		}
	}

	void updateModeSensor(EventType event) {
		if (!isDirty) {
			return;
		}
		modeSensor.setCondition(display.getTextBoxCondition().getText());
		modeSensor.setMessage(display.getTextBoxMesage().getText());
		rpcProvider.getModeSensorService().save(modeSensor.getId(), modeSensor,  
				new MethodCallback<ModeSensorDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
				if (EventType.CLICK_ENABLED.equals(event)) {
					if (Boolean.TRUE.equals(modeSensor.getEnabled())) {
						modeSensor.setEnabled(false);
						display.getIconEnabled().setIconType(IconType.CHECK_BOX_OUTLINE_BLANK);
					}
					else {
						modeSensor.setEnabled(true);
						display.getIconEnabled().setIconType(IconType.CHECK);
					}
				}
			}
			@Override
			public void onSuccess(Method method, ModeSensorDto result) {
				eventBus.fireEvent(new ShowMessageEvent(AdminLocale.getAdminMessages().updateSuccess(),
						ShowMessageEvent.SUCCESS));
				modeSensor = result;
				display.getLblCondition().setText(modeSensor.getCondition());
				if (Boolean.TRUE.equals(modeSensor.getEnabled())) {
					display.getIconEnabled().setIconType(IconType.CHECK);
				}
				else {
					display.getIconEnabled().setIconType(IconType.CHECK_BOX_OUTLINE_BLANK);
				}
				isDirty = false;
			}
		});
	}

	@Override
	public void show(HasWidgets container) {
		container.clear();
		container.add(display.asPanel());
	}
}
