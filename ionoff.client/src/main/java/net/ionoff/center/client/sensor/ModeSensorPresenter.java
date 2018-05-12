package net.ionoff.center.client.sensor;

import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.HasWidgets;

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
	}
	
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
	}

	private void bind() {
		display.getLblModeName().setText(modeSensor.getModeName() == null ?
				AdminLocale.getAdminConst().all() : 
				BaseDto.formatNameID(modeSensor.getModeName(), modeSensor.getModeId()));
		
		display.getLblCondition().setText(modeSensor.getCondition());
		
		display.getIconEnabled().addClickHandler(e -> {
			if (Boolean.TRUE.equals(modeSensor.getEnabled())) {
				modeSensor.setEnabled(false);
			}
			else {
				modeSensor.setEnabled(true);
			}
			onUpdateModeSensor();
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

	private void onUpdateModeSensor() {
		rpcProvider.getModeSensorService().save(modeSensor.getId(), modeSensor,  
				new MethodCallback<ModeSensorDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, ModeSensorDto result) {
				eventBus.fireEvent(new ShowMessageEvent(AdminLocale.getAdminMessages().updateSuccess(),
						ShowMessageEvent.SUCCESS));
				modeSensor = result;
			}
		});
	}

	@Override
	public void show(HasWidgets container) {
		container.clear();
		container.add(display.asPanel());
	}
}
