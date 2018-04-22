package net.ionoff.center.client.mode;

import java.util.ArrayList;
import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.dom.client.ChangeEvent;
import com.google.gwt.event.dom.client.ChangeHandler;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.logical.shared.ValueChangeEvent;
import com.google.gwt.event.logical.shared.ValueChangeHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.CheckBox;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.Label;

import gwt.material.design.client.ui.MaterialButton;
import gwt.material.design.client.ui.MaterialIntegerBox;
import gwt.material.design.client.ui.MaterialListBox;
import net.ionoff.center.client.base.AbstractPresenter;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.event.ShowMessageEvent;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.ModeSensorDto;
import net.ionoff.center.shared.dto.ModeSensorSceneDto;
import net.ionoff.center.shared.dto.ModeSensorUserDto;

public class ModeSensorsSettingPresenter extends AbstractPresenter {
	
	public interface Display  {
		FlowPanel asPanel();
		CheckBox getCheckBoxEnabled(); 
		Label getLblTabDetectedHuman(); 
		Label getLblTabDetectedNoHuman(); 
		FlowPanel getSensorScenesPanel();
		FlowPanel getModeSensorSettingPanel();
		MaterialListBox getListBoxSensors();
		MaterialIntegerBox getIntBoxTimeBuffer();
		FlowPanel getSensorUsersPanel();
		MaterialButton getBtnSaveTimeBuffer();
	}
	
	private final IRpcServiceProvider rpcProvider;
	private final Display display;
	
	private ModeSensorDto selectedModeSensor;
	private final List<ModeSensorDto> modeSensors;
	
	public ModeSensorsSettingPresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus,
			Display view) {
		super(eventBus);
		this.rpcProvider= rpcProvider;
		this.display = view;
		modeSensors = new ArrayList<ModeSensorDto>();
	}
	
	@Override
	public void go() {
		bind();
	}

	private void bind() {
		display.getCheckBoxEnabled().addValueChangeHandler(new ValueChangeHandler<Boolean>() {
			@Override
			public void onValueChange(ValueChangeEvent<Boolean> event) {
				onUpdateModeSensor();
			}
		});
		
		display.getIntBoxTimeBuffer().addChangeHandler(new ChangeHandler() {
			@Override
			public void onChange(ChangeEvent event) {
				String value = display.getIntBoxTimeBuffer().getText();
				try {
					Integer.parseInt(value);
					onUpdateModeSensor();
				}
				catch (NumberFormatException e) {
					display.getIntBoxTimeBuffer().setValue(0);
				}
			}
		});
		
		display.getBtnSaveTimeBuffer().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onUpdateModeSensor();
			}
		});
		
		display.getLblTabDetectedHuman().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onSelectTabDetectedHuman();
			}
		});
		
		display.getLblTabDetectedNoHuman().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onSelectTabDetectedNoHuman();
			}
		});
		
		display.getListBoxSensors().addValueChangeHandler(new ValueChangeHandler<String>() {
			
			@Override
			public void onValueChange(ValueChangeEvent<String> event) {
				int selectedIdx = display.getListBoxSensors().getSelectedIndex();
				selectSensorNameId(display.getListBoxSensors().getItemText(selectedIdx));
			}
		});
	}

	
	public void setModeSensors(List<ModeSensorDto> mSensors) {
		display.getListBoxSensors().clear();
		modeSensors.clear();
		if (mSensors == null) {
			return;
		}
		for (final ModeSensorDto modeSensor : mSensors) {
			modeSensors.add(modeSensor);
			display.getListBoxSensors().addItem(
					BaseDto.formatNameID(modeSensor.getSensorName(), modeSensor.getSensorId()));
		}
		if (!modeSensors.isEmpty()) {
			display.getListBoxSensors().setSelectedIndex(0);
			selectSensorNameId(display.getListBoxSensors().getItemText(0));
		}
		else {
			selectSensorNameId(null);
		}
	}
	
	protected void selectSensorNameId(String sensorNameId) {
		ModeSensorDto selectedSensor = null;
		for (ModeSensorDto modeSensor : modeSensors) {
			Long sensorId = BaseDto.parseIdFromFormattedNameID(sensorNameId);
			if (modeSensor.getSensorId().equals(sensorId)) {
				selectedSensor = modeSensor;
				break;
			}
		}
		
		if (selectedSensor == null) {
			selectModeSensor(null);
			display.getModeSensorSettingPanel().setVisible(false);
			return;
		}
		else {
			selectModeSensor(selectedSensor);
			display.getModeSensorSettingPanel().setVisible(true);
		}
	}

	private void selectModeSensor(ModeSensorDto modeSensor) {
		selectedModeSensor = modeSensor;
		selectTabDetectedHuman();
		updateCheckBoxEnabled(modeSensor);
		updateListBoxInterval(modeSensor);
		rpcFindModeSensorScenes(true);
		rpcFindModeSensorUsers(true);
	}

	private void updateListBoxInterval(ModeSensorDto modeSensor) {
		if (modeSensor.getTimeBuffer() == null) {
			display.getIntBoxTimeBuffer().setValue(0);
			return;
		}
		display.getIntBoxTimeBuffer().setValue(modeSensor.getTimeBuffer());
	}

	private void updateCheckBoxEnabled(ModeSensorDto modeSensor) {
		display.getCheckBoxEnabled().setValue(modeSensor.getEnabled());
	}

	private void selectTabDetectedHuman() {
		display.getLblTabDetectedHuman().setStyleName("lblTab selected");
		display.getLblTabDetectedNoHuman().setStyleName("lblTab");
	}
	
	private void selectTabDetectedNoHuman() {
		display.getLblTabDetectedHuman().setStyleName("lblTab");
		display.getLblTabDetectedNoHuman().setStyleName("lblTab selected");
	}

	
	private void onSelectTabDetectedNoHuman() {
		if (selectedModeSensor == null) {
			return;
		}
		selectTabDetectedNoHuman();
		rpcFindModeSensorScenes(false);
		rpcFindModeSensorUsers(false);
	}

	private void onSelectTabDetectedHuman() {
		if (selectedModeSensor == null) {
			return;
		}
		selectTabDetectedHuman();
		rpcFindModeSensorScenes(true);
		rpcFindModeSensorUsers(true);
	}
	
	private void rpcFindModeSensorUsers(boolean detectedHuman) {
		rpcProvider.getModeSensorUserService().findByModeSensorId(selectedModeSensor.getId(), 
				detectedHuman, new MethodCallback<List<ModeSensorUserDto>>() {
			
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, List<ModeSensorUserDto> result) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				showModeSensorUsers(result);
			}
		});
	}

	private void rpcFindModeSensorScenes(boolean detectedHuman) {
		
		rpcProvider.getModeSensorSceneService().findByModeSensorId(selectedModeSensor.getId(), 
				detectedHuman, new MethodCallback<List<ModeSensorSceneDto>>() {
			
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, List<ModeSensorSceneDto> result) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				showModeSensorScenes(result);
			}
		});
	}

	private void showModeSensorUsers(List<ModeSensorUserDto> modeSensorUsers) {
		display.getSensorUsersPanel().clear();
		if (modeSensorUsers == null) {
			return;
		}
		for (ModeSensorUserDto modeSensorUserDto : modeSensorUsers) {
			ModeSensorUserView modeSensorUserView = new ModeSensorUserView(modeSensorUserDto);
			ModeSensorUserPresenter modeSensorUserPresenter = 
					new ModeSensorUserPresenter(modeSensorUserDto, rpcProvider, eventBus, modeSensorUserView);
			modeSensorUserPresenter.go();
			display.getSensorUsersPanel().add(modeSensorUserView);
		}
	}

	private void showModeSensorScenes(List<ModeSensorSceneDto> modeSensorScenes) {
		
		display.getSensorScenesPanel().clear();
		if (modeSensorScenes == null) {
			return;
		}
		for (ModeSensorSceneDto modeSensorSceneDto : modeSensorScenes) {
			ModeSensorSceneView areaSceneView = new ModeSensorSceneView();
			ModeSensorScenePresenter areaScenePresenter = 
					new ModeSensorScenePresenter(rpcProvider, eventBus, areaSceneView);
			areaScenePresenter.setModeSensorScene(modeSensorSceneDto);
			areaScenePresenter.go();
			display.getSensorScenesPanel().add(areaSceneView);
		}
	}

	private void onUpdateModeSensor() {
		if (selectedModeSensor == null) {
			return;
		}
		selectedModeSensor.setEnabled(display.getCheckBoxEnabled().getValue());
		Integer intBoxValue = display.getIntBoxTimeBuffer().getValue();
		selectedModeSensor.setTimeBuffer(intBoxValue);
		rpcProvider.getModeSensorService().update(selectedModeSensor.getId(), selectedModeSensor,  
				new MethodCallback<ModeSensorDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, ModeSensorDto result) {
				eventBus.fireEvent(new ShowMessageEvent(AdminLocale.getAdminMessages().updateSuccess(),
						ShowMessageEvent.SUCCESS));
			}
		});
	}

	@Override
	public void show(HasWidgets container) {
		container.clear();
		container.add(display.asPanel());
	}
}
