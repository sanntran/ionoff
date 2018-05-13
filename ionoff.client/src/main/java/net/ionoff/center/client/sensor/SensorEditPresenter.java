package net.ionoff.center.client.sensor;


import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.HasWidgets;

import gwt.material.design.client.ui.MaterialButton;
import gwt.material.design.client.ui.MaterialIntegerBox;
import gwt.material.design.client.ui.MaterialListBox;
import net.ionoff.center.client.base.AbstractEditPresenter;
import net.ionoff.center.client.base.IEditView;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.event.ShowMessageEvent;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.EntityService;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.AppToken;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.ModeDto;
import net.ionoff.center.shared.dto.ModeSensorDto;
import net.ionoff.center.shared.dto.RelayDriverDto;
import net.ionoff.center.shared.dto.SensorDto;

public class SensorEditPresenter extends AbstractEditPresenter<SensorDto> {

	public interface Display extends IEditView<SensorDto> {
		MaterialIntegerBox getIntBoxInputIndex();
		MaterialListBox getListBoxGateways();
		FlowPanel getPanelActions();
		FlowPanel getPanelAddAction();
		MaterialListBox getListBoxModes();
		MaterialButton getBtnAddModeAction();
	}
	
	private final Display view;
	private SensorDto entityDto;
	private SensorTablePresenter sensorManager;
	protected IRpcServiceProvider rpcProvider;

	public SensorEditPresenter(IRpcServiceProvider rpcProvider, 
			HandlerManager eventBus, Display view, SensorTablePresenter sensorManager) {
		super(rpcProvider, eventBus, view);
		this.rpcProvider = rpcProvider;
		this.view = view;
		this.sensorManager = sensorManager;
	}

	@Override
	protected void bind() {
		view.getBtnSave().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				save();
			}
		});
		view.getBtnClose().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				sensorManager.hideEditForm();
			}
		});
		view.getBtnCancel().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				sensorManager.hideEditForm();
			}
		});
		view.getBtnAddModeAction().addClickHandler(event -> createModeSensor());
	}

	private void createModeSensor() {
		ModeSensorDto modeSensor = new ModeSensorDto();
		String selectedMode = view.getListBoxModes().getSelectedValue();
		Long modeId = null;
		try {
			modeId = BaseDto.parseIdFromFormattedNameID(selectedMode);
		}
		catch (Exception e) {
			// no mode selecected
		}
		modeSensor.setModeId(modeId);
		modeSensor.setSensorId(entityDto.getId());
		modeSensor.setEnabled(false);
		rpcProvider.getModeSensorService().save(modeSensor.getId(), modeSensor, 
				new MethodCallback<ModeSensorDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}

			@Override
			public void onSuccess(Method method, ModeSensorDto response) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				showModeSensor(response);
			}
		});
	}

	@Override
	public void go() {
		bind();
	}

	@Override
	public void show(HasWidgets container) {
		//
	}

	@Override
	protected void save() {
		if (entityDto == null) {
			return;
		}
		
		String newName = view.getTextBoxName().getValue();
		if (!validateInputStringValue(AdminLocale.getAdminConst().name(), newName)) {
			return;
		}
		
		entityDto.setName(newName);
		
		int selectedRelayDriverIndex = view.getListBoxGateways().getSelectedIndex();
		String selectedItem = view.getListBoxGateways().getItemText(selectedRelayDriverIndex);
		if (selectedRelayDriverIndex == 0) {
			entityDto.setDriverId(null);
			entityDto.setDriverName(null);
		}
		else {
			entityDto.setDriverId(BaseDto.parseIdFromFormattedNameID(selectedItem));
			entityDto.setDriverName(BaseDto.parseNameFromFormattedNameID(selectedItem));
		}
		int newInput = view.getIntBoxInputIndex().getValue();
		entityDto.setIndex(newInput);
		
		rpcProvider.getSensorService().save(entityDto.getId(), entityDto, 
				new MethodCallback<SensorDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, SensorDto result) {
				sensorManager.onSavedSucess(result);
			}
		});
	}

	@Override
	protected String getClazz() {
		return SensorDto.class.getSimpleName();
	}

	@Override
	protected EntityService<SensorDto> getRpcService() {
		return rpcProvider.getSensorService();
	}

	public void setEntityDto(SensorDto dto) {
		entityDto = dto;
		if (dto.getDeviceId() == null || dto.izNew()) {
			loadRelayDrivers();
		}
		else {
			setGatewayOptions(BaseDto.formatNameID(dto.getDeviceName(), dto.getDeviceId()));
			updateView(dto);
		}
		if (dto.izNew()) {
			view.getPanelActions().setVisible(false);
			view.getPanelAddAction().setVisible(false);
		}
		else {
			loadModesByProject();
			loadAndShowModeSensors();
			view.getPanelActions().setVisible(true);
			view.getPanelAddAction().setVisible(true);
		}
	}
	
	private void loadAndShowModeSensors() {
		rpcProvider.getModeSensorService().findBySensorId(entityDto.getId(), new MethodCallback<List<ModeSensorDto>>() {
			@Override
			public void onSuccess(Method method, List<ModeSensorDto> response) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				showModeSensors(response);
			}

			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);	
			}
		});
	}
	private void showModeSensors(List<ModeSensorDto> modeSensors) {
		view.getPanelActions().clear();
		for (ModeSensorDto modeSensor : modeSensors) {
			showModeSensor(modeSensor);
		}
	}
	
	private void showModeSensor(ModeSensorDto modeSensor) {
		ModeSensorView modeSensorView = new ModeSensorView();
		view.getPanelActions().add(modeSensorView);
		ModeSensorPresenter modeSensorPresenter 
					= new ModeSensorPresenter(rpcProvider, eventBus, modeSensor, modeSensorView);
		modeSensorPresenter.go();

		modeSensorView.getBtnDelete().addClickHandler(e -> {
			deleteModeSensor(modeSensor, modeSensorView);
		});
	}

	private void deleteModeSensor(ModeSensorDto modeSensor, final ModeSensorView modeSensorView) {
		rpcProvider.getModeSensorService().delete(modeSensor.getId(), new MethodCallback<MessageDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, MessageDto result) {
				eventBus.fireEvent(new ShowMessageEvent(AdminLocale.getAdminMessages().deleteSuccess(),
						ShowMessageEvent.SUCCESS));
				view.getPanelActions().remove(modeSensorView);
			}
		});
	}

	private void loadModesByProject() {
		rpcProvider.getModeService().findByProjectId(getProjectId(), new MethodCallback<List<ModeDto>>() {
			@Override
			public void onSuccess(Method method, List<ModeDto> response) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				fillListBoxModes(response);
			}
			
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);	
			}
		});
	}

	private void loadRelayDrivers() {
		rpcProvider.getRelayDriverService().findByProjectId(getProjectId(), 
				new MethodCallback<List<RelayDriverDto>>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}

			@Override
			public void onSuccess(Method method, List<RelayDriverDto> result) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				setGatewayOptions(result);
				updateView(entityDto);
			}
		});
	}

	protected Long getProjectId() {
		if (AppToken.hasTokenItem(AppToken.PROJECT)) {
			return AppToken.getProjectIdLong();
		}
		return null;
	}

	private void updateView(SensorDto dto) {
		view.getLblId().setText("#" + dto.getId());
		view.getLblName().setText(dto.getName());
		view.getTextBoxName().setText(dto.getName());
		
		if (dto.getDeviceId() == null) {
			view.getListBoxGateways().setEnabled(true);
			view.getListBoxGateways().setSelectedValue(
					BaseDto.formatNameID(dto.getDriverName(), dto.getDriverId()));
			view.getIntBoxInputIndex().setVisible(true);
		}
		else {
			view.getListBoxGateways().setEnabled(false);
			view.getIntBoxInputIndex().setVisible(false);
			view.getListBoxGateways().setSelectedIndex(0);
		}
		if (dto.getIndex() == null) {
			view.getIntBoxInputIndex().setValue(0);;
		}
		else {
			view.getIntBoxInputIndex().setValue(dto.getIndex());
		}
	}

	public void setGatewayOptions(List<RelayDriverDto> options) {
		view.getListBoxGateways().clear();
		view.getListBoxGateways().setEnabled(true);
		view.getListBoxGateways().addItem(AdminLocale.getAdminConst().none());
		if (options == null || options.isEmpty()) {
			return;
		}
		for (final RelayDriverDto option : options) {
			view.getListBoxGateways().addItem(option.formatNameID());
		}
		view.getIntBoxInputIndex().setVisible(true);
	}

	public void setGatewayOptions(String deviceNameId) {
		view.getListBoxGateways().clear();
		view.getListBoxGateways().addItem(deviceNameId);
		view.getListBoxGateways().setEnabled(false);
		view.getIntBoxInputIndex().setVisible(false);
	}
	
	public void fillListBoxModes(List<ModeDto> modes) {
		view.getListBoxModes().clear();
		view.getListBoxModes().addItem(AdminLocale.getAdminConst().all());
		if (modes == null) {
			return;
		}
		for (final ModeDto mode : modes) {
			view.getListBoxModes().addItem(mode.formatNameID());
		}
		if (!modes.isEmpty()) {
			view.getListBoxModes().setSelectedIndex(0);
		}
	}
	
}
