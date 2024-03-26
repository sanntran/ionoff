package net.ionoff.center.client.sensor;


import java.util.ArrayList;
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
import net.ionoff.center.shared.dto.ControllerDto;
import net.ionoff.center.shared.dto.SensorDto;
import net.ionoff.center.shared.entity.SensorType;

public class SensorEditPresenter extends AbstractEditPresenter<SensorDto> {

	public interface Display extends IEditView<SensorDto> {
		MaterialIntegerBox getIntBoxOrder();
		MaterialIntegerBox getIntBoxInputIndex();
		MaterialListBox getListBoxGateways();
		MaterialListBox getListBoxTypes();
	}
	
	private final Display view;
	private SensorDto entityDto;
	private SensorTablePresenter sensorManager;
	protected IRpcServiceProvider rpcProvider;
	private final List<ModeSensorPresenter> modeSensorPresenters;

	public SensorEditPresenter(IRpcServiceProvider rpcProvider, 
			HandlerManager eventBus, Display view, SensorTablePresenter sensorManager) {
		super(rpcProvider, eventBus, view);
		this.rpcProvider = rpcProvider;
		this.view = view;
		this.sensorManager = sensorManager;
		this.modeSensorPresenters = new ArrayList<>();
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

		view.getTextBoxName().addValueChangeHandler(event -> isDirty = true);
		view.getIntBoxOrder().addValueChangeHandler(event -> isDirty = true);
		view.getIntBoxInputIndex().addValueChangeHandler(event -> isDirty = true);
		view.getListBoxGateways().addValueChangeHandler(event -> isDirty = true);
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
		saveSensorDto();
		saveAllModeSensors();
	}

	private void saveAllModeSensors() {
		for (ModeSensorPresenter modeSensorPresenter : modeSensorPresenters) {
			modeSensorPresenter.updateModeSensor(null);
		}
	}

	private void saveSensorDto() {
		if (!isDirty) {
			eventBus.fireEvent(new ShowMessageEvent(AdminLocale.getAdminMessages().updateSuccess(),
					ShowMessageEvent.SUCCESS));
			return;
		}
		
		String newName = view.getTextBoxName().getValue();
		Long newDriverId = null;
		String newDriverName = null;
		int newInput = view.getIntBoxInputIndex().getValue();
		
		int selectedControllerIndex = view.getListBoxGateways().getSelectedIndex();
		String selectedItem = view.getListBoxGateways().getItemText(selectedControllerIndex);
		if (selectedControllerIndex == 0) {
			//
		}
		else {
			newDriverId = BaseDto.parseIdFromFormattedNameID(selectedItem);
			newDriverName = BaseDto.parseNameFromFormattedNameID(selectedItem);
		}
		
		if (!validateInputStringValue(AdminLocale.getAdminConst().name(), newName)) {
			return;
		}
		
		entityDto.setName(newName);
		entityDto.setIndex(newInput);
		entityDto.setOrder(view.getIntBoxOrder().getValue());
		entityDto.setDriverId(newDriverId);
		entityDto.setDriverName(newDriverName);

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
		isDirty = false;
		entityDto = dto;
		updateView(dto);
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
		view.getIntBoxOrder().setValue(dto.getOrder());
		view.getTextBoxName().setText(dto.getName());
		
		view.getListBoxTypes().setEnabled(false);
		if (dto.getDeviceId() == null) {
			view.getListBoxGateways().setEnabled(true);
			view.getListBoxGateways().setSelectedValue(
					BaseDto.formatNameID(dto.getDriverName(), dto.getDriverId()));
			view.getIntBoxInputIndex().setVisible(true);
			view.getListBoxTypes().setSelectedValue(SensorType.DIGITAL.toString());
		}
		else {
			view.getListBoxGateways().setEnabled(false);
			view.getIntBoxInputIndex().setVisible(false);
			view.getListBoxGateways().setSelectedIndex(0);
			view.getListBoxTypes().setSelectedValue(SensorType.ANALOG.toString());
		}
		if (dto.getIndex() == null) {
			view.getIntBoxInputIndex().setValue(0);;
		}
		else {
			view.getIntBoxInputIndex().setValue(dto.getIndex());
		}
	}
	
}
