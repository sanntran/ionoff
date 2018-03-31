package net.ionoff.center.client.sensor;


import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.HasWidgets;

import gwt.material.design.client.ui.MaterialIntegerBox;
import gwt.material.design.client.ui.MaterialListBox;
import net.ionoff.center.client.common.AbstractEditPresenter;
import net.ionoff.center.client.common.IEditView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.EntityService;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.ControllerDto;
import net.ionoff.center.shared.dto.SensorDto;

public class SensorEditPresenter extends AbstractEditPresenter<SensorDto> {

	public interface Display extends IEditView<SensorDto> {
		MaterialListBox getListBoxControllers();

		MaterialIntegerBox getIntBoxControllerInputIdx();
	}
	protected IRpcServiceProvider rpcProvider;
	
	private final Display view;
	private SensorDto entityDto;
	private SensorTablePresenter sensorManager;

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
		
		int selectedControllerIndex = view.getListBoxControllers().getSelectedIndex();
		String selectedItem = view.getListBoxControllers().getItemText(selectedControllerIndex);
		if (selectedControllerIndex == 0) {
			entityDto.setControllerId(null);
			entityDto.setControllerName(null);
		}
		else {
			entityDto.setControllerId(BaseDto.parseIdFromFormattedNameID(selectedItem));
			entityDto.setControllerName(BaseDto.parseNameFromFormattedNameID(selectedItem));
		}
		int newInput = view.getIntBoxControllerInputIdx().getValue();
		entityDto.setControllerInput(newInput);
		
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
		updateView(dto);
	}

	private void updateView(SensorDto dto) {
		view.getLblId().setText("#" + dto.getId());
		view.getLblName().setText(dto.getName());
		view.getTextBoxName().setText(dto.getName());
		
		if (dto.getControllerId() != null) {
			view.getListBoxControllers().setSelectedValue(BaseDto.formatNameID(dto.getControllerName(), dto.getControllerId()));
		}
		else {
			view.getListBoxControllers().setSelectedIndex(0);
		}
		
		if (dto.getControllerInput() == null) {
			view.getIntBoxControllerInputIdx().setValue(0);
		}
		else {
			view.getIntBoxControllerInputIdx().setValue(dto.getControllerInput());
		}
	}

	public void setControllerOptions(List<ControllerDto> options) {
		view.getListBoxControllers().clear();
		view.getListBoxControllers().addItem(AdminLocale.getAdminConst().none());
		if (options == null || options.isEmpty()) {
			return;
		}
		for (final ControllerDto option : options) {
			view.getListBoxControllers().addItem(option.formatNameID());
		}
	}
}
