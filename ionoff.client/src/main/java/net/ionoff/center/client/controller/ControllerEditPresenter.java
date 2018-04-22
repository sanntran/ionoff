package net.ionoff.center.client.controller;


import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.logical.shared.ValueChangeEvent;
import com.google.gwt.event.logical.shared.ValueChangeHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.HasWidgets;

import gwt.material.design.client.ui.MaterialIntegerBox;
import gwt.material.design.client.ui.MaterialListBox;
import gwt.material.design.client.ui.MaterialTextBox;
import net.ionoff.center.client.base.AbstractEditPresenter;
import net.ionoff.center.client.base.IEditView;
import net.ionoff.center.client.event.ShowMessageEvent;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.EntityService;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.ControllerDto;
import net.ionoff.center.shared.entity.ControllerModel;

public class ControllerEditPresenter extends AbstractEditPresenter<ControllerDto> {

	public interface Display extends IEditView<ControllerDto> {
		MaterialListBox getListBoxModels();
		MaterialTextBox getTextBoxKey();
		MaterialTextBox getTextBoxIp();
		MaterialIntegerBox getIntBoxPort();
	}
	protected IRpcServiceProvider rpcProvider;
	
	private final Display view;
	private ControllerDto entityDto;
	private ControllerTablePresenter controllerManager;

	public ControllerEditPresenter(IRpcServiceProvider rpcProvider, 
			HandlerManager eventBus, Display view, ControllerTablePresenter controllerManager) {
		super(rpcProvider, eventBus, view);
		this.rpcProvider = rpcProvider;
		this.view = view;
		this.controllerManager = controllerManager;
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
				controllerManager.hideEditForm();
			}
		});
		view.getBtnCancel().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				controllerManager.hideEditForm();
			}
		});
		view.getListBoxModels().addValueChangeHandler(new ValueChangeHandler<String>() {
			@Override
			public void onValueChange(ValueChangeEvent<String> event) {
				if (entityDto != null && !entityDto.isNew()) {
					eventBus.fireEvent(new ShowMessageEvent(AdminLocale.getAdminMessages().unsupportChangingControllerModel(), 
							ShowMessageEvent.ERROR));
					view.getListBoxModels().setSelectedValue(entityDto.getModel().toString());
					return;
				}
				String model = view.getListBoxModels().getSelectedValue();
				
				
				if (ControllerModel.IONOFF_E4.toString().equals(model) ||
						ControllerModel.IONOFF_P4.toString().equals(model) ||
						ControllerModel.IONOFF_P8.toString().equals(model)
						) { 
					view.getTextBoxIp().setReadOnly(true);
					view.getIntBoxPort().setVisible(false);
					view.getTextBoxKey().setVisible(true);
				}
				else {
					view.getIntBoxPort().setVisible(true);
					view.getTextBoxIp().setReadOnly(false);
					view.getTextBoxKey().setVisible(false);
				}
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
		String newIp = view.getTextBoxIp().getValue();
		String newKey = view.getTextBoxKey().getValue();
		String model = view.getListBoxModels().getSelectedValue();
		
		if (ControllerModel.IONOFF_E4.toString().equals(model) 
				|| ControllerModel.IONOFF_P4.toString().equals(model) 
				|| ControllerModel.IONOFF_P8.toString().equals(model)) {
			if (!validateInputStringValue(AdminLocale.getAdminConst().key(), newKey)) {
				return;
			}
		}
		else {
			if (!validateInputStringValue(AdminLocale.getAdminConst().ip(), newIp)) {
				return;
			}
		}
		
		entityDto.setName(newName);
		entityDto.setKey(newKey);
		entityDto.setIp(newIp);
		entityDto.setPort(view.getIntBoxPort().getValue());
		
		if (entityDto.getId() == BaseDto.DEFAULT_ID) {
			entityDto.setModel(ControllerModel.fromString(model));
		}
		
		rpcProvider.getControllerService().save(entityDto.getId(), entityDto, 
				new MethodCallback<ControllerDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, ControllerDto result) {
				controllerManager.onSavedSucess(result);
			}
		});
	}

	@Override
	protected String getClazz() {
		return ControllerDto.class.getSimpleName();
	}

	@Override
	protected EntityService<ControllerDto> getRpcService() {
		return rpcProvider.getControllerService();
	}

	public void setEntityDto(ControllerDto dto) {
		entityDto = dto;
		updateView(dto);
	}

	private void updateView(ControllerDto dto) {
		view.getLblId().setText("#" + dto.getId());
		view.getLblName().setText(dto.getName());
		view.getTextBoxName().setText(dto.getName());
		view.getTextBoxIp().setText(dto.getIp());
		view.getIntBoxPort().setValue(entityDto.getPort());
		if (ControllerModel.IONOFF_E4.equals(dto.getModel()) ||
				ControllerModel.IONOFF_P4.equals(dto.getModel()) ||
				ControllerModel.IONOFF_P8.equals(dto.getModel())
				) { 
			view.getTextBoxIp().setReadOnly(true);
			view.getIntBoxPort().setVisible(false);
			view.getTextBoxKey().setVisible(true);
		}
		else {
			view.getIntBoxPort().setVisible(true);
			view.getTextBoxIp().setReadOnly(false);
			view.getTextBoxKey().setVisible(false);
		}
		view.getTextBoxKey().setText(dto.getKey());
		view.getListBoxModels().setSelectedValue(dto.getModel().toString());
		if (dto.isNew()) {
			view.getListBoxModels().setEnabled(true);
		}
		else {
			view.getListBoxModels().setEnabled(false);
		}
	}
}
