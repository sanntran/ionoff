package net.ionoff.center.client.relaydriver;


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
import net.ionoff.center.shared.dto.RelayDriverDto;
import net.ionoff.center.shared.entity.RelayDriverModel;

public class RelayDriverEditPresenter extends AbstractEditPresenter<RelayDriverDto> {

	public interface Display extends IEditView<RelayDriverDto> {
		MaterialListBox getListBoxModels();
		MaterialTextBox getTextBoxKey();
		MaterialTextBox getTextBoxIp();
		MaterialIntegerBox getIntBoxPort();
	}
	protected IRpcServiceProvider rpcProvider;
	
	private final Display view;
	private RelayDriverDto entityDto;
	private RelayDriverTablePresenter relayDriverManager;

	public RelayDriverEditPresenter(IRpcServiceProvider rpcProvider, 
			HandlerManager eventBus, Display view, RelayDriverTablePresenter relayDriverManager) {
		super(rpcProvider, eventBus, view);
		this.rpcProvider = rpcProvider;
		this.view = view;
		this.relayDriverManager = relayDriverManager;
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
				relayDriverManager.hideEditForm();
			}
		});
		view.getBtnCancel().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				relayDriverManager.hideEditForm();
			}
		});
		view.getListBoxModels().addValueChangeHandler(new ValueChangeHandler<String>() {
			@Override
			public void onValueChange(ValueChangeEvent<String> event) {
				if (entityDto != null && !entityDto.izNew()) {
					eventBus.fireEvent(new ShowMessageEvent(AdminLocale.getAdminMessages().unsupportChangingRelayDriverModel(), 
							ShowMessageEvent.ERROR));
					view.getListBoxModels().setSelectedValue(entityDto.getModel().toString());
					return;
				}
				String model = view.getListBoxModels().getSelectedValue();
				if (RelayDriverModel.IONOFF_E3.toString().equals(model) ||
						RelayDriverModel.IONOFF_E4.toString().equals(model) ||
						RelayDriverModel.IONOFF_P4.toString().equals(model) ||
						RelayDriverModel.IONOFF_P8.toString().equals(model)
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
		
		if (RelayDriverModel.IONOFF_E3.toString().equals(model)
				|| RelayDriverModel.IONOFF_E4.toString().equals(model) 
				|| RelayDriverModel.IONOFF_P4.toString().equals(model) 
				|| RelayDriverModel.IONOFF_P8.toString().equals(model)) {
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
			entityDto.setModel(RelayDriverModel.fromString(model));
		}
		
		rpcProvider.getRelayDriverService().save(entityDto.getId(), entityDto, 
				new MethodCallback<RelayDriverDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, RelayDriverDto result) {
				relayDriverManager.onSavedSucess(result);
			}
		});
	}

	@Override
	protected String getClazz() {
		return RelayDriverDto.class.getSimpleName();
	}

	@Override
	protected EntityService<RelayDriverDto> getRpcService() {
		return rpcProvider.getRelayDriverService();
	}

	public void setEntityDto(RelayDriverDto dto) {
		entityDto = dto;
		updateView(dto);
	}

	private void updateView(RelayDriverDto dto) {
		view.getLblId().setText("#" + dto.getId());
		view.getLblName().setText(dto.getName());
		view.getTextBoxName().setText(dto.getName());
		view.getTextBoxIp().setText(dto.getIp());
		view.getIntBoxPort().setValue(entityDto.getPort());
		if (RelayDriverModel.IONOFF_E3.equals(dto.getModel())  || 
				RelayDriverModel.IONOFF_E4.equals(dto.getModel()) ||
				RelayDriverModel.IONOFF_P4.equals(dto.getModel()) ||
				RelayDriverModel.IONOFF_P8.equals(dto.getModel())
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
		if (dto.izNew()) {
			view.getListBoxModels().setEnabled(true);
		}
		else {
			view.getListBoxModels().setEnabled(false);
		}
	}
}
