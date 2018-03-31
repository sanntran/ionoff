package net.ionoff.center.client.schedule;


import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.HasWidgets;

import gwt.material.design.client.ui.MaterialCheckBox;
import net.ionoff.center.client.common.AbstractEditPresenter;
import net.ionoff.center.client.common.DevicesSelectionPanel;
import net.ionoff.center.client.common.IEditView;
import net.ionoff.center.client.event.ShowMessageEvent;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.EntityService;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.AreaDto;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.ScheduleDto;

public class ScheduleEditPresenter extends AbstractEditPresenter<ScheduleDto> {

	public interface Display extends IEditView<ScheduleDto> {
		ScheduleTimeSettingPanel getScheduleTimeSettingPanel();
		DevicesSelectionPanel getDevicesSelectionPanel();
		ScheduleActionsView getScheduleActionsView();
		MaterialCheckBox getCheckBoxEnable();
	}
	protected IRpcServiceProvider rpcProvider;
	
	private final Display view;
	private ScheduleDto entityDto;
	private ScheduleTablePresenter scheduleManager;
	private ScheduleActionsPresenter scheduleActionsPresenter;

	public ScheduleEditPresenter(IRpcServiceProvider rpcProvider, 
			HandlerManager eventBus, Display view, ScheduleTablePresenter scheduleManager) {
		super(rpcProvider, eventBus, view);
		this.rpcProvider = rpcProvider;
		this.view = view;
		this.scheduleManager = scheduleManager;
		scheduleActionsPresenter = new ScheduleActionsPresenter(rpcProvider, eventBus, view.getScheduleActionsView());
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
				scheduleManager.hideEditForm();
			}
		});
		view.getBtnCancel().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				scheduleManager.hideEditForm();
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
		
		String selectedDeviceNameId = view.getDevicesSelectionPanel().getSelectedDevice();
		if (selectedDeviceNameId == null || selectedDeviceNameId.isEmpty()
				|| AdminLocale.getAdminConst().none().equals(selectedDeviceNameId)) {
			final String message = AdminLocale.getAdminMessages().invalidFieldValue(AdminLocale.getAdminConst().device());
			eventBus.fireEvent(new ShowMessageEvent(message, ShowMessageEvent.ERROR));
			return;
		}
		entityDto.setDeviceId(BaseDto.parseIdFromFormattedNameID(selectedDeviceNameId));
		entityDto.setDeviceName(BaseDto.parseNameFromFormattedNameID(selectedDeviceNameId));
		
		entityDto.setEnabled(view.getCheckBoxEnable().getValue());
		entityDto.setRepeat(view.getScheduleTimeSettingPanel().getSelectedRepeat());
		entityDto.setDay(view.getScheduleTimeSettingPanel().getSelectedDay());
		entityDto.setTime(view.getScheduleTimeSettingPanel().getSelectedTime());
		
		scheduleActionsPresenter.save();
		
		rpcProvider.getScheduleService().save(entityDto.getId(), entityDto, 
				new MethodCallback<ScheduleDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, ScheduleDto result) {
				scheduleManager.onSavedSucess(result);
			}
		});
	}

	@Override
	protected String getClazz() {
		return ScheduleDto.class.getSimpleName();
	}

	@Override
	protected EntityService<ScheduleDto> getRpcService() {
		return rpcProvider.getScheduleService();
	}

	public void setEntityDto(ScheduleDto dto) {
		entityDto = dto;
		updateView(dto);
	}

	private void updateView(ScheduleDto dto) {
		view.getLblId().setText("#" + dto.getId());
		view.getLblName().setText(dto.getName());
		view.getTextBoxName().setText(dto.getName());
		view.getCheckBoxEnable().setValue(dto.getEnabled());
		view.getScheduleTimeSettingPanel().setScheduleData(dto.getRepeat(), dto.getDay(), dto.getTime());
		view.getDevicesSelectionPanel().setSelectedItem(dto.getDeviceId(), dto.getDeviceName());
		view.getDevicesSelectionPanel().getBtnClearSelectedDevice().setVisible(false);
		if (entityDto.getId() == BaseDto.DEFAULT_ID) {
			view.getDevicesSelectionPanel().setEnable(true);
			scheduleActionsPresenter.hide();
		}
		else {
			view.getDevicesSelectionPanel().setEnable(false);
			scheduleActionsPresenter.show();
			scheduleActionsPresenter.setScheduleActions(dto.getActions());
			
		}
	}

	public void setDeviceOptions(List<AreaDto> areaDtos) {
		view.getDevicesSelectionPanel().setDeviceOptions(areaDtos);
	}
}
