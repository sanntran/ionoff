package net.ionoff.center.client.relay;


import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.http.client.Response;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.HasWidgets;

import gwt.material.design.client.ui.MaterialButton;
import gwt.material.design.client.ui.MaterialCheckBox;
import gwt.material.design.client.ui.MaterialIntegerBox;
import gwt.material.design.client.ui.MaterialTextBox;
import net.ionoff.center.client.base.AbstractEditPresenter;
import net.ionoff.center.client.base.IEditView;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.event.ShowMessageEvent;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.EntityService;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.ui.DevicesSelectionPanel;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.AreaDto;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.RelayDto;
import net.ionoff.center.shared.dto.RelayGroupDto;

public class RelayEditPresenter extends AbstractEditPresenter<RelayDto> {

	public interface Display extends IEditView<RelayDto> {
		
		MaterialTextBox getTextBoxDriver();

		MaterialTextBox getTextBoxIndex();

		MaterialIntegerBox getIntBoxAutoRevert();
		
		MaterialCheckBox getCheckBoxLocked();

		DevicesSelectionPanel getDevicesSelectionPanel();
		
		FlowPanel getRelayGroupListPanel();	
		
		MaterialButton getBtnAddGroup();
	}
	
	protected IRpcServiceProvider rpcProvider;
	
	private final Display view;
	private RelayDto entityDto;
	private RelayTablePresenter relayManager;

	public RelayEditPresenter(IRpcServiceProvider rpcProvider, 
			HandlerManager eventBus, Display view, RelayTablePresenter relayManager) {
		super(rpcProvider, eventBus, view);
		this.rpcProvider = rpcProvider;
		this.view = view;
		this.relayManager = relayManager;
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
				relayManager.hideEditForm();
			}
		});
		
		view.getBtnCancel().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				relayManager.hideEditForm();
			}
		});
		view.getBtnAddGroup().addClickHandler(event -> createNewRelayGroup());
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
		entityDto.setIsLocked(view.getCheckBoxLocked().getValue());
		entityDto.setAutoRevert(view.getIntBoxAutoRevert().getValue());
		
		String selectedDeviceNameId = view.getDevicesSelectionPanel().getSelectedDevice();
		if (selectedDeviceNameId == null || selectedDeviceNameId.isEmpty() 
				|| AdminLocale.getAdminConst().none().equals(selectedDeviceNameId)) {
			entityDto.setDeviceId(null);
			entityDto.setDeviceName(null);
		}
		else {
			entityDto.setDeviceId(BaseDto.parseIdFromFormattedNameID(selectedDeviceNameId));
			entityDto.setDeviceName(BaseDto.parseNameFromFormattedNameID(selectedDeviceNameId));
		}
		
		rpcProvider.getRelayService().save(entityDto.getId(), entityDto, 
				new MethodCallback<RelayDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, RelayDto result) {
				relayManager.onSavedSucess(result);
			}
		});
	}

	@Override
	protected String getClazz() {
		return RelayDto.class.getSimpleName();
	}

	@Override
	protected EntityService<RelayDto> getRpcService() {
		return rpcProvider.getRelayService();
	}

	public void setEntityDto(RelayDto dto) {
		entityDto = dto;
		updateView(dto);
		view.getRelayGroupListPanel().clear();
		rpcLoadRelayGroups(dto);
	}

	private void createNewRelayGroup() {
		rpcProvider.getRelayService().createRelayGroups(entityDto.getId(), new MethodCallback<RelayGroupDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				final int statusCode = method.getResponse().getStatusCode();
				if (statusCode == Response.SC_NOT_FOUND) {
					// Ignore
					eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				}
				else {
					ClientUtil.handleRpcFailure(method, exception, eventBus);
				}
			}
			@Override
			public void onSuccess(Method method, RelayGroupDto result) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				showRelayGroup(result);
			}
		});
	}
	
	private void rpcLoadRelayGroups(RelayDto dto) {
		rpcProvider.getRelayService().getRelayGroups(dto.getId(), new MethodCallback<List<RelayGroupDto>>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				final int statusCode = method.getResponse().getStatusCode();
				if (statusCode == Response.SC_NOT_FOUND) {
					// Ignore
					eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				}
				else {
					ClientUtil.handleRpcFailure(method, exception, eventBus);
				}
			}
			@Override
			public void onSuccess(Method method, List<RelayGroupDto> result) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				showRelayGroupList(result);
			}
		});
	}

	private void showRelayGroupList(List<RelayGroupDto> relayGroups) {
		view.getRelayGroupListPanel().clear();
		for (RelayGroupDto relayGroup : relayGroups) {
			showRelayGroup(relayGroup);
		}
	}

	private void showRelayGroup(RelayGroupDto relayGroup) {
		RelayGroupView relayGroupView = new RelayGroupView();
		view.getRelayGroupListPanel().add(relayGroupView);
		relayGroupView.getBtnDelete().addClickHandler(event -> deleteRelayGroup(relayGroup, relayGroupView));
		RelayGroupPresenter relayGroupPresenter = new RelayGroupPresenter(
				rpcProvider, eventBus, entityDto, relayGroup, relayGroupView);
		relayGroupPresenter.go();
	}

	private void deleteRelayGroup(RelayGroupDto relayGroup, RelayGroupView relayGroupView) {
		rpcProvider.getRelayService().deleteGroupById(relayGroup.getId(), new MethodCallback<MessageDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, MessageDto result) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				eventBus.fireEvent(new ShowMessageEvent(AdminLocale.getAdminMessages().deleteSuccess(),
						ShowMessageEvent.SUCCESS));
				view.getRelayGroupListPanel().remove(relayGroupView);
			}
		});
	}

	private void updateView(RelayDto dto) {
		view.getLblId().setText("#" + dto.getId());
		view.getLblName().setText(dto.getName());
		view.getTextBoxName().setText(dto.getName());
		view.getCheckBoxLocked().setValue(entityDto.getIsLocked() == null ? false : entityDto.getIsLocked());
		view.getTextBoxDriver().setText(BaseDto.formatNameID(dto.getDriverName(), dto.getDriverId()));
		view.getTextBoxIndex().setText(dto.getIndex() + "");
		view.getIntBoxAutoRevert().setValue(dto.getAutoRevert());
		view.getDevicesSelectionPanel().setSelectedItem(dto.getDeviceId(), dto.getDeviceName());

		if (dto.izNew()) {
			view.getBtnAddGroup().setVisible(false);
		}
		else {
			view.getBtnAddGroup().setVisible(true);
		}
	}

	public void setDeviceOptions(List<AreaDto> areaDtos) {
		view.getDevicesSelectionPanel().setDeviceOptions(areaDtos);
	}
	
}
