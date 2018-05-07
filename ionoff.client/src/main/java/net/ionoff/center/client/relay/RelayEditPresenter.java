package net.ionoff.center.client.relay;


import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.http.client.Response;
import com.google.gwt.user.client.ui.HasWidgets;

import gwt.material.design.client.ui.MaterialListBox;
import gwt.material.design.client.ui.MaterialTextBox;
import net.ionoff.center.client.base.AbstractEditPresenter;
import net.ionoff.center.client.ui.DevicesSelectionPanel;
import net.ionoff.center.client.base.IEditView;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.EntityService;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.AppToken;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.AreaDto;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.RelayDriverDto;
import net.ionoff.center.shared.dto.RelayDto;
import net.ionoff.center.shared.dto.RelayGroupDto;

public class RelayEditPresenter extends AbstractEditPresenter<RelayDto> {

	public interface Display extends IEditView<RelayDto> {
		
		MaterialTextBox getTextBoxRelayDriver();

		MaterialTextBox getTextBoxIndex();

		MaterialListBox getListBoxTypes();

		DevicesSelectionPanel getDevicesSelectionPanel();
		
		RelayGroupView getRelayGroupView();
		
		RelaySelectionView getRelaySelectionView();
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
		
		view.getRelaySelectionView().setRelaySelectionHandler(new RelaySelectionHandler(this));
		
		view.getRelayGroupView().getBtnAdd().addClickHandler((event) -> showRelaySelection());
	}

	private void showRelaySelection() {
		if (entityDto == null) {
			return;
		}
		rpcProvider.getRelayDriverService().findByProjectId(AppToken.getProjectIdLong(), 
				new MethodCallback<List<RelayDriverDto>>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, List<RelayDriverDto> result) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				view.getRelaySelectionView().setRelayDriverOptions(result);
			}
		});
		
		view.getRelaySelectionView().setVisibility(true);
	}
	

	public void addRelayToGroup(RelayDto relay) {
		rpcProvider.getRelayService().addToRelayGroup(entityDto.getId(), relay, new MethodCallback<RelayGroupDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, RelayGroupDto result) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				showRelayGroup(result);
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
		
		int typeIndex = view.getListBoxTypes().getSelectedIndex();
		if (typeIndex == 0) {
			entityDto.setType(RelayDto.SWITCH);
		}
		else if (typeIndex == 1) {
			entityDto.setType(RelayDto.BUTTON);
		}
		
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
		rpcLoadRelayGroup(dto);
		view.getRelaySelectionView().setVisibility(false);
	}

	private void rpcLoadRelayGroup(RelayDto dto) {
		rpcProvider.getRelayService().getRelayGroup(dto.getId(), new MethodCallback<RelayGroupDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				view.getRelayGroupView().clear();
				view.getRelayGroupView().add(view.getRelayGroupView().getBtnAdd());
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

	private void showRelayGroup(RelayGroupDto relayGroup) {
		view.getRelayGroupView().clear();
		for (RelayDto relay : relayGroup.getRelays()) {
			RelayItemView relayView = new RelayItemView(relay);
			view.getRelayGroupView().add(relayView);
			relayView.getBtnRemove().addClickHandler((event) -> removeRelayFromGroup(relay));
			relayView.getBtnLeader().addClickHandler((event) -> setRelayAsGroupLeader(relay, relayView));
		}
		view.getRelayGroupView().add(view.getRelayGroupView().getBtnAdd());
	}

	private void setRelayAsGroupLeader(RelayDto relay, RelayItemView relayView) {
		relayView.getBtnLeader().removeStyleName("none");
		if (Boolean.TRUE.equals(relay.getIsLeader())) {
			relay.setIsLeader(false);
			relayView.getBtnLeader().addStyleName("none");
		}
		else {
			relay.setIsLeader(true);
		}
		rpcProvider.getRelayService().updateLeader(relay.getId(), relay.getIsLeader(), new MethodCallback<RelayDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
				relayView.getBtnLeader().removeStyleName("none");
				if (Boolean.TRUE.equals(relay.getIsLeader())) {
					relay.setIsLeader(false);
					relayView.getBtnLeader().addStyleName("none");
				}
				else {
					relay.setIsLeader(true);
				}
			}
			@Override
			public void onSuccess(Method method, RelayDto result) {
				// does nothing
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
			}
		});
	}

	private void removeRelayFromGroup(RelayDto relay) {
		rpcProvider.getRelayService().removeFromRelayGroup(entityDto.getId(), relay.getId()
				, new MethodCallback<RelayGroupDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, RelayGroupDto result) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				showRelayGroup(result);
			}
		});
	}

	private void updateView(RelayDto dto) {
		view.getLblId().setText("#" + dto.getId());
		view.getLblName().setText(dto.getName());
		view.getTextBoxName().setText(dto.getName());
		view.getTextBoxRelayDriver().setText(BaseDto.formatNameID(dto.getRelayDriverName(), dto.getRelayDriverId()));
		view.getTextBoxIndex().setText(dto.getIndex() + "");
		if (RelayDto.SWITCH.equals(entityDto.getType())) {
			view.getListBoxTypes().setSelectedIndex(0);
		}
		else {
			view.getListBoxTypes().setSelectedIndex(1);
		}
		view.getDevicesSelectionPanel().setSelectedItem(dto.getDeviceId(), dto.getDeviceName());
	}

	public void setDeviceOptions(List<AreaDto> areaDtos) {
		view.getDevicesSelectionPanel().setDeviceOptions(areaDtos);
	}
	
}
