package net.ionoff.center.client.relay;

import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.HasWidgets;

import gwt.material.design.client.ui.MaterialButton;
import gwt.material.design.client.ui.MaterialCollapsible;
import gwt.material.design.client.ui.MaterialIcon;
import gwt.material.design.client.ui.MaterialLabel;
import net.ionoff.center.client.base.AbstractPresenter;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.AppToken;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.ControllerDto;
import net.ionoff.center.shared.dto.RelayDto;
import net.ionoff.center.shared.dto.RelayGroupDto;

public class RelayGroupPresenter extends AbstractPresenter {
	
	public interface Display  {
		MaterialCollapsible asPanel();
		MaterialLabel getLblName();
		MaterialIcon getBtnDelete();
		FlowPanel getRelayListPanel();		
		MaterialButton getBtnAddRelay();
		RelaySelectionView getRelaySelectionView();
	}

	private final Display display;
	private RelayDto relayDto;
	private RelayGroupDto relayGroup;
	private final IRpcServiceProvider rpcProvider;
	
	public RelayGroupPresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus,
			RelayDto relay, RelayGroupDto relayGroup, Display display) {
		super(eventBus);
		this.display = display;
		this.relayDto = relay;
		this.relayGroup = relayGroup;
		this.rpcProvider= rpcProvider;
	}
	
	@Override
	public void go() {
		bind();
	}

	private void bind() {
		display.getLblName().setText(AdminLocale.getAdminConst().relayGroup() + " " + relayGroup.getId());
		display.getRelaySelectionView().setRelaySelectionHandler(new RelaySelectionHandler(this));
		display.getBtnAddRelay().addClickHandler((event) -> showRelaySelection());
		
		showRelayViews();
	}

	private void showRelayViews() {
		display.getRelayListPanel().clear();
		for (RelayDto relay : relayGroup.getRelays()) {
			RelayItemView relayView = new RelayItemView(relay);
			display.getRelayListPanel().add(relayView);
			relayView.getBtnRemove().addClickHandler((event) -> removeRelayFromGroup(relay));
			relayView.getBtnLeader().addClickHandler((event) -> setRelayAsGroupLeader(relay, relayView));
		}
	}

	@Override
	public void show(HasWidgets container) {
		container.clear();
		container.add(display.asPanel());
	}
	
	public void addRelayToGroup(RelayDto relay) {
		rpcProvider.getRelayService().addToRelayGroup(relayGroup.getId(), relay, new MethodCallback<RelayGroupDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, RelayGroupDto result) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				relayGroup.setRelays(result.getRelays());
				showRelayViews();
			}
		});
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
		rpcProvider.getRelayService().updateLeader(relayGroup.getId(), relay.getId(), relay.getIsLeader(), 
				new MethodCallback<MessageDto>() {
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
			public void onSuccess(Method method, MessageDto result) {
				// does nothing
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
			}
		});
	}

	private void removeRelayFromGroup(RelayDto relay) {
		rpcProvider.getRelayService().removeFromRelayGroup(relayGroup.getId(), relay.getId()
				, new MethodCallback<RelayGroupDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, RelayGroupDto result) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				relayGroup.setRelays(result.getRelays());
				showRelayViews();
			}
		});
	}
	

	private void showRelaySelection() {
		if (relayDto == null) {
			return;
		}
		rpcProvider.getControllerService().findByProjectId(AppToken.getProjectIdLong(),
				new MethodCallback<List<ControllerDto>>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, List<ControllerDto> result) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				display.getRelaySelectionView().setControllerOptions(result);
			}
		});
		
		display.getRelaySelectionView().setVisibility(true);
	}
}
