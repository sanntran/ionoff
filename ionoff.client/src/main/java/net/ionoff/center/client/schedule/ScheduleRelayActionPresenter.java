package net.ionoff.center.client.schedule;


import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.shared.HandlerManager;

import net.ionoff.center.client.event.ShowMessageEvent;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.ScheduleRelayActionDto;

public class ScheduleRelayActionPresenter extends RelayActionPresenter<ScheduleRelayActionDto> {
	
	private ScheduleRelayActionDto scheduleAction;

	public ScheduleRelayActionPresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus,
			IRelayActionView view) {
		super(rpcProvider, eventBus, view);
	}

	@Override
	protected void setTarget(ScheduleRelayActionDto target) {
		scheduleAction = target;
		resetLblRelayNameId();
		resetListBoxActions();
	}

	@Override
	protected String getTargetClazz() {
		return ScheduleRelayActionDto.class + "";
	}

	@Override
	protected Long getTargetId() {
		return scheduleAction.getId();
	}

	@Override
	protected Long getTargetRelayId() {
		return scheduleAction.getRelayId();
	}

	@Override
	protected String getTargetRelayName() {
		return scheduleAction.getRelayName();
	}

	@Override
	protected String getTargetAction() {
		return scheduleAction.getAction();
	}

	@Override
	protected void setTargetAction(String action) {
		scheduleAction.setAction(action);
	}

	@Override
	protected void save(String action) {
		setTargetAction(action);
		rpcProvider.getScheduleRelayActionService().save(scheduleAction.getId(), scheduleAction, 
				new MethodCallback<ScheduleRelayActionDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, ScheduleRelayActionDto result) {
				eventBus.fireEvent(new ShowMessageEvent(AdminLocale.getAdminMessages().updateSuccess(),
						ShowMessageEvent.SUCCESS));
			}
		});
	}

	@Override
	public void save() {
		save(getSelectedAction());
	}
	
	@Override
	public ScheduleRelayActionDto getTarget() {
		return scheduleAction;
	}
}
