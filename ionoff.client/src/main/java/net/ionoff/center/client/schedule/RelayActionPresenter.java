package net.ionoff.center.client.schedule;

import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.HasWidgets;

import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.RelayDto;
import net.ionoff.center.shared.dto.ScheduleRelayActionDto;

public abstract class RelayActionPresenter<T extends BaseDto> extends SceneActionPresenter {
	
	protected final IRpcServiceProvider rpcProvider;
	private final IRelayActionView display;

	public RelayActionPresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus,
			IRelayActionView view) {
		super(eventBus);
		this.rpcProvider= rpcProvider;
		this.display = view;
	}
	
	public void bind() {
	}
	
	protected String getSelectedAction() {
		String action = ScheduleRelayActionDto.NONE;
		String relayType = getTargetRelayType();
		if (RelayDto.BUTTON.equals(relayType)) {
			if (display.getListBoxActions().getSelectedIndex() == 1) {
				action = ScheduleRelayActionDto.CLOSE_OPEN;
			}
		}
		else if (RelayDto.SWITCH.equals(relayType)) {
			if (display.getListBoxActions().getSelectedIndex() == 1) {
				action = ScheduleRelayActionDto.OPEN;
			}
			else if (display.getListBoxActions().getSelectedIndex() == 2) {
				action = ScheduleRelayActionDto.CLOSE;
			}
		}
		return action;
	}
	
	protected abstract void setTarget(T target);
	
	protected abstract String getTargetClazz();
	protected abstract Long getTargetId();
	protected abstract Long getTargetRelayId();
	protected abstract String getTargetRelayName();
	protected abstract String getTargetAction();
	protected abstract String getTargetRelayType();
	
	protected abstract void setTargetAction(String action);
	
	protected void resetLblRelayNameId() {
		display.getLblRelayNameId().setText(BaseDto.formatNameID(getTargetRelayName(), getTargetRelayId()));
	}
	
	protected void resetListBoxActions() {
		if (RelayDto.SWITCH.equals(getTargetRelayType())) {
			display.getListBoxActions().addItem(AdminLocale.getAdminConst().none());
			display.getListBoxActions().addItem(AdminLocale.getAdminConst().open());
			display.getListBoxActions().addItem(AdminLocale.getAdminConst().close());
		}
		else if (RelayDto.BUTTON.equals(getTargetRelayType())) {
			display.getListBoxActions().addItem(AdminLocale.getAdminConst().none());
			display.getListBoxActions().addItem(AdminLocale.getAdminConst().closeOpen());
		}
		
		if (ScheduleRelayActionDto.NONE.equals(getTargetAction())) {
			display.getListBoxActions().setSelectedIndex(0);
		}
		else if (ScheduleRelayActionDto.OPEN.equals(getTargetAction())) {
			display.getListBoxActions().setSelectedIndex(1);
		}
		else if (ScheduleRelayActionDto.CLOSE.equals(getTargetAction())) {
			display.getListBoxActions().setSelectedIndex(2);
		}
		else if (ScheduleRelayActionDto.CLOSE_OPEN.equals(getTargetAction())) {
			display.getListBoxActions().setSelectedIndex(1);
		}
	}
	
	abstract protected void save(String action);
	
	@Override
	public void go() {
		bind();
	}

	@Override
	public void show(HasWidgets container) {
		container.add(display.asWidget());
	}
}
