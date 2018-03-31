package net.ionoff.center.client.schedule;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.shared.HandlerManager;

import net.ionoff.center.client.event.ShowMessageEvent;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.SchedulePlayerActionDto;

public class SchedulePlayerActionPresenter extends PlayerActionPresenter<SchedulePlayerActionDto> {
	
	private SchedulePlayerActionDto scheduleAction;
	
	public SchedulePlayerActionPresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus,
			IPlayerActionView view) {
		super(rpcProvider, eventBus, view);
	}

	@Override
	protected void setTarget(SchedulePlayerActionDto target) {
		scheduleAction = target;
		updateDisplay();
	}

	@Override
	protected String getTargetClazz() {
		return SchedulePlayerActionDto.class + "";
	}

	@Override
	protected Long getPlayerId() {
		return scheduleAction.getPlayerId();
	}

	@Override
	protected Long getTargetId() {
		return scheduleAction.getId();
	}

	@Override
	protected String getTargetAction() {
		return scheduleAction.getAction();
	}

	@Override
	protected String getTargetAlbumType() {
		return scheduleAction.getAlbumType();
	}

	@Override
	protected void setTargetVolume(String volume) {
		scheduleAction.setVolume(volume);
	}

	@Override
	protected void setTargetAction(String action) {
		scheduleAction.setAction(action);
	}

	@Override
	protected void setTargetAlbumType(String albumType) {
		scheduleAction.setAlbumType(albumType);
	}

	@Override
	protected String getTargetVolume() {
		return scheduleAction.getVolume();
	}

	@Override
	protected String getTargetAlbum() {
		return scheduleAction.getAlbum();
	}

	@Override
	protected String getPlayerName() {
		return scheduleAction.getPlayerName();
	}

	@Override
	protected void setTargetAlbum(String album) {
		scheduleAction.setAlbum(album);
	}

	@Override
	public void save() {
		scheduleAction.setAction(getTargetAction());
		scheduleAction.setVolume(display.getIntBoxVolume().getText());
		scheduleAction.setAlbum(display.getTextBoxAlbum().getText());
		scheduleAction.setAlbumType(getTargetAlbumType());
		rpcProvider.getSchedulePlayerActionService().save(getTargetId(), 
				scheduleAction, new MethodCallback<SchedulePlayerActionDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, SchedulePlayerActionDto result) {
				eventBus.fireEvent(new ShowMessageEvent(AdminLocale.getAdminMessages().updateSuccess(),
						ShowMessageEvent.SUCCESS));
			}
		});
	}

	@Override
	public SchedulePlayerActionDto getTarget() {
		return scheduleAction;
	}
}
