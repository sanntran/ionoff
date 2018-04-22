package net.ionoff.center.client.mode;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.HTMLPanel;
import com.google.gwt.user.client.ui.HasWidgets;

import gwt.material.design.client.ui.MaterialIcon;
import gwt.material.design.client.ui.MaterialLabel;
import gwt.material.design.client.ui.MaterialSwitch;
import net.ionoff.center.client.base.AbstractPresenter;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.ModeDto;

public class ModePresenter extends AbstractPresenter {
	
	public interface Display {
		HTMLPanel asPanel();
		MaterialIcon getIcon();
		MaterialLabel getLblName();
		MaterialLabel getLblTime();
		MaterialSwitch getBtnSwitch();
	}
	
	private Display display;
	private final IRpcServiceProvider rpcService;
	private final ModeDto mode;
	private boolean locked; // lock when sending request of control 

	public ModePresenter(IRpcServiceProvider rpcService, HandlerManager eventBus, ModeDto scene, Display view) {
		super(eventBus);
		this.setLocked(false);
		this.rpcService = rpcService;
		this.mode = scene;
		this.display = view;
	}

	@Override
	public void go() {
		bind();
	}
	
	public void bind() {
		display.getLblName().setText(mode.getName());
		if (mode.getTime() != null) {
			display.getLblTime().setText(mode.getTime());
		}
		if (Boolean.TRUE.equals(mode.getIsActivated())) {
			display.getBtnSwitch().setValue(true);
		}
		else {
			display.getBtnSwitch().setValue(false);
		}
		display.getBtnSwitch().addClickHandler((e) -> activateMode());
	}

	private void activateMode() {
		if (Boolean.TRUE.equals(mode.getIsActivated())) {
			display.getBtnSwitch().setValue(true);
		}
		rpcService.getModeService().activateById(mode.getId(), new MethodCallback<MessageDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}

			@Override
			public void onSuccess(Method method, MessageDto response) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
			}
		});
	}

	@Override
	public void show(HasWidgets container) {
		container.add(display.asPanel());
	}

	protected IRpcServiceProvider getRpcProvider() {
		return rpcService;
	}

	public ModeDto getMode() {
		return mode;
	}
	
	public void updateMode(ModeDto modeDto) {
		display.asPanel().removeStyleName("activated");
		mode.setIsActivated(modeDto.getIsActivated());
		mode.setTime(modeDto.getTime());
		if (mode.getTime() != null) {
			display.getLblTime().setText(mode.getTime());
		}
		if (Boolean.TRUE.equals(mode.getIsActivated())) {
			display.getBtnSwitch().setValue(true);
			display.asPanel().addStyleName("activated");
		}
		else {
			display.getBtnSwitch().setValue(false);
		}
	}

	public boolean isLocked() {
		return locked;
	}

	public void setLocked(boolean lock) {
		locked = lock;
	}
}
