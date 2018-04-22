package net.ionoff.center.client.device;

import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.HasWidgets;

import net.ionoff.center.client.base.AbstractPresenter;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.shared.dto.DeviceDto;
import net.ionoff.center.shared.dto.StatusDto;

public abstract class DevicePresenter extends AbstractPresenter {

	private final IRpcServiceProvider rpcService;
	private final DeviceDto device;
	private boolean locked; // lock when sending request of control 

	public DevicePresenter(IRpcServiceProvider rpcService, HandlerManager eventBus, DeviceDto device) {
		super(eventBus);
		this.setLocked(false);
		this.rpcService = rpcService;
		this.device = device;
	}

	@Override
	public void go() {
		bind();
	}
	
	protected abstract void bind();
	protected abstract IDeviceView getDeviceView();
	public abstract void updateStatus(StatusDto status);

	@Override
	public void show(HasWidgets container) {
		container.add(getDeviceView().asPanel());
	}

	protected IRpcServiceProvider getRpcProvider() {
		return rpcService;
	}

	public DeviceDto getDevice() {
		return device;
	}

	public boolean isLocked() {
		return locked;
	}

	public void setLocked(boolean lock) {
		locked = lock;
	}
}
