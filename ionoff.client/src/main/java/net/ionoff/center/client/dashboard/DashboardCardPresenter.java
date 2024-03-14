package net.ionoff.center.client.dashboard;


import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.HasWidgets;
import gwt.material.design.client.constants.Color;
import net.ionoff.center.client.base.AbstractPresenter;
import net.ionoff.center.client.event.ChangeTokenEvent;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.AppToken;

public class DashboardCardPresenter extends AbstractPresenter {

	public interface Display {
		void setPresenter(DashboardCardPresenter presenter);
		void setTitleValue(String value);
		void setBackgroundColor(Color color);
	}

	private Display display;
	private IRpcServiceProvider rpcService;

	public DashboardCardPresenter(IRpcServiceProvider rpcService, HandlerManager eventBus, Display view) {
		super(eventBus);
		this.rpcService = rpcService;
		this.display = view;
		this.display.setPresenter(this);
	}

	public void onRootClicked() {
		String token = AppToken.newDeviceTableToken();
		eventBus.fireEvent(new ChangeTokenEvent(token));
	}

	@Override
	public void go() { }

	@Override
	public void show(HasWidgets container) { }
}
