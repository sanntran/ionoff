package net.ionoff.center.client.zone;

import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.cellview.client.Column;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.Widget;
import com.google.gwt.view.client.AsyncDataProvider;
import com.google.gwt.view.client.HasData;
import net.ionoff.center.client.base.AbstractPresenter;
import net.ionoff.center.client.base.AbstractTablePresenter;
import net.ionoff.center.client.base.ITableView;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.EntityService;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.AreaDto;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.ZoneDto;
import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import java.util.ArrayList;
import java.util.List;

public class ZoneGridPresenter extends AbstractPresenter {

	public interface Display {
		void setPresenter(ZoneGridPresenter presenter);
		Widget asWidget();
	}

	private final Display view;
	private final IRpcServiceProvider rpcProvider;

	public ZoneGridPresenter(IRpcServiceProvider rpcProvider,
							 HandlerManager eventBus,
                             Display view) {
		super(eventBus);
		this.rpcProvider = rpcProvider;
		this.view = view;
	}

	@Override
	public void go() {
		view.setPresenter(this);
	}

	@Override
	public void show(HasWidgets container) {
		container.clear();
		container.add(view.asWidget());
	}

}
