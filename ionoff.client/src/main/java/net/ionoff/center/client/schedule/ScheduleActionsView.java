package net.ionoff.center.client.schedule;

import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.Label;

import net.ionoff.center.client.locale.AdminLocale;

public class ScheduleActionsView extends FlowPanel implements ScheduleActionsPresenter.Display {
	
	private Label lblName;
	private FlowPanel container;
	
	public ScheduleActionsView() {

		lblName = new Label(AdminLocale.getAdminConst().action());
		lblName.setStyleName("lbl");
		add(lblName);
		
		container = new FlowPanel();
		container.setStyleName("wrapper");
		add(container);
	}

	@Override
	public FlowPanel asPanel() {
		return this;
	}

	@Override
	public FlowPanel getContainer() {
		return container;
	}
}
