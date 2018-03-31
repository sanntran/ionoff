package net.ionoff.center.client.user;

import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.InlineLabel;
import com.google.gwt.user.client.ui.Label;

import net.ionoff.center.client.locale.AdminLocale;

public class UserRolesView extends FlowPanel implements UserRolesPresenter.Display {
	
	private final FlowPanel contentWrapper;
	
	public UserRolesView() {
		setStyleName("userRoles");
		
		Label lblRole = new InlineLabel(AdminLocale.getAdminConst().userRole());
		lblRole.setStyleName("lbl");
		lblRole.setHeight("25px");
		add(lblRole);
		
		contentWrapper = new FlowPanel();
		add(contentWrapper);
	}

	@Override
	public FlowPanel asPanel() {
		return this;
	}

	@Override
	public FlowPanel getContentPanel() {
		return contentWrapper;
	}
}
