package net.ionoff.center.client.dashboard;

import com.google.gwt.core.client.GWT;
import com.google.gwt.uibinder.client.UiBinder;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiTemplate;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.HTMLPanel;
import com.google.gwt.user.client.ui.Panel;
import com.google.gwt.user.client.ui.Widget;

import gwt.material.design.client.ui.MaterialCard;
import gwt.material.design.client.ui.MaterialCardTitle;
import gwt.material.design.client.ui.MaterialIcon;
import gwt.material.design.client.ui.MaterialLabel;
import gwt.material.design.client.ui.MaterialLink;
import gwt.material.design.client.ui.MaterialRow;
import net.ionoff.center.client.locale.ProjectLocale;

public class DashboardView extends Composite implements DashboardPresenter.Display {

	@UiTemplate("DashboardView.ui.xml")
	interface DashboardViewUiBinder extends UiBinder<Widget, DashboardView> {
	}

	private static DashboardViewUiBinder uiBinder = GWT.create(DashboardViewUiBinder.class);

	@UiField
	HTMLPanel root;
	@UiField
	MaterialLink lblTitle;
	@UiField
	MaterialRow wrapper;

	public DashboardView() {
		uiBinder.createAndBindUi(this);
		lblTitle.setText(ProjectLocale.getProjectConst().dashboard());
	}

	@Override
	public HTMLPanel asPanel() {
		return root;
	}

	@Override
	public MaterialRow getWrapper() {
		return wrapper;
	}

	@Override
	public MaterialLink getLblTitle() {
		return lblTitle;
	}
	
}
