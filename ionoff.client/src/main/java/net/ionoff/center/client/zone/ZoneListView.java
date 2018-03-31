package net.ionoff.center.client.zone;

import com.google.gwt.core.client.GWT;
import com.google.gwt.uibinder.client.UiBinder;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiTemplate;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.HTMLPanel;
import com.google.gwt.user.client.ui.Widget;

import gwt.material.design.client.ui.MaterialIcon;
import gwt.material.design.client.ui.MaterialLink;
import gwt.material.design.client.ui.MaterialRow;
import net.ionoff.center.client.locale.ProjectLocale;

public class ZoneListView extends Composite implements ZoneListPresenter.Display {
	
	@UiTemplate("ZoneListView.ui.xml")
	interface ZoneListViewUiBinder extends UiBinder<Widget, ZoneListView> {
	}

	private static ZoneListViewUiBinder uiBinder = GWT.create(ZoneListViewUiBinder.class);
	
	@UiField
	HTMLPanel root;
	@UiField
	MaterialLink lblTitle;
	@UiField
	MaterialIcon iconSetting;
	@UiField
	MaterialRow wrapper;

	public ZoneListView() {
		uiBinder.createAndBindUi(this);
		lblTitle.setText(ProjectLocale.getProjectConst().zone());
	}

	@Override
	public MaterialIcon getIconSetting() {
		return iconSetting;
	}
	
	@Override
	public HTMLPanel asPanel() {
		return root;
	}

	@Override
	public MaterialRow getWrapper() {
		return wrapper;
	}
}
