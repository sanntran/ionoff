package net.ionoff.center.client.device;

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

public class DeviceListView extends Composite implements DeviceListPresenter.Display {
	
	@UiTemplate("DeviceListView.ui.xml")
	interface DeviceListViewUiBinder extends UiBinder<Widget, DeviceListView> {
	}

	private static DeviceListViewUiBinder uiBinder = GWT.create(DeviceListViewUiBinder.class);
	
	@UiField
	HTMLPanel root;
	@UiField
	MaterialLink lblTitle;
	@UiField
	MaterialIcon iconSetting;
	@UiField
	MaterialRow wrapper;

	public DeviceListView() {
		uiBinder.createAndBindUi(this);

		lblTitle.setText(ProjectLocale.getProjectConst().device());

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
	public MaterialIcon getIconSetting() {
		return iconSetting;
	}

}
