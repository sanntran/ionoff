package net.ionoff.center.client.mode;

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

public class ModeListView extends Composite implements ModeListPresenter.Display {
	
	@UiTemplate("ModeListView.ui.xml")
	interface ModeListViewUiBinder extends UiBinder<Widget, ModeListView> {
	}

	private static ModeListViewUiBinder uiBinder = GWT.create(ModeListViewUiBinder.class);
	
	@UiField
	HTMLPanel root;
	@UiField
	MaterialLink lblTitle;
	@UiField
	MaterialIcon iconSetting;
	@UiField
	MaterialRow wrapper;

	public ModeListView() {
		uiBinder.createAndBindUi(this);
		lblTitle.setText(ProjectLocale.getProjectConst().mode());
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
