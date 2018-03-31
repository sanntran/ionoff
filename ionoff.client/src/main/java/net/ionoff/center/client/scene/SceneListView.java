package net.ionoff.center.client.scene;

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

public class SceneListView extends Composite implements SceneListPresenter.Display {
	
	@UiTemplate("SceneListView.ui.xml")
	interface SceneListViewUiBinder extends UiBinder<Widget, SceneListView> {
	}

	private static SceneListViewUiBinder uiBinder = GWT.create(SceneListViewUiBinder.class);
	
	@UiField
	HTMLPanel root;
	@UiField
	MaterialLink lblTitle;
	@UiField
	MaterialIcon iconSetting;
	@UiField
	MaterialRow wrapper;

	public SceneListView() {
		uiBinder.createAndBindUi(this);
		lblTitle.setText(ProjectLocale.getProjectConst().scene());
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
