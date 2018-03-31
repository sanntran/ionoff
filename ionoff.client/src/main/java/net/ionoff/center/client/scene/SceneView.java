package net.ionoff.center.client.scene;

import com.google.gwt.core.client.GWT;
import com.google.gwt.uibinder.client.UiBinder;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiTemplate;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.HTMLPanel;
import com.google.gwt.user.client.ui.Widget;

import gwt.material.design.client.ui.MaterialIcon;
import gwt.material.design.client.ui.MaterialLabel;

public class SceneView extends Composite implements ScenePresenter.Display {
	
	@UiTemplate("SceneView.ui.xml")
	interface SceneViewUiBinder extends UiBinder<Widget, SceneView> {
	}

	private static SceneViewUiBinder uiBinder = GWT.create(SceneViewUiBinder.class);
	
	@UiField
	HTMLPanel root;
	@UiField 
	MaterialIcon icon;
	@UiField 
	MaterialLabel lblName;
	@UiField 
	MaterialLabel lblTime;
	@UiField 
	MaterialIcon btnPlay;
	
	public SceneView() {
		uiBinder.createAndBindUi(this);
	}

	@Override
	public HTMLPanel asPanel() {
		return root;
	}

	@Override
	public MaterialIcon getIcon() {
		return icon;
	}

	@Override
	public MaterialLabel getLblTime() {
		return lblTime;
	}
	
	@Override
	public MaterialLabel getLblName() {
		return lblName;
	}
	
	public MaterialIcon getBtnPlay() {
		return btnPlay;
	}
}
