package net.ionoff.center.client.mode;

import com.google.gwt.core.client.GWT;
import com.google.gwt.uibinder.client.UiBinder;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiTemplate;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.HTMLPanel;
import com.google.gwt.user.client.ui.Widget;

import gwt.material.design.client.ui.MaterialIcon;
import gwt.material.design.client.ui.MaterialLabel;
import gwt.material.design.client.ui.MaterialSwitch;

public class ModeView extends Composite implements ModePresenter.Display {
	
	@UiTemplate("ModeView.ui.xml")
	interface ModeViewUiBinder extends UiBinder<Widget, ModeView> {
	}

	private static ModeViewUiBinder uiBinder = GWT.create(ModeViewUiBinder.class);
	
	@UiField
	HTMLPanel root;
	@UiField 
	MaterialIcon icon;
	@UiField 
	MaterialLabel lblName;
	@UiField 
	MaterialLabel lblProj;
	@UiField 
	MaterialLabel lblTime;
	@UiField 
	MaterialSwitch btnSwitch;
	
	public ModeView() {
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

	@Override
	public MaterialLabel getLblProj() {
		return lblProj;
	}
	
	@Override
	public MaterialSwitch getBtnSwitch() {
		return btnSwitch;
	}
}
