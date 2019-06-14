package net.ionoff.center.client.zone;

import com.google.gwt.core.client.GWT;
import com.google.gwt.uibinder.client.UiBinder;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiTemplate;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.HTMLPanel;
import com.google.gwt.user.client.ui.Widget;

import gwt.material.design.client.ui.MaterialIcon;
import gwt.material.design.client.ui.MaterialLabel;
import gwt.material.design.client.ui.MaterialLink;
import gwt.material.design.client.ui.MaterialPanel;

public class ZoneView extends Composite implements ZonePresenter.Display {
	
	@UiTemplate("ZoneView.ui.xml")
	interface ZoneViewUiBinder extends UiBinder<Widget, ZoneView> {
	}

	private static ZoneViewUiBinder uiBinder = GWT.create(ZoneViewUiBinder.class);
	
	@UiField
	HTMLPanel root;
	@UiField 
	MaterialIcon icon;
	@UiField 
	MaterialLabel lblName;
	@UiField 
	MaterialLabel lblArea;
	@UiField
	MaterialLabel lblDevices;
	@UiField
	MaterialPanel zoneCard;
	
	public ZoneView() {
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
	public MaterialLabel getLblDevices() {
		return lblDevices;
	}
	
	@Override
	public MaterialLabel getLblArea() {
		return lblArea;
	}

	@Override
	public MaterialLabel getLblName() {
		return lblName;
	}

	@Override
	public MaterialPanel getZoneCard() {
		return zoneCard;
	}
}
