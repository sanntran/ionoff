package net.ionoff.center.client.device;

import com.google.gwt.core.client.GWT;
import com.google.gwt.uibinder.client.UiBinder;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiTemplate;
import com.google.gwt.user.client.ui.FocusPanel;
import com.google.gwt.user.client.ui.HTMLPanel;
import com.google.gwt.user.client.ui.Widget;

import gwt.material.design.client.ui.MaterialIcon;
import gwt.material.design.client.ui.MaterialLabel;
import gwt.material.design.client.ui.MaterialPanel;

public class PlayerSliceView extends FocusPanel implements IDeviceView {

	@UiTemplate("PlayerSliceView.ui.xml")
	interface PlayerSliceViewUiBinder extends UiBinder<Widget, PlayerSliceView> {
	}

	private static PlayerSliceViewUiBinder uiBinder = GWT.create(PlayerSliceViewUiBinder.class);

	@UiField
	HTMLPanel root;
	@UiField
	MaterialPanel playerCard;
	@UiField 
	MaterialIcon btnIcon;
	@UiField 
	MaterialLabel lblName;
	@UiField 
	MaterialLabel lblZone;
	@UiField 
	MaterialLabel lblTime;
	@UiField 
	MaterialIcon btnStop;
	@UiField 
	MaterialIcon btnNext;
	@UiField 
	MaterialLabel lblPlayed;

	public PlayerSliceView() {
		uiBinder.createAndBindUi(this);
	}

	@Override
	public HTMLPanel asPanel() {
		return root;
	}
	
	@Override
	public MaterialIcon getBtnIcon() {
		return btnIcon;
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
	public MaterialLabel getLblZone() {
		return lblZone;
	}

	public MaterialPanel getPlayerCard() {
		return playerCard;
	}
	
	public MaterialIcon getBtnStop() {
		return btnStop;
	}
	
	public MaterialIcon getBtnNext() {
		return btnNext;
	}
	
	public MaterialLabel getLblPlayed() {
		return lblPlayed;
	}
}
