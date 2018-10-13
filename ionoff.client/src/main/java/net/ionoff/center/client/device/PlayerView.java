package net.ionoff.center.client.device;

import com.google.gwt.core.client.GWT;
import com.google.gwt.uibinder.client.UiBinder;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiTemplate;
import com.google.gwt.user.client.ui.FocusPanel;
import com.google.gwt.user.client.ui.HTMLPanel;
import com.google.gwt.user.client.ui.Widget;

import gwt.material.design.client.ui.MaterialDropDown;
import gwt.material.design.client.ui.MaterialIcon;
import gwt.material.design.client.ui.MaterialImage;
import gwt.material.design.client.ui.MaterialLabel;
import gwt.material.design.client.ui.MaterialLink;
import gwt.material.design.client.ui.MaterialPanel;
import net.ionoff.center.client.locale.ProjectLocale;

public class PlayerView extends FocusPanel implements IDeviceView {

	@UiTemplate("PlayerView.ui.xml")
	interface PlayerViewUiBinder extends UiBinder<Widget, PlayerView> {
	}

	private static PlayerViewUiBinder uiBinder = GWT.create(PlayerViewUiBinder.class);

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
	MaterialIcon btnPlay;
	@UiField 
	MaterialLabel lblPlayed;
	@UiField 
	MaterialIcon menuIcon;
	@UiField 
	MaterialDropDown menuDropdown;
	@UiField 
	MaterialLink menuItemAddToZoneDashboard;
	@UiField 
	MaterialLink menuItemAddToProjectDashboard;
	@UiField 
	MaterialLink menuItemRemoveFromDashboard;

	public PlayerView() {
		uiBinder.createAndBindUi(this);
		menuItemAddToZoneDashboard.setText(ProjectLocale.getProjectConst().addToZoneDashboard());
		menuItemAddToProjectDashboard.setText(ProjectLocale.getProjectConst().addToProjectDashboard());
		menuItemRemoveFromDashboard.setText(ProjectLocale.getProjectConst().removeFromDashboard());
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
	
	public MaterialIcon getBtnPlay() {
		return btnPlay;
	}
	
	public MaterialLabel getLblPlayed() {
		return lblPlayed;
	}
	
	public MaterialLink getMenuItemAddToZoneDashboard() {
		return menuItemAddToZoneDashboard;
	}
	
	public MaterialLink getMenuItemAddToProjectDashboard() {
		return menuItemAddToProjectDashboard;
	}
	
	public MaterialLink getMenuItemRemoveFromDashboard() {
		return menuItemRemoveFromDashboard;
	}
	
	@Override
	public void setMenuDropdownId(long id) {
		String activate = "menuDropdown" + id;
		menuIcon.setActivates(activate);
		menuDropdown.setActivator(activate);
	}
}
