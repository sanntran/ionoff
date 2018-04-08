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

public class WeighScaleView extends FocusPanel implements IDeviceView {

	@UiTemplate("WeighScaleView.ui.xml")
	interface WeighScaleViewUiBinder extends UiBinder<Widget, WeighScaleView> {
	}

	private static WeighScaleViewUiBinder uiBinder = GWT.create(WeighScaleViewUiBinder.class);

	@UiField
	HTMLPanel root;
	@UiField
	MaterialPanel scaleCard;
	@UiField 
	MaterialImage imgIcon;
	@UiField 
	MaterialLabel lblName;
	@UiField 
	MaterialLabel lblZone;
	@UiField 
	MaterialLabel lblTime;
	@UiField 
	MaterialLabel lblLatestValue;
	@UiField 
	MaterialLabel lblSetupValue;
	@UiField 
	MaterialLabel lblTotalValue;
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

	public WeighScaleView() {
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
	public MaterialImage getImgIcon() {
		return imgIcon;
	}

	@Override
	public MaterialLabel getLblTime() {
		return lblTime;
	}
	
	public MaterialLabel getLblLatestValue() {
		return lblLatestValue;
	}
	
	public MaterialLabel getLblSetupValue() {
		return lblSetupValue;
	}
	
	public MaterialLabel getLblTotalValue() {
		return lblTotalValue;
	}
	
	@Override
	public MaterialLabel getLblName() {
		return lblName;
	}
	
	@Override
	public MaterialLabel getLblZone() {
		return lblZone;
	}

	public MaterialPanel getScaleCard() {
		return scaleCard;
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
