package net.ionoff.center.client.device;

import com.google.gwt.core.client.GWT;
import com.google.gwt.uibinder.client.UiBinder;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiTemplate;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.HTMLPanel;
import com.google.gwt.user.client.ui.Widget;

import gwt.material.design.client.ui.MaterialButton;
import gwt.material.design.client.ui.MaterialDropDown;
import gwt.material.design.client.ui.MaterialIcon;
import gwt.material.design.client.ui.MaterialImage;
import gwt.material.design.client.ui.MaterialLabel;
import gwt.material.design.client.ui.MaterialLink;
import gwt.material.design.client.ui.MaterialSwitch;
import net.ionoff.center.client.locale.ProjectLocale;

public class LightView extends Composite implements IDeviceView {
	
	@UiTemplate("LightView.ui.xml")
	interface LightViewUiBinder extends UiBinder<Widget, LightView> {
	}

	private static LightViewUiBinder uiBinder = GWT.create(LightViewUiBinder.class);
	
	@UiField
	HTMLPanel root;
	@UiField
	MaterialButton btnIcon;
	@UiField 
	MaterialImage imgIcon;
	@UiField 
	MaterialLabel lblName;
	@UiField 
	MaterialLabel lblZone;
	@UiField 
	MaterialLabel lblTime;
	@UiField 
	MaterialSwitch btnSwitch;
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

	public LightView() {
		uiBinder.createAndBindUi(this);
		menuItemAddToZoneDashboard.setText(ProjectLocale.getProjectConst().addToZoneDashboard());
		menuItemAddToProjectDashboard.setText(ProjectLocale.getProjectConst().addToProjectDashboard());
		menuItemRemoveFromDashboard.setText(ProjectLocale.getProjectConst().removeFromDashboard());
	}

	@Override
	public HTMLPanel asPanel() {
		return root;
	}

	public MaterialButton getBtnIcon() {
		return btnIcon;
	}
	
	@Override
	public MaterialImage getImgIcon() {
		return imgIcon;
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
	
	public MaterialSwitch getBtnSwitch() {
		return btnSwitch;
	}

	@Override
	public void setMenuDropdownId(long id) {
		String activate = "menuDropdown" + id;
		menuIcon.setActivates(activate);
		menuDropdown.setActivator(activate);
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
}
