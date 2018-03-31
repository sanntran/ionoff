package net.ionoff.center.client.device;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.core.client.GWT;
import com.google.gwt.uibinder.client.UiBinder;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiTemplate;
import com.google.gwt.user.client.ui.HTMLPanel;
import com.google.gwt.user.client.ui.Widget;

import gwt.material.design.client.ui.MaterialCollection;
import gwt.material.design.client.ui.MaterialDropDown;
import gwt.material.design.client.ui.MaterialIcon;
import gwt.material.design.client.ui.MaterialImage;
import gwt.material.design.client.ui.MaterialLabel;
import gwt.material.design.client.ui.MaterialLink;
import gwt.material.design.client.ui.MaterialSwitch;
import net.ionoff.center.client.locale.ProjectLocale;

public class ApplianceCollapsibleView extends ApplianceView {
	
	@UiTemplate("ApplianceCollapsibleView.ui.xml")
	interface ApplianceCollapsibleViewUiBinder extends UiBinder<Widget, ApplianceCollapsibleView> {
	}

	private static ApplianceCollapsibleViewUiBinder uiBinder = GWT.create(ApplianceCollapsibleViewUiBinder.class);
	
	@UiField
	HTMLPanel root;
	@UiField 
	MaterialImage imgIcon; 
	@UiField 
	MaterialLabel lblName;
	@UiField 
	MaterialLabel lblTime;
	@UiField 
	MaterialLabel lblZone;
	@UiField 
	MaterialCollection relayCollection;
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
	
	private List<RelayView> relayViews;

	public ApplianceCollapsibleView() {
		uiBinder.createAndBindUi(this);
		relayViews = new ArrayList<>();
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
	
	@Override
	public MaterialLabel getLblName() {
		return lblName;
	}
	
	@Override
	public MaterialLabel getLblZone() {
		return lblZone;
	}
	
	public MaterialCollection getRelayCollection() {
		return relayCollection;
	}
	
	@Override
	public List<RelayView> getRelayViews() {
		return relayViews;
	}

	@Override
	public MaterialSwitch getBtnSwitch() {
		return null;
	}

	@Override
	public void setMenuDropdownId(long id) {
		String activate = "menuDropdown" + id;
		menuIcon.setActivates(activate);
		menuDropdown.setActivator(activate);
	}
	
	@Override
	public MaterialLink getMenuItemAddToZoneDashboard() {
		return menuItemAddToZoneDashboard;
	}
	
	@Override
	public MaterialLink getMenuItemAddToProjectDashboard() {
		return menuItemAddToProjectDashboard;
	}
	
	@Override
	public MaterialLink getMenuItemRemoveFromDashboard() {
		return menuItemRemoveFromDashboard;
	}
}
