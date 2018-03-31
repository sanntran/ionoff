package net.ionoff.center.client.device;

import java.util.List;

import com.google.gwt.user.client.ui.Composite;

import gwt.material.design.client.ui.MaterialLink;
import gwt.material.design.client.ui.MaterialSwitch;

public abstract class ApplianceView extends Composite implements IDeviceView {
	
	public abstract MaterialSwitch getBtnSwitch();
	
	public abstract List<RelayView> getRelayViews();
	
	public abstract MaterialLink getMenuItemAddToZoneDashboard();
	
	public abstract MaterialLink getMenuItemAddToProjectDashboard();
	
	public abstract MaterialLink getMenuItemRemoveFromDashboard();

}
