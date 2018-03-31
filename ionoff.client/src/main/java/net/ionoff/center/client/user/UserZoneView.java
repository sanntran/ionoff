package net.ionoff.center.client.user;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.user.client.ui.CheckBox;
import com.google.gwt.user.client.ui.InlineLabel;
import com.google.gwt.user.client.ui.Label;

import gwt.material.design.client.ui.MaterialCollapsible;
import gwt.material.design.client.ui.MaterialCollapsibleBody;
import gwt.material.design.client.ui.MaterialCollapsibleHeader;
import gwt.material.design.client.ui.MaterialCollapsibleItem;
import gwt.material.design.client.ui.MaterialCollection;
import gwt.material.design.client.ui.MaterialCollectionItem;
import gwt.material.design.client.ui.MaterialLabel;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.shared.dto.UserDeviceDto;
import net.ionoff.center.shared.dto.UserSceneDto;
import net.ionoff.center.shared.dto.UserZoneDto;

public class UserZoneView extends MaterialCollectionItem {

	private final CheckBox checkBoxRole;
	private final Label lblAreaName;
	private final UserZoneDto userZone;
	private final MaterialCollapsible materialCollapsible;
	private final MaterialCollection deviceCollection;
	private final MaterialCollection sceneCollection;
	private final List<UserSceneView> userSceneViews;
	private final List<UserDeviceView> userDeviceViews;
	
	public UserZoneView(UserZoneDto userZone) {
		this.userZone = userZone;
		userDeviceViews = new ArrayList<>();
		userSceneViews = new ArrayList<>(); 
		
		addStyleName("userRoles userZone");
		
		materialCollapsible = new MaterialCollapsible();
		materialCollapsible.setAccordion(false);
		add(materialCollapsible);
		
		MaterialCollapsibleItem zoneCollapsibleItem = new MaterialCollapsibleItem();
		materialCollapsible.add(zoneCollapsibleItem);
				
		MaterialCollapsibleHeader zoneCollapsibleHeader = new MaterialCollapsibleHeader();
		zoneCollapsibleItem.add(zoneCollapsibleHeader);
		
		MaterialCollectionItem zoneHeaderContent = new MaterialCollectionItem();
		
		zoneCollapsibleHeader.add(zoneHeaderContent);
		
		MaterialCollapsibleBody zoneCollapsibleBody = new MaterialCollapsibleBody();
		zoneCollapsibleItem.add(zoneCollapsibleBody);

		checkBoxRole = new CheckBox(userZone.getZoneName());
		zoneHeaderContent.add(checkBoxRole);
		checkBoxRole.setValue(userZone.hasRole());
		
		lblAreaName = new InlineLabel(userZone.getAreaName());
		zoneHeaderContent.add(lblAreaName);
		
		
		MaterialLabel lblDevice = new MaterialLabel();
		lblDevice.addStyleName("lblDevice");
		lblDevice.setText(AdminLocale.getAdminConst().device());
		zoneCollapsibleBody.add(lblDevice);
		
		deviceCollection = new MaterialCollection();
		zoneCollapsibleBody.add(deviceCollection);
		
		MaterialLabel lblScene = new MaterialLabel();
		lblScene.addStyleName("lblScene");
		lblScene.setText(AdminLocale.getAdminConst().scene());
		zoneCollapsibleBody.add(lblScene);
		
		sceneCollection = new MaterialCollection();
		zoneCollapsibleBody.add(sceneCollection);
	
	}

	public UserZoneDto getUserZone() {
		return userZone;
	}

	public CheckBox getCheckBoxRole() {
		return this.checkBoxRole;
	}

	public int getCheckBoxValue() {
		return this.checkBoxRole.getValue() == true ? 1 : 0;
	}
	
	public void setUserDevices(List<UserDeviceDto> userDevices) {
		deviceCollection.clear();
		userDeviceViews.clear();
		
		for (final UserDeviceDto userDevice : userDevices) {
			UserDeviceView userDeviceView = new UserDeviceView(userDevice);
			deviceCollection.add(userDeviceView);
			userDeviceViews.add(userDeviceView);
		}
	}
	
	public void setUserScenes(List<UserSceneDto> userScenes) {
		sceneCollection.clear();
		userSceneViews.clear();
		
		for (final UserSceneDto userScene : userScenes) {
			UserSceneView userSceneView = new UserSceneView(userScene);
			sceneCollection.add(userSceneView);
			userSceneViews.add(userSceneView);
		}
	}

	public List<UserDeviceView> getUserDevices() {
		return userDeviceViews;
	}

	public List<UserSceneView> getUserScenes() {
		return userSceneViews;
	}
}