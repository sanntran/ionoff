package net.ionoff.center.client.user;

import com.google.gwt.user.client.ui.CheckBox;

import gwt.material.design.client.ui.MaterialCollectionItem;
import net.ionoff.center.shared.dto.UserDeviceDto;

public class UserDeviceView extends MaterialCollectionItem {

	private final CheckBox checkBoxRole;
	private final UserDeviceDto userDevice;

	public UserDeviceView(UserDeviceDto userDevice) {
		this.userDevice = userDevice;
		addStyleName("userDevice");
		checkBoxRole = new CheckBox(userDevice.getDeviceName());
		add(checkBoxRole);
		checkBoxRole.setValue(userDevice.hasRole());
	}

	public UserDeviceDto getUserDevice() {
		return userDevice;
	}

	public CheckBox getCheckBoxRole() {
		return this.checkBoxRole;
	}

	public int getCheckBoxValue() {
		return this.checkBoxRole.getValue() == true ? 1 : 0;
	}
}