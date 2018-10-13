package net.ionoff.center.client.device;

import com.google.gwt.user.client.ui.HTMLPanel;

import gwt.material.design.client.ui.MaterialIcon;
import gwt.material.design.client.ui.MaterialImage;
import gwt.material.design.client.ui.MaterialLabel;

public interface IDeviceView {
	HTMLPanel asPanel();
	MaterialLabel getLblName();
	MaterialLabel getLblZone();
	MaterialIcon getBtnIcon();
	MaterialLabel getLblTime();
	void setMenuDropdownId(long id);
}
