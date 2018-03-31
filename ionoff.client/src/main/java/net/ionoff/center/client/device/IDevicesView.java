package net.ionoff.center.client.device;

import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.Label;

public interface IDevicesView {
	FlowPanel asPanel();
	Label getLblIcon();
	Label getLblName();
}
