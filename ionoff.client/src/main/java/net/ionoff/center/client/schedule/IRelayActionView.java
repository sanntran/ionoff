package net.ionoff.center.client.schedule;

import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.Widget;

import gwt.material.design.client.ui.MaterialListBox;

public interface IRelayActionView  {
	Label getLblRelayNameId();
	MaterialListBox getListBoxActions();
	Widget asWidget();
}