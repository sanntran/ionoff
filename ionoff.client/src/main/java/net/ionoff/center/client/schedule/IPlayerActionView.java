package net.ionoff.center.client.schedule;

import com.google.gwt.user.client.ui.FlowPanel;

import gwt.material.design.client.ui.MaterialIntegerBox;
import gwt.material.design.client.ui.MaterialListBox;
import gwt.material.design.client.ui.MaterialTextBox;

public interface IPlayerActionView  {
	FlowPanel asPanel();
	MaterialListBox getListBoxActions();
	MaterialIntegerBox getIntBoxVolume();
	MaterialTextBox getTextBoxAlbum();
	FlowPanel getAlbumContainer();
	FlowPanel getAlbumSelectionPanel();
}