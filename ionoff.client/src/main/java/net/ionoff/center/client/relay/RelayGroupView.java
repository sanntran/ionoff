package net.ionoff.center.client.relay;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.user.client.ui.FlowPanel;

import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.constants.WavesType;
import gwt.material.design.client.ui.MaterialButton;

public class RelayGroupView extends FlowPanel {

	private final List<RelayItemView> relayViews;
	private final MaterialButton btnAdd;
	
	public RelayGroupView() {
		addStyleName("relayGroup");
		relayViews = new ArrayList<>();
		btnAdd = new MaterialButton();
		btnAdd.setIconType(IconType.ADD);
		btnAdd.setWaves(WavesType.LIGHT);
		btnAdd.addStyleName("add");
		add(btnAdd);
	}
	
	public MaterialButton getBtnAdd() {
		return btnAdd;
	}
	
	public List<RelayItemView> getRelayViews() {
		return relayViews;
	}
}