package net.ionoff.center.client.relay;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.user.client.ui.FlowPanel;

import gwt.material.design.client.constants.Color;
import gwt.material.design.client.constants.WavesType;
import gwt.material.design.client.ui.MaterialButton;
import net.ionoff.center.client.locale.ProjectLocale;

public class RelayGroupView extends FlowPanel {

	private final List<RelayItemView> relayViews;
	private final MaterialButton btnAdd;
	
	public RelayGroupView() {
		addStyleName("relayGroup");
		relayViews = new ArrayList<>();
		btnAdd = new MaterialButton(ProjectLocale.getProjectConst().add());
		btnAdd.setBackgroundColor(Color.WHITE);
		btnAdd.setTextColor(Color.GREY_DARKEN_4);
		btnAdd.setWaves(WavesType.DEFAULT);
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