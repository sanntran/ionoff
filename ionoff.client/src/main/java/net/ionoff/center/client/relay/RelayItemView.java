package net.ionoff.center.client.relay;

import com.google.gwt.dom.client.Style.Float;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.InlineLabel;
import com.google.gwt.user.client.ui.Label;

import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.ui.MaterialIcon;
import net.ionoff.center.shared.dto.RelayDto;

public class RelayItemView extends FlowPanel {

	private final Label lblName;
	private final Label lblDriverName;
	private final MaterialIcon btnDelete;
	private final MaterialIcon btnLeader;

	public RelayItemView(RelayDto relay) {
		addStyleName("relay");
		lblName = new InlineLabel(relay.getName());
		add(lblName);
		lblName.addStyleName("name");
		
		lblDriverName = new InlineLabel(relay.getRelayDriverName());
		lblDriverName.addStyleName("driver");
		add(lblDriverName);
		
		btnDelete = new MaterialIcon(); 
		btnDelete.addStyleName("del");
		btnDelete.setFloat(Float.RIGHT);
		btnDelete.setIconType(IconType.DELETE);
		add(btnDelete);
		
		btnLeader = new MaterialIcon(); 
		btnLeader.addStyleName("leader");
		if (!Boolean.TRUE.equals(relay.getIsLeader())) {
			btnLeader.addStyleName("none");
		}
		btnLeader.setFloat(Float.RIGHT);
		btnLeader.setIconType(IconType.FLAG);
		add(btnLeader);
	}

	public MaterialIcon getBtnRemove() {
		return btnDelete;
	}
	
	public MaterialIcon getBtnLeader() {
		return btnLeader;
	}
}