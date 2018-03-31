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
	private final Label lblControllerName;
	private final MaterialIcon btnDelete;

	public RelayItemView(RelayDto relay) {
		addStyleName("relay");
		lblName = new InlineLabel(relay.getName());
		add(lblName);
		lblName.addStyleName("name");
		lblControllerName = new InlineLabel(relay.getControllerName());
		add(lblControllerName);
		lblControllerName.addStyleName("controller");
		btnDelete = new MaterialIcon(); 
		btnDelete.addStyleName("del");
		btnDelete.setFloat(Float.RIGHT);
		btnDelete.setIconType(IconType.DELETE);
		add(btnDelete);
	}

	public MaterialIcon getBtnRemove() {
		return btnDelete;
	}
}