package net.ionoff.center.client.schedule;

import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.InlineLabel;
import com.google.gwt.user.client.ui.Label;

import gwt.material.design.client.ui.MaterialListBox;

public class RelayActionView extends FlowPanel implements IRelayActionView {
		
	private final Label lblIcon;
	private final Label lblRelayNameId;
	private final MaterialListBox listBoxActions;
	
	public RelayActionView() {
		
		setStyleName("relayAction");
				
		lblIcon = new Label();
		lblIcon.setStyleName("icon relay");
		add(lblIcon);
		
		lblRelayNameId = new InlineLabel();
		lblRelayNameId.setStyleName("name");
		add(lblRelayNameId);
		
		listBoxActions = new MaterialListBox();
		listBoxActions.addStyleName("listBox");
		
		add(listBoxActions);
	}

	@Override
	public MaterialListBox getListBoxActions() {
		return listBoxActions;
	}

	@Override
	public Label getLblRelayNameId() {
		return lblRelayNameId;
	}
}