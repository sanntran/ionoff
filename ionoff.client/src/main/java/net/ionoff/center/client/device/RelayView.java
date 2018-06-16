package net.ionoff.center.client.device;

import com.google.gwt.core.client.GWT;
import com.google.gwt.uibinder.client.UiBinder;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiTemplate;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.Widget;

import gwt.material.design.client.constants.Color;
import gwt.material.design.client.ui.MaterialCollectionItem;
import gwt.material.design.client.ui.MaterialIcon;
import gwt.material.design.client.ui.MaterialLabel;
import gwt.material.design.client.ui.MaterialSwitch;
import net.ionoff.center.shared.dto.RelayDto;
import net.ionoff.center.shared.dto.StatusDto;

public class RelayView extends Composite {
	
	@UiTemplate("RelayView.ui.xml")
	interface RelayViewUiBinder extends UiBinder<Widget, RelayView> {
	}

	private static RelayViewUiBinder uiBinder = GWT.create(RelayViewUiBinder.class);
	
	@UiField
	MaterialCollectionItem root;
	@UiField 
	MaterialIcon icon;
	@UiField 
	MaterialLabel lblName;
	@UiField 
	MaterialLabel lblTime;
	@UiField 
	MaterialSwitch btnSwitch;
	
	private boolean locked;
	private final RelayDto relay;

	public RelayView(RelayDto relay) {
		uiBinder.createAndBindUi(this);
		this.relay = relay;
		if (relay.izAutoRevert()) {
			btnSwitch.addStyleName("press");
		}
		lblName.setText(relay.getName());
		lblTime.setText(relay.getTime());
	}

	public void displayStatus() {
		if (Boolean.TRUE.equals(relay.getStatus())) {
			icon.setTextColor(Color.GREEN_ACCENT_1);
			btnSwitch.setValue(true);
		}
		else {
			icon.setTextColor(Color.GREY_LIGHTEN_3);
			btnSwitch.setValue(false);
		}
		lblTime.setText(relay.getTime());
	}

	public RelayDto getRelay() {
		return relay;
	}
	
	public MaterialSwitch getBtnSwitch() {
		return btnSwitch;
	}
	
	public MaterialCollectionItem asPanel() {
		return root;
	}

	public boolean isLocked() {
		return locked;
	}

	public void setLocked(boolean locked) {
		this.locked = locked;
	}

	public void setStatus(StatusDto status) {
		if (isLocked()) {
			return;
		}
		relay.setStatus(status.getValue());
		relay.setTime(status.getTime());
		displayStatus();
	}
}
