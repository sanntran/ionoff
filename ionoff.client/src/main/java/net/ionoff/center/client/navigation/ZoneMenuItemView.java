package net.ionoff.center.client.navigation;

import gwt.material.design.client.constants.ButtonType;
import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.constants.WavesType;
import gwt.material.design.client.ui.MaterialButton;
import gwt.material.design.client.ui.MaterialIcon;
import gwt.material.design.client.ui.MaterialTitle;
import net.ionoff.center.shared.dto.ZoneDto;

public class ZoneMenuItemView extends MaterialButton {
	
	private MaterialTitle lblTitle;
	private boolean selected;
	private final ZoneDto zone;
	
	public ZoneMenuItemView(ZoneDto zone) {
		this.zone = zone;
		setType(ButtonType.FLAT);
		setWaves(WavesType.LIGHT);
		addStyleName("zone");
		MaterialIcon icon = new MaterialIcon();
		icon.setIconType(IconType.BORDER_OUTER);
		add(icon);
		lblTitle = new MaterialTitle();
		add(lblTitle);
		lblTitle.setTitle(zone.getName());
		lblTitle.setDescription(zone.getAreaName());
	}
	
	@Override
	public ZoneMenuItemView asWidget() {
		return this;
	}
	
	public void setSelected(boolean selected) {
		this.selected = selected;
		removeStyleName("selected");
		if (selected) {
			addStyleName("selected");
		}
	}
	
	public boolean isSelected() {
		return this.selected;
	}

	public ZoneDto getZone() {
		return zone;
	}
}
