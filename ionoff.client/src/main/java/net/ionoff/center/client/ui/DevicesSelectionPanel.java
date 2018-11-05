package net.ionoff.center.client.ui;

import java.util.List;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.user.client.ui.FlowPanel;

import gwt.material.design.client.constants.ButtonType;
import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.constants.WavesType;
import gwt.material.design.client.ui.MaterialButton;
import gwt.material.design.client.ui.MaterialCollapsible;
import gwt.material.design.client.ui.MaterialCollapsibleBody;
import gwt.material.design.client.ui.MaterialCollapsibleHeader;
import gwt.material.design.client.ui.MaterialCollapsibleItem;
import gwt.material.design.client.ui.MaterialCollection;
import gwt.material.design.client.ui.MaterialCollectionItem;
import gwt.material.design.client.ui.MaterialLabel;
import gwt.material.design.client.ui.MaterialTextBox;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.shared.dto.AreaDto;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.DeviceDto;
import net.ionoff.center.shared.dto.ZoneDto;

public class DevicesSelectionPanel extends FlowPanel {
	
	private boolean enable;
	private FlowPanel selectPanel;
	private MaterialTextBox textBoxSelectedDevice;
	private MaterialButton btnClearSelectedDevice;
	
	private MaterialCollapsible materialCollapsible;
	
	public DevicesSelectionPanel() {
		enable = true;
		addStyleName("deviceSelection");
		
		selectPanel = new FlowPanel();
		add(selectPanel);
		
		textBoxSelectedDevice = new MaterialTextBox();
		textBoxSelectedDevice.setLabel(AdminLocale.getAdminConst().device());
		selectPanel.add(textBoxSelectedDevice);
		
		btnClearSelectedDevice = new MaterialButton(ButtonType.FLAT);
		btnClearSelectedDevice.addStyleName("clear");
		btnClearSelectedDevice.setIconType(IconType.CLOSE);
		btnClearSelectedDevice.setWaves(WavesType.LIGHT);
		selectPanel.add(btnClearSelectedDevice);
		
		materialCollapsible = new MaterialCollapsible();
		materialCollapsible.setAccordion(false);
		add(materialCollapsible);
		materialCollapsible.setVisible(false);
		
		btnClearSelectedDevice.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (!enable) {
					return;
				}
				materialCollapsible.setVisible(false);
				textBoxSelectedDevice.setText(AdminLocale.getAdminConst().none());
			}
		});
		
		textBoxSelectedDevice.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (!enable) {
					return;
				}
				if (materialCollapsible.isVisible()) {
					materialCollapsible.setVisible(false);
				}
				else {
					materialCollapsible.setVisible(true);
				}
			}
		});
	}
	
	public void setSelectedItem(Long itemId, String itemName) {
		if (itemId == null) {
			textBoxSelectedDevice.setText(AdminLocale.getAdminConst().none());
		}
		else {
			textBoxSelectedDevice.setText(BaseDto.formatNameID(itemName, itemId));
		}
	}
	
	/**
	 * @param areas List of areas that contains devices
	 */
	public void setDeviceOptions(List<AreaDto> areas) {
		materialCollapsible.clear();
		
		for (final AreaDto area : areas) {
			for (final ZoneDto zone : area.getZones()) {
				MaterialCollapsibleItem zoneCollapsibleItem = new MaterialCollapsibleItem();
				materialCollapsible.add(zoneCollapsibleItem);
				
				
				MaterialCollapsibleHeader zoneCollapsibleHeader = new MaterialCollapsibleHeader();
				zoneCollapsibleItem.add(zoneCollapsibleHeader);
				
				MaterialCollectionItem zoneHeaderContent = new MaterialCollectionItem();
				
				MaterialLabel zoneIcon = new MaterialLabel();
				zoneIcon.setStyleName("icon zone");
				zoneHeaderContent.add(zoneIcon);
				
				MaterialLabel zoneLbl = new MaterialLabel(BaseDto.formatNameID(zone));
				zoneLbl.setFontSize("15px");
				zoneHeaderContent.add(zoneLbl);
				
				MaterialLabel areaLbl = new MaterialLabel(BaseDto.formatNameID(area));
				zoneHeaderContent.add(areaLbl);
				
				zoneCollapsibleHeader.add(zoneHeaderContent);
				
				MaterialCollapsibleBody zoneCollapsibleBody = new MaterialCollapsibleBody();
				zoneCollapsibleItem.add(zoneCollapsibleBody);
				MaterialCollection devicesCollection = new MaterialCollection();
				zoneCollapsibleBody.add(devicesCollection);
				
				for (final DeviceDto device : zone.getDevices()) {
					MaterialCollectionItem deviceItem = new MaterialCollectionItem();
					deviceItem.addStyleName(device.styleName());
					devicesCollection.add(deviceItem);
					
					MaterialLabel deviceLbl = new MaterialLabel(device.formatNameID());
					deviceLbl.setFontSize("15px");
					deviceItem.add(deviceLbl);
					
					deviceItem.addClickHandler(new ClickHandler() {
						@Override
						public void onClick(ClickEvent event) {
							textBoxSelectedDevice.setText(device.formatNameID());
							materialCollapsible.setVisible(false);
						}
					});
				}
			}
		}
	}
	
	public MaterialButton getBtnClearSelectedDevice() {
		return btnClearSelectedDevice;
	}
	
	
	public void setEnable(boolean enable) {
		this.enable = enable;
		if (!enable) {
			materialCollapsible.setVisible(false);
		}
	}

	public String getSelectedDevice() {
		return textBoxSelectedDevice.getText();
	}
}
