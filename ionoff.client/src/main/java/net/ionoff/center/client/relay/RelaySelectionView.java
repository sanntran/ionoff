package net.ionoff.center.client.relay;

import java.util.List;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.user.client.ui.FlowPanel;

import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.ui.MaterialCollapsible;
import gwt.material.design.client.ui.MaterialCollapsibleBody;
import gwt.material.design.client.ui.MaterialCollapsibleHeader;
import gwt.material.design.client.ui.MaterialCollapsibleItem;
import gwt.material.design.client.ui.MaterialCollection;
import gwt.material.design.client.ui.MaterialCollectionItem;
import gwt.material.design.client.ui.MaterialIcon;
import gwt.material.design.client.ui.MaterialLabel;
import net.ionoff.center.shared.dto.RelayDriverDto;
import net.ionoff.center.shared.dto.RelayDto;

public class RelaySelectionView extends FlowPanel {
	
	private FlowPanel selectPanel;
	private MaterialCollapsible materialCollapsible;
	private RelaySelectionHandler relaySelectionHandler;
	
	public RelaySelectionView() {
		addStyleName("relaySelection");
		
		materialCollapsible = new MaterialCollapsible();
		materialCollapsible.setAccordion(false);
		add(materialCollapsible);
		materialCollapsible.setVisible(false);
	}
	
	public void setRelayDriverOptions(List<RelayDriverDto> relayDrivers) {
		materialCollapsible.clear();
		
		for (final RelayDriverDto relayDriver : relayDrivers) {
			MaterialCollapsibleItem relayDriverCollapsibleItem = new MaterialCollapsibleItem();
			materialCollapsible.add(relayDriverCollapsibleItem);
			
			MaterialCollapsibleHeader relayDriverCollapsibleHeader = new MaterialCollapsibleHeader();
			relayDriverCollapsibleItem.add(relayDriverCollapsibleHeader);
			
			MaterialCollectionItem relayDriverHeaderContent = new MaterialCollectionItem();
			
			MaterialIcon relayDriverIcon = new MaterialIcon();
			relayDriverIcon.setIconType(IconType.MEMORY);
			relayDriverIcon.setStyleName("icon");
			relayDriverHeaderContent.add(relayDriverIcon);
			
			MaterialLabel lblRelayDriverName = new MaterialLabel(relayDriver.getName());
			lblRelayDriverName.setFontSize("15px");
			relayDriverHeaderContent.add(lblRelayDriverName);
			
			MaterialLabel lblRelayDriverKey = new MaterialLabel(relayDriver.getKey());
			lblRelayDriverKey.addStyleName("key");
			relayDriverHeaderContent.add(lblRelayDriverKey);
			
			relayDriverCollapsibleHeader.add(relayDriverHeaderContent);
			
			MaterialCollapsibleBody relayDriverCollapsibleBody = new MaterialCollapsibleBody();
			relayDriverCollapsibleItem.add(relayDriverCollapsibleBody);
			MaterialCollection relaysCollection = new MaterialCollection();
			relayDriverCollapsibleBody.add(relaysCollection);
			
			for (final RelayDto relay : relayDriver.getRelays()) {
				MaterialCollectionItem relayItem = new MaterialCollectionItem();
				relayItem.addStyleName("relay");
				relaysCollection.add(relayItem);
				
				MaterialLabel lblRelayName = new MaterialLabel(relay.getName());
				lblRelayName.setFontSize("15px");
				relayItem.add(lblRelayName);
				
				MaterialLabel lblDeviceName = new MaterialLabel(relay.getDeviceName());
				lblDeviceName.setFontSize("12px");
				relayItem.add(lblDeviceName);
				
				relayItem.addClickHandler(new ClickHandler() {
					@Override
					public void onClick(ClickEvent event) {
						if (relaySelectionHandler != null) {
							relaySelectionHandler.onRelaySelected(relay);
						}
						materialCollapsible.setVisible(false);
					}
				});
			}
		}
	}
	

	public void setVisibility(boolean visible) {
		materialCollapsible.setVisible(visible);
	}
	
	public void setRelaySelectionHandler(RelaySelectionHandler relaySelectionHandler) {
		this.relaySelectionHandler = relaySelectionHandler;
	}
}
