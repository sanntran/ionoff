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
import net.ionoff.center.shared.dto.ControllerDto;
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
	
	public void setControllerOptions(List<ControllerDto> controllers) {
		materialCollapsible.clear();
		
		for (final ControllerDto controller : controllers) {
			MaterialCollapsibleItem controllerCollapsibleItem = new MaterialCollapsibleItem();
			materialCollapsible.add(controllerCollapsibleItem);
			
			MaterialCollapsibleHeader controllerCollapsibleHeader = new MaterialCollapsibleHeader();
			controllerCollapsibleItem.add(controllerCollapsibleHeader);
			
			MaterialCollectionItem controllerHeaderContent = new MaterialCollectionItem();
			
			MaterialIcon controllerIcon = new MaterialIcon();
			controllerIcon.setIconType(IconType.MEMORY);
			controllerIcon.setStyleName("icon");
			controllerHeaderContent.add(controllerIcon);
			
			MaterialLabel lblControllerName = new MaterialLabel(controller.getName());
			lblControllerName.setFontSize("15px");
			controllerHeaderContent.add(lblControllerName);
			
			MaterialLabel lblControllerKey = new MaterialLabel(controller.getKey());
			lblControllerKey.addStyleName("key");
			controllerHeaderContent.add(lblControllerKey);
			
			controllerCollapsibleHeader.add(controllerHeaderContent);
			
			MaterialCollapsibleBody controllerCollapsibleBody = new MaterialCollapsibleBody();
			controllerCollapsibleItem.add(controllerCollapsibleBody);
			MaterialCollection relaysCollection = new MaterialCollection();
			controllerCollapsibleBody.add(relaysCollection);
			
			for (final RelayDto relay : controller.getRelays()) {
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
