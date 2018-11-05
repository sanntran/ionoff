package net.ionoff.center.client.scene;

import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.InlineLabel;
import com.google.gwt.user.client.ui.Label;

import gwt.material.design.client.ui.MaterialCollapsibleBody;
import gwt.material.design.client.ui.MaterialCollapsibleHeader;
import gwt.material.design.client.ui.MaterialCollapsibleItem;
import gwt.material.design.client.ui.MaterialCollectionItem;
import gwt.material.design.client.ui.MaterialIntegerBox;
import gwt.material.design.client.ui.MaterialLabel;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.DeviceDto;
import net.ionoff.center.shared.dto.MediaPlayerDto;
import net.ionoff.center.shared.dto.SceneDeviceDto;

public class SceneDeviceView extends MaterialCollapsibleItem implements SceneDevicePresenter.Display {

	private final MaterialCollapsibleHeader collapsibleHeader;
	private final MaterialIntegerBox intBoxOrder;
	private final MaterialIntegerBox intBoxDuration;
	private final FlowPanel deviceActionsContainer;
	
	public SceneDeviceView(SceneDeviceDto sceneDevice) {
		setStyleName("deviceActions");

		collapsibleHeader = new MaterialCollapsibleHeader();
		add(collapsibleHeader);
		
		MaterialCollectionItem headerContent = new MaterialCollectionItem();
		
		MaterialLabel deviceLbl = new MaterialLabel(BaseDto.formatNameID(sceneDevice.getDevice()));
		deviceLbl.setFontSize("15px");
		headerContent.add(deviceLbl);
		
		MaterialLabel zoneLbl = new MaterialLabel(BaseDto.formatNameID(sceneDevice.getDevice().getZoneName(), sceneDevice.getDevice().getZoneId()));
		headerContent.add(zoneLbl);
		
		collapsibleHeader.add(headerContent);
		
		MaterialCollapsibleBody collapsibleBody = new MaterialCollapsibleBody();
		add(collapsibleBody);
		
		intBoxOrder = new MaterialIntegerBox();
		intBoxOrder.addStyleName("order");
		intBoxOrder.setLabel(AdminLocale.getAdminConst().order());
		intBoxOrder.setValue(sceneDevice.getOrder());
		collapsibleBody.add(intBoxOrder);
		
		intBoxDuration = new MaterialIntegerBox();
		intBoxDuration.addStyleName("duration");
		intBoxDuration.setLabel(AdminLocale.getAdminConst().durationSecond());
		intBoxDuration.setValue(sceneDevice.getDuration());
		collapsibleBody.add(intBoxDuration);
		
		Label lblAction = new InlineLabel(AdminLocale.getAdminConst().action());
		lblAction.setStyleName("lbl");
		collapsibleBody.add(lblAction);
		
		deviceActionsContainer = new FlowPanel();
		collapsibleBody.add(deviceActionsContainer);
	}

	private String getStyle(DeviceDto device) {
		if (device instanceof MediaPlayerDto) {
			return "player";
		}
		return "relayLoad";
	}

	@Override
	public MaterialCollapsibleHeader getCollapsibleHeader() {
		return collapsibleHeader;
	}

	@Override
	public MaterialCollapsibleItem asPanel() {
		return this;
	}
	
	@Override
	public MaterialIntegerBox getTntBoxOrder() {
		return intBoxOrder;
	}
	
	
	@Override
	public MaterialIntegerBox getIntBoxDuration() {
		return intBoxDuration;
	}
	
	@Override
	public FlowPanel getDeviceActionsContainer() {
		return deviceActionsContainer;
	}
}
