package net.ionoff.center.client.sensor;

import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.ui.MaterialIntegerBox;
import gwt.material.design.client.ui.MaterialListBox;
import net.ionoff.center.client.base.AbstractEditView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.shared.dto.SensorDto;

public class SensorEditView extends AbstractEditView<SensorDto> implements SensorEditPresenter.Display {
	
	private MaterialListBox listBoxRelayDrivers;
	private MaterialIntegerBox intBoxInputIndex;
	
	public SensorEditView() {
		super();
		getLblIcon().setIconType(IconType.WIFI_TETHERING);
		
		listBoxRelayDrivers = new MaterialListBox();
		listBoxRelayDrivers.setPlaceholder(AdminLocale.getAdminConst().relayDriver());
		contentPanel.add(listBoxRelayDrivers);
		
		intBoxInputIndex = new MaterialIntegerBox();
		intBoxInputIndex.setLabel(AdminLocale.getAdminConst().input());
		contentPanel.add(intBoxInputIndex);
	}

	@Override
	public MaterialListBox getListBoxRelayDrivers() {
		return listBoxRelayDrivers;
	}
	
	@Override
	public MaterialIntegerBox getIntBoxRelayDriverInputIdx() {
		return intBoxInputIndex;
	}
}
