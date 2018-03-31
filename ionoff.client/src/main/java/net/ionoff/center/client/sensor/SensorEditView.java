package net.ionoff.center.client.sensor;

import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.ui.MaterialIntegerBox;
import gwt.material.design.client.ui.MaterialListBox;
import net.ionoff.center.client.common.AbstractEditView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.shared.dto.SensorDto;

public class SensorEditView extends AbstractEditView<SensorDto> implements SensorEditPresenter.Display {
	
	private MaterialListBox listBoxControllers;
	private MaterialIntegerBox intBoxInputIndex;
	
	public SensorEditView() {
		super();
		getLblIcon().setIconType(IconType.WIFI_TETHERING);
		
		listBoxControllers = new MaterialListBox();
		listBoxControllers.setPlaceholder(AdminLocale.getAdminConst().controller());
		contentPanel.add(listBoxControllers);
		
		intBoxInputIndex = new MaterialIntegerBox();
		intBoxInputIndex.setLabel(AdminLocale.getAdminConst().input());
		contentPanel.add(intBoxInputIndex);
	}

	@Override
	public MaterialListBox getListBoxControllers() {
		return listBoxControllers;
	}
	
	@Override
	public MaterialIntegerBox getIntBoxControllerInputIdx() {
		return intBoxInputIndex;
	}
}
