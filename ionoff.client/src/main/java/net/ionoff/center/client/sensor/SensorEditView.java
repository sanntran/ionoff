package net.ionoff.center.client.sensor;

import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.ui.MaterialIntegerBox;
import gwt.material.design.client.ui.MaterialListBox;
import net.ionoff.center.client.base.AbstractEditView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.shared.dto.SensorDto;
import net.ionoff.center.shared.entity.SensorType;

public class SensorEditView extends AbstractEditView<SensorDto> implements SensorEditPresenter.Display {

    private MaterialIntegerBox intBoxOrder;
	private MaterialListBox listBoxTypes;
	private MaterialListBox listBoxControllers;
	private MaterialIntegerBox intBoxInputIndex;

	public SensorEditView() {
		super();
		getLblIcon().setIconType(IconType.WIFI_TETHERING);

		intBoxOrder = new MaterialIntegerBox();
        intBoxOrder.setMin("0");
        intBoxOrder.setLabel(AdminLocale.getAdminConst().order());
        intBoxOrder.setPlaceholder(AdminLocale.getAdminConst().order());
		contentPanel.add(intBoxOrder);

		listBoxTypes = new MaterialListBox();
		listBoxTypes.setPlaceholder(AdminLocale.getAdminConst().type());
		listBoxTypes.addItem(SensorType.DIGITAL.toString());
		listBoxTypes.addItem(SensorType.ANALOG.toString());
		contentPanel.add(listBoxTypes);
		
		listBoxControllers = new MaterialListBox();
		listBoxControllers.setPlaceholder(AdminLocale.getAdminConst().controller());
		contentPanel.add(listBoxControllers);
		
		intBoxInputIndex = new MaterialIntegerBox();
		intBoxInputIndex.setMin("0");
		intBoxInputIndex.setLabel(AdminLocale.getAdminConst().input() + " " + AdminLocale.getAdminConst().inputNote());
		intBoxInputIndex.setPlaceholder(AdminLocale.getAdminConst().input() + " " +  AdminLocale.getAdminConst().inputNote());
		contentPanel.add(intBoxInputIndex);
	}	
	
	@Override
	public MaterialListBox getListBoxTypes() {
		return listBoxTypes;
	}

	@Override
	public MaterialListBox getListBoxControllers() {
		return listBoxControllers;
	}

    @Override
    public MaterialIntegerBox getIntBoxOrder() {
        return intBoxOrder;
    }

    @Override
	public MaterialIntegerBox getIntBoxInputIndex() {
		return intBoxInputIndex;
	}

}
