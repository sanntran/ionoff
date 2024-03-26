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
	private MaterialListBox listBoxGateways;
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
		
		listBoxGateways = new MaterialListBox();
		listBoxGateways.setPlaceholder(AdminLocale.getAdminConst().controller());
		contentPanel.add(listBoxGateways);
		
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
	public MaterialListBox getListBoxGateways() {
		return listBoxGateways;
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
