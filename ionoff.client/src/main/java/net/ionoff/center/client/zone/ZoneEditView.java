package net.ionoff.center.client.zone;


import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.ui.MaterialIntegerBox;
import gwt.material.design.client.ui.MaterialListBox;
import net.ionoff.center.client.common.AbstractEditView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.shared.dto.ZoneDto;

public class ZoneEditView extends AbstractEditView<ZoneDto> implements ZoneEditPresenter.Display {
	
	private MaterialIntegerBox intBoxOrder;
	private MaterialListBox listBoxAreas;
	
	public ZoneEditView() {
		super();
		getLblIcon().setIconType(IconType.CROP_3_2);
		
		intBoxOrder = new MaterialIntegerBox();
		intBoxOrder.setLabel(AdminLocale.getAdminConst().order());
		contentPanel.add(intBoxOrder);
		
		listBoxAreas = new MaterialListBox();
		listBoxAreas.setPlaceholder(AdminLocale.getAdminConst().area());
		contentPanel.add(listBoxAreas);
	}

	@Override
	public MaterialIntegerBox getIntBoxOrder() {
		return intBoxOrder;
	}
	
	@Override
	public MaterialListBox getListBoxAreas() {
		return listBoxAreas;
	}
}
