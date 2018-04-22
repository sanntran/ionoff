package net.ionoff.center.client.area;

import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.ui.MaterialIntegerBox;
import net.ionoff.center.client.base.AbstractEditView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.shared.dto.AreaDto;

public class AreaEditView extends AbstractEditView<AreaDto> implements AreaEditPresenter.Display {
	
	private MaterialIntegerBox intBoxOrder;
	
	public AreaEditView() {
		super();
		getLblIcon().setIconType(IconType.BORDER_ALL);
		
		intBoxOrder = new MaterialIntegerBox();
		intBoxOrder.setLabel(AdminLocale.getAdminConst().order());
		contentPanel.add(intBoxOrder);
	}
	
	@Override
	public MaterialIntegerBox getIntBoxOrder() {
		return intBoxOrder;
	}
}
