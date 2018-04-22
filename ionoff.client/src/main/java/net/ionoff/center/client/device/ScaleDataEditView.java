package net.ionoff.center.client.device;

import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.ui.MaterialDoubleBox;
import gwt.material.design.client.ui.MaterialTextBox;
import net.ionoff.center.client.base.AbstractEditView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.shared.dto.SensorDataDto;

public class ScaleDataEditView extends AbstractEditView<SensorDataDto> implements ScaleDataEditPresenter.Display {

	private MaterialTextBox textBoxTime;
	private MaterialDoubleBox doubleBoxValue;

	public ScaleDataEditView() {
		super();
		getLblIcon().setIconType(IconType.DEVICES_OTHER);

		getTextBoxName().setPlaceholder(AdminLocale.getAdminConst().time());
		getTextBoxName().setLabel(AdminLocale.getAdminConst().time());

		doubleBoxValue = new MaterialDoubleBox();
		doubleBoxValue.setLabel(AdminLocale.getAdminConst().value());
		contentPanel.add(doubleBoxValue);
	}
	
	@Override
	public MaterialDoubleBox getDoubleBoxValue() {
		return doubleBoxValue;
	}
}
