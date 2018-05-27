package net.ionoff.center.client.device;

import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.ui.MaterialTextBox;
import net.ionoff.center.client.base.AbstractEditView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.shared.dto.SensorDataDto;

public class SensorDataEditView extends AbstractEditView<SensorDataDto> implements SensorDataEditPresenter.Display {

	private MaterialTextBox textBoxValue;
	private MaterialTextBox textBoxIndex;
	
	public SensorDataEditView() {
		super();
		getLblIcon().setIconType(IconType.DEVICES_OTHER);
		getTextBoxName().setVisible(false);
		
		textBoxValue = new MaterialTextBox();
		textBoxValue.setPlaceholder(AdminLocale.getAdminConst().value());
		textBoxValue.add(textBoxValue);
		
		textBoxIndex = new MaterialTextBox();
		textBoxIndex.setPlaceholder(AdminLocale.getAdminConst().index());
		contentPanel.add(textBoxIndex);
		
		getBtnSave().setVisible(false);
	}

	@Override
	public MaterialTextBox getTextBoxValue() {
		return textBoxValue;
	}

	@Override
	public MaterialTextBox getTextBoxIndex() {
		return textBoxIndex;
	}
}
