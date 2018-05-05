package net.ionoff.center.client.device;

import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.ui.MaterialCollection;
import gwt.material.design.client.ui.MaterialDoubleBox;
import gwt.material.design.client.ui.MaterialTextBox;
import net.ionoff.center.client.base.AbstractEditView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.shared.dto.SensorDataDto;

public class ScaleDataEditView extends AbstractEditView<SensorDataDto> implements ScaleDataEditPresenter.Display {

	private MaterialCollection dataCollection;

	public ScaleDataEditView() {
		super();
		getLblIcon().setIconType(IconType.DEVICES_OTHER);
		getTextBoxName().setVisible(false);
		dataCollection = new MaterialCollection();
		getContentPanel().add(dataCollection);
		getBtnSave().setVisible(false);
	}

	@Override
	public MaterialCollection getDataCollection() {
		return dataCollection;
	}
}
