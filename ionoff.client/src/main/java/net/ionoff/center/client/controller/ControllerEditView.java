package net.ionoff.center.client.controller;

import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.ui.MaterialIntegerBox;
import gwt.material.design.client.ui.MaterialListBox;
import gwt.material.design.client.ui.MaterialTextBox;
import net.ionoff.center.client.base.AbstractEditView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.shared.dto.ControllerDto;

public class ControllerEditView extends AbstractEditView<ControllerDto> implements ControllerEditPresenter.Display {
	
	private MaterialIntegerBox intBoxPort;
	private MaterialTextBox textBoxIp;
	private MaterialTextBox textBoxKey;
	private MaterialListBox listBoxModels;
	
	public ControllerEditView() {
		super();
		getLblIcon().setIconType(IconType.MEMORY);
		
		textBoxIp = new MaterialTextBox();
		textBoxIp.setLabel(AdminLocale.getAdminConst().ip());
		contentPanel.add(textBoxIp);
		
		intBoxPort = new MaterialIntegerBox();
		intBoxPort.setLabel(AdminLocale.getAdminConst().port());
		contentPanel.add(intBoxPort);
		
		
		textBoxKey = new MaterialTextBox();
		textBoxKey.setLabel(AdminLocale.getAdminConst().key());
		contentPanel.add(textBoxKey);
		
		listBoxModels = new MaterialListBox();
		listBoxModels.setPlaceholder(AdminLocale.getAdminConst().model());
		listBoxModels.addItem(ControllerModel.IONOFF_E3.toString());
		listBoxModels.addItem(ControllerModel.IONOFF_E4.toString());
		listBoxModels.addItem(ControllerModel.IONOFF_P4.toString());
		listBoxModels.addItem(ControllerModel.IONOFF_P8.toString());
		listBoxModels.addItem(ControllerModel.HLAB_EP2.toString());
		listBoxModels.addItem(ControllerModel.HBQ_EC100.toString());
		listBoxModels.setSelectedIndex(0);
		contentPanel.add(listBoxModels);
	}

	@Override
	public MaterialListBox getListBoxModels() {
		return listBoxModels;
	}

	@Override
	public MaterialTextBox getTextBoxKey() {
		return textBoxKey;
	}
	
	@Override
	public MaterialTextBox getTextBoxIp() {
		return textBoxIp;
	}
	
	@Override
	public MaterialIntegerBox getIntBoxPort() {
		return intBoxPort;
	}
}