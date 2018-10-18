package net.ionoff.center.client.relay;

import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.InlineLabel;
import com.google.gwt.user.client.ui.Label;

import gwt.material.design.client.constants.Color;
import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.constants.WavesType;
import gwt.material.design.client.ui.MaterialButton;
import gwt.material.design.client.ui.MaterialCheckBox;
import gwt.material.design.client.ui.MaterialIntegerBox;
import gwt.material.design.client.ui.MaterialTextBox;
import net.ionoff.center.client.base.AbstractEditView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.locale.ProjectLocale;
import net.ionoff.center.client.ui.DevicesSelectionPanel;
import net.ionoff.center.shared.dto.RelayDto;

public class RelayEditView extends AbstractEditView<RelayDto> implements RelayEditPresenter.Display {
	
	private MaterialCheckBox checkBoxLocked;
	private MaterialTextBox textBoxDriver;
	private MaterialTextBox textBoxIndex;
	private MaterialIntegerBox intBoxAutoRevert;
	private DevicesSelectionPanel devicesSelectionPanel;
	private final FlowPanel relayGroupListPanel;
	private final MaterialButton btnAddRelayGroup;
	
	public RelayEditView() {
		super();
		addStyleName("row");
		getLblIcon().setIconType(IconType.RADIO_BUTTON_UNCHECKED);
		
		FlowPanel panel = new FlowPanel();
		panel.setHeight("65px");
		panel.addStyleName("row");
		contentPanel.add(panel);
		
		textBoxDriver = new MaterialTextBox();
		textBoxDriver.setEnabled(false);
		textBoxDriver.addStyleName("col s6 no-padding");
		textBoxDriver.setLabel(AdminLocale.getAdminConst().controller());
		panel.add(textBoxDriver);
		
		textBoxIndex = new MaterialTextBox();
		textBoxIndex.setEnabled(false);
		textBoxIndex.addStyleName("col s6 no-padding");
		textBoxIndex.setLabel(AdminLocale.getAdminConst().index());
		panel.add(textBoxIndex);
		
		checkBoxLocked = new MaterialCheckBox();
		checkBoxLocked.addStyleName("lock");
		checkBoxLocked.setText(AdminLocale.getAdminConst().lock());
		checkBoxLocked.setValue(false);
		contentPanel.add(checkBoxLocked);
		
		intBoxAutoRevert = new MaterialIntegerBox();
		intBoxAutoRevert.setLabel(AdminLocale.getAdminConst().autoRevert());
		contentPanel.add(intBoxAutoRevert);
		
		devicesSelectionPanel = new DevicesSelectionPanel();
		contentPanel.add(devicesSelectionPanel);
		
		Label lblGroup = new InlineLabel(AdminLocale.getAdminConst().relayGroup());
		lblGroup.setStyleName("lbl");
		contentPanel.add(lblGroup);
			
		relayGroupListPanel = new FlowPanel();
		relayGroupListPanel.addStyleName("relayGroups");
		contentPanel.add(relayGroupListPanel);
		
		btnAddRelayGroup = new MaterialButton(ProjectLocale.getProjectConst().add() 
				+ " " + AdminLocale.getAdminConst().relayGroup());
		btnAddRelayGroup.setBackgroundColor(Color.WHITE);
		btnAddRelayGroup.setTextColor(Color.GREY_DARKEN_4);
		btnAddRelayGroup.setWaves(WavesType.DEFAULT);
		btnAddRelayGroup.addStyleName("add");
		contentPanel.add(btnAddRelayGroup);
	}
	
	@Override
	public MaterialCheckBox getCheckBoxLocked() {
		return checkBoxLocked;
	}

	@Override
	public MaterialTextBox getTextBoxDriver() {
		return textBoxDriver;
	}
	
	@Override
	public MaterialTextBox getTextBoxIndex() {
		return textBoxIndex;
	}
	
	@Override
	public MaterialIntegerBox getIntBoxAutoRevert() {
		return intBoxAutoRevert;
	}
	
	@Override
	public DevicesSelectionPanel getDevicesSelectionPanel() {
		return devicesSelectionPanel;
	}

	@Override
	public FlowPanel getRelayGroupListPanel() {
		return relayGroupListPanel;
	}

	@Override
	public MaterialButton getBtnAddGroup() {
		return btnAddRelayGroup;
	}
}
