package net.ionoff.center.client.relay;

import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.InlineLabel;
import com.google.gwt.user.client.ui.Label;

import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.ui.MaterialListBox;
import gwt.material.design.client.ui.MaterialTextBox;
import net.ionoff.center.client.base.AbstractEditView;
import net.ionoff.center.client.ui.DevicesSelectionPanel;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.shared.dto.RelayDto;

public class RelayEditView extends AbstractEditView<RelayDto> implements RelayEditPresenter.Display {
	
	private MaterialTextBox textBoxRelayDriver;
	private MaterialTextBox textBoxIndex;
	private MaterialListBox listBoxTypes;
	private DevicesSelectionPanel devicesSelectionPanel;
	private RelayGroupView relayGroupView;
	private RelaySelectionView relaySelectionView;
	
	public RelayEditView() {
		super();
		addStyleName("row");
		getLblIcon().setIconType(IconType.POWER_SETTINGS_NEW);
		
		FlowPanel panel = new FlowPanel();
		panel.setHeight("65px");
		panel.addStyleName("row");
		contentPanel.add(panel);
		
		textBoxRelayDriver = new MaterialTextBox();
		textBoxRelayDriver.setEnabled(false);
		textBoxRelayDriver.addStyleName("col s6 no-padding");
		textBoxRelayDriver.setLabel(AdminLocale.getAdminConst().relayDriver());
		panel.add(textBoxRelayDriver);
		
		textBoxIndex = new MaterialTextBox();
		textBoxIndex.setEnabled(false);
		textBoxIndex.addStyleName("col s6 no-padding");
		textBoxIndex.setLabel(AdminLocale.getAdminConst().index());
		panel.add(textBoxIndex);
		
		listBoxTypes = new MaterialListBox();
		listBoxTypes.setPlaceholder(AdminLocale.getAdminConst().type());
		listBoxTypes.addItem(AdminLocale.getAdminConst().switch_());
		listBoxTypes.addItem(AdminLocale.getAdminConst().button());
		contentPanel.add(listBoxTypes);
		
		devicesSelectionPanel = new DevicesSelectionPanel();
		contentPanel.add(devicesSelectionPanel);
		
		Label lblGroup = new InlineLabel(AdminLocale.getAdminConst().relayGroup());
		lblGroup.setStyleName("lbl");
		contentPanel.add(lblGroup);
			
		relayGroupView = new RelayGroupView();
		contentPanel.add(relayGroupView);
		
		relaySelectionView = new RelaySelectionView();
		contentPanel.add(relaySelectionView);
	}

	@Override
	public MaterialTextBox getTextBoxRelayDriver() {
		return textBoxRelayDriver;
	}
	
	@Override
	public MaterialTextBox getTextBoxIndex() {
		return textBoxIndex;
	}
	
	@Override
	public MaterialListBox getListBoxTypes() {
		return listBoxTypes;
	}
	
	@Override
	public DevicesSelectionPanel getDevicesSelectionPanel() {
		return devicesSelectionPanel;
	}
	
	@Override
	public RelayGroupView getRelayGroupView() {
		return relayGroupView;
	}

	@Override
	public RelaySelectionView getRelaySelectionView() {
		return relaySelectionView;
	}
}
