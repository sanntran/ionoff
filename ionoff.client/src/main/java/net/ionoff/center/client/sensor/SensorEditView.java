package net.ionoff.center.client.sensor;

import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.InlineLabel;
import com.google.gwt.user.client.ui.Label;

import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.ui.MaterialButton;
import gwt.material.design.client.ui.MaterialIntegerBox;
import gwt.material.design.client.ui.MaterialListBox;
import net.ionoff.center.client.base.AbstractEditView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.locale.ProjectLocale;
import net.ionoff.center.shared.dto.SensorDto;

public class SensorEditView extends AbstractEditView<SensorDto> implements SensorEditPresenter.Display {
	
	private MaterialListBox listBoxGateways;
	private MaterialIntegerBox intBoxInputIndex;
	private FlowPanel actionPanel;
	private MaterialListBox listBoxModes;
	private MaterialButton btnAddModeAction;
	private FlowPanel addActionPanel;
	
	public SensorEditView() {
		super();
		getLblIcon().setIconType(IconType.WIFI_TETHERING);
		
		listBoxGateways = new MaterialListBox();
		listBoxGateways.setPlaceholder(AdminLocale.getAdminConst().gateway());
		contentPanel.add(listBoxGateways);
		
		intBoxInputIndex = new MaterialIntegerBox();
		intBoxInputIndex.setMin("0");
		intBoxInputIndex.setLabel(AdminLocale.getAdminConst().input());
		intBoxInputIndex.setPlaceholder(AdminLocale.getAdminConst().input());
		contentPanel.add(intBoxInputIndex);

		Label lblAction = new InlineLabel(AdminLocale.getAdminConst().action());
		lblAction.setStyleName("lbl");
		contentPanel.add(lblAction);
		
		actionPanel = new FlowPanel();
		contentPanel.add(actionPanel);
		
		addActionPanel = new FlowPanel();
		addActionPanel.addStyleName("addModeSensor");
		
		listBoxModes = new MaterialListBox();
		listBoxModes.setPlaceholder(AdminLocale.getAdminConst().mode());
		addActionPanel.add(listBoxModes);
		
		btnAddModeAction = new MaterialButton(ProjectLocale.getProjectConst().add());
		
		addActionPanel.add(btnAddModeAction);
		
		contentPanel.add(addActionPanel);
	}

	@Override
	public MaterialListBox getListBoxGateways() {
		return listBoxGateways;
	}
	
	@Override
	public MaterialIntegerBox getIntBoxInputIndex() {
		return intBoxInputIndex;
	}

	@Override
	public FlowPanel getPanelActions() {
		return actionPanel;
	}

	@Override
	public MaterialListBox getListBoxModes() {
		return listBoxModes;
	}

	@Override
	public MaterialButton getBtnAddModeAction() {
		return btnAddModeAction;
	}

	@Override
	public FlowPanel getPanelAddAction() {
		return addActionPanel;
	}
}
