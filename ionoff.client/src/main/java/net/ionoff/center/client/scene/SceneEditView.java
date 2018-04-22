package net.ionoff.center.client.scene;

import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.InlineLabel;
import com.google.gwt.user.client.ui.Label;

import gwt.material.design.addins.client.combobox.MaterialComboBox;
import gwt.material.design.client.constants.IconType;
import net.ionoff.center.client.base.AbstractEditView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.shared.dto.SceneDto;

public class SceneEditView extends AbstractEditView<SceneDto> implements SceneEditPresenter.Display {
	
	private MaterialComboBox<String> listBoxZones;
	private FlowPanel panelSceneDevices;
	
	public SceneEditView() {
		super();
		getLblIcon().setIconType(IconType.SLIDESHOW);
		Label lblArea = new InlineLabel(AdminLocale.getAdminConst().zone());
		lblArea.setStyleName("lbl");
		contentPanel.add(lblArea);
		listBoxZones = new MaterialComboBox<String>();
		contentPanel.add(listBoxZones);
		
		Label lblAction = new InlineLabel(AdminLocale.getAdminConst().action());
		lblAction.setStyleName("lbl");
		contentPanel.add(lblAction);
		
		panelSceneDevices = new FlowPanel();
		panelSceneDevices.setStyleName("sceneDevices");
		contentPanel.add(panelSceneDevices);
	}
	
	@Override
	public MaterialComboBox<String> getListBoxZones() {
		return listBoxZones;
	}
	
	@Override
	public FlowPanel getSceneDevicesView() {
		return panelSceneDevices;
	}
}
