package net.ionoff.center.client.mode;

import com.google.gwt.user.client.ui.CheckBox;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.Label;

import gwt.material.design.client.constants.ButtonType;
import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.constants.WavesType;
import gwt.material.design.client.ui.MaterialButton;
import gwt.material.design.client.ui.MaterialIntegerBox;
import gwt.material.design.client.ui.MaterialListBox;
import net.ionoff.center.client.locale.AdminLocale;

public class ModeSensorsSettingView extends FlowPanel implements ModeSensorsSettingPresenter.Display {
	
	private final MaterialListBox listBoxSensors;
	
	private final FlowPanel modeSensorSettingPanel;
	private final CheckBox checkBoxEnabled;
	private final MaterialIntegerBox intBoxTimeBuffer;
	private final MaterialButton btnSaveTimeBuffer;
	
	private final Label lblTabDetectedHuman;
	private final Label lblTabDetectedNoHuman;
	private final FlowPanel sensorScenesPanel;
	private final FlowPanel sensorUsersPanel;
	
	public ModeSensorsSettingView() {
		setStyleName("modeSensors");
		
		listBoxSensors = new MaterialListBox();
		listBoxSensors.addStyleName("listBox");
		add(listBoxSensors);
		
		modeSensorSettingPanel = new FlowPanel();
		modeSensorSettingPanel.setStyleName("sensorSetting");
		add(modeSensorSettingPanel);
		
		FlowPanel modeSensorDetailPanel = new FlowPanel();
		modeSensorDetailPanel.setStyleName("detail");
		modeSensorSettingPanel.add(modeSensorDetailPanel);
		
		checkBoxEnabled = new CheckBox(AdminLocale.getAdminConst().enabled());
		checkBoxEnabled.addStyleName("checkBox");
		modeSensorDetailPanel.add(checkBoxEnabled);
		
		intBoxTimeBuffer = new MaterialIntegerBox();
		intBoxTimeBuffer.setPlaceholder(AdminLocale.getAdminConst().timeBuffer());
		modeSensorDetailPanel.add(intBoxTimeBuffer);
		
		btnSaveTimeBuffer = new MaterialButton(ButtonType.FLAT);
		btnSaveTimeBuffer.addStyleName("saveTimeBuffer");
		btnSaveTimeBuffer.setIconType(IconType.SAVE);
		btnSaveTimeBuffer.setWaves(WavesType.DEFAULT);
		modeSensorDetailPanel.add(btnSaveTimeBuffer);
		
		FlowPanel sensorScenesTabs = new FlowPanel();
		sensorScenesTabs.setStyleName("tabsPanel");
		modeSensorSettingPanel.add(sensorScenesTabs);
		
		lblTabDetectedHuman = new Label(AdminLocale.getAdminConst().detectedHuman());
		lblTabDetectedHuman.setStyleName("lblTab");
		sensorScenesTabs.add(lblTabDetectedHuman);
		
		lblTabDetectedNoHuman = new Label(AdminLocale.getAdminConst().detectedNoHuman());
		lblTabDetectedNoHuman.setStyleName("lblTab");
		sensorScenesTabs.add(lblTabDetectedNoHuman);
		
		sensorScenesPanel = new FlowPanel();
		sensorScenesPanel.setStyleName("sensorScenes");
		modeSensorSettingPanel.add(sensorScenesPanel);
		
		sensorUsersPanel = new FlowPanel();
		sensorUsersPanel.setStyleName("sensorUsers");
		modeSensorSettingPanel.add(sensorUsersPanel);
	}

	@Override
	public FlowPanel asPanel() {
		return this;
	}
	
	@Override
	public MaterialListBox getListBoxSensors() { 
		return listBoxSensors; 
	}
	
	@Override
	public CheckBox getCheckBoxEnabled() { 
		return this.checkBoxEnabled; 
	}
	
	@Override
	public MaterialIntegerBox getIntBoxTimeBuffer() { 
		return this.intBoxTimeBuffer; 
	}
	
	@Override
	public MaterialButton getBtnSaveTimeBuffer() { 
		return this.btnSaveTimeBuffer; 
	}
	
	@Override
	public Label getLblTabDetectedHuman() { 
		return this.lblTabDetectedHuman; 
	}
	
	@Override
	public Label getLblTabDetectedNoHuman() { 
		return this.lblTabDetectedNoHuman; 
	}
	
	@Override
	public FlowPanel getSensorScenesPanel() { 
		return this.sensorScenesPanel; 
	}

	@Override
	public FlowPanel getModeSensorSettingPanel() {
		return modeSensorSettingPanel;
	}

	@Override
	public FlowPanel getSensorUsersPanel() {
		return sensorUsersPanel;
	}
}
