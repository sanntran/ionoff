package net.ionoff.center.client.sensor;

import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.Label;

import gwt.material.design.client.ui.MaterialListBox;

public class ModeSensorSceneView extends FlowPanel implements ModeSensorScenePresenter.Display {
	
	private final Label lblZoneIcon;
	private final Label lblZoneName;
	private final MaterialListBox listBoxScenes;
	
	public ModeSensorSceneView() {
		setStyleName("modeSensorScene");
		
		lblZoneIcon = new Label();
		lblZoneIcon.setStyleName("zoneIcon");
		add(lblZoneIcon);
		
		lblZoneName = new Label();
		lblZoneName.setStyleName("zoneName");
		add(lblZoneName);
		
		listBoxScenes = new MaterialListBox();
		listBoxScenes.addStyleName("listBox");
		add(listBoxScenes);
	}

	@Override
	public Label getLblZoneName() {
		return lblZoneName;
	}

	@Override
	public MaterialListBox getListBoxScenes() {
		return listBoxScenes;
	}
}
