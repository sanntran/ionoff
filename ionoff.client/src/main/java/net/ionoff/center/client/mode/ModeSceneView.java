package net.ionoff.center.client.mode;

import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.Label;

import gwt.material.design.client.ui.MaterialListBox;

public class ModeSceneView extends FlowPanel implements ModeScenePresenter.Display {
	
	private final Label lblZoneIcon;
	private final Label lblZoneName;
	private final MaterialListBox listBoxScenes;
	
	public ModeSceneView() {
		
		setStyleName("modeScene");
		
		lblZoneIcon = new Label();
		lblZoneIcon.setStyleName("zoneIcon");
		add(lblZoneIcon);
		
		lblZoneName = new Label();
		lblZoneName.addStyleName("zoneName");
		add(lblZoneName);
		
		listBoxScenes = new MaterialListBox();
		listBoxScenes.setStyleName("listBox");
		add(listBoxScenes);
	}

	@Override
	public Label getLblAreaName() {
		return lblZoneName;
	}

	@Override
	public MaterialListBox getListBoxScenes() {
		return listBoxScenes;
	}

	@Override
	public FlowPanel asPanel() {
		return this;
	}
}
