package net.ionoff.center.client.navigation;

import gwt.material.design.client.constants.ButtonType;
import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.constants.WavesType;
import gwt.material.design.client.ui.MaterialButton;
import gwt.material.design.client.ui.MaterialIcon;
import gwt.material.design.client.ui.MaterialTitle;
import net.ionoff.center.shared.dto.ProjectDto;

public class ProjectMenuItemView extends MaterialButton {
	
	private MaterialTitle lblTitle;
	private boolean selected;
	private final ProjectDto project;
	
	public ProjectMenuItemView(ProjectDto proj) {
		this.project = proj;
		setType(ButtonType.FLAT);
		setWaves(WavesType.LIGHT);
		addStyleName("project");
		MaterialIcon icon = new MaterialIcon();
		icon.setIconType(IconType.BORDER_OUTER);
		add(icon);
		lblTitle = new MaterialTitle();
		add(lblTitle);
		lblTitle.setTitle(proj.getName());
		lblTitle.setDescription(proj.getAddress());
	}
	
	@Override
	public ProjectMenuItemView asWidget() {
		return this;
	}
	
	public void setSelected(boolean selected) {
		this.selected = selected;
		removeStyleName("selected");
		if (selected) {
			addStyleName("selected");
		}
	}
	
	public boolean isSelected() {
		return this.selected;
	}

	public ProjectDto getProject() {
		return project;
	}
}
