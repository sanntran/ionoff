package net.ionoff.center.client.user;

import com.google.gwt.user.client.ui.CheckBox;
import com.google.gwt.user.client.ui.InlineLabel;
import com.google.gwt.user.client.ui.Label;

import gwt.material.design.client.ui.MaterialCollectionItem;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.UserProjectDto;

public class UserProjectView extends MaterialCollectionItem {

	private final CheckBox checkBoxRole;
	private final Label lblDescription;
	private final UserProjectDto userProject;

	public UserProjectView(UserProjectDto userProject) {
		this.userProject = userProject;
		addStyleName("userProject");
		checkBoxRole = new CheckBox(BaseDto.formatNameID(
				userProject.getProjectName(), userProject.getProjectId()));
		add(checkBoxRole);
		checkBoxRole.setValue(userProject.hasRole());
		
		lblDescription = new InlineLabel(userProject.getProjectDesc());
		add(lblDescription);
	}

	public UserProjectDto getUserProject() {
		return userProject;
	}

	public CheckBox getCheckBoxRole() {
		return this.checkBoxRole;
	}

	public int getCheckBoxValue() {
		return this.checkBoxRole.getValue() == true ? 1 : 0;
	}
}