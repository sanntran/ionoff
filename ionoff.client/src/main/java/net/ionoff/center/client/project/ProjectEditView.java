package net.ionoff.center.client.project;

import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.ui.MaterialTextBox;
import net.ionoff.center.client.common.AbstractEditView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.shared.dto.ProjectDto;

public class ProjectEditView extends AbstractEditView<ProjectDto> implements ProjectEditPresenter.Display {
	
	private MaterialTextBox textBoxAddress;
	
	public ProjectEditView() {
		super();
		getLblIcon().setIconType(IconType.BUSINESS);
		
		textBoxAddress = new MaterialTextBox();
		textBoxAddress.setLabel(AdminLocale.getAdminConst().address());
		contentPanel.add(textBoxAddress);
	}

	@Override
	public MaterialTextBox getTextBoxAddress() {
		return textBoxAddress;
	}

}
