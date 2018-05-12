package net.ionoff.center.client.sensor;

import com.google.gwt.dom.client.Style.Float;
import com.google.gwt.user.client.ui.FlowPanel;

import gwt.material.design.client.constants.IconSize;
import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.ui.MaterialCollapsible;
import gwt.material.design.client.ui.MaterialCollapsibleBody;
import gwt.material.design.client.ui.MaterialCollapsibleHeader;
import gwt.material.design.client.ui.MaterialCollapsibleItem;
import gwt.material.design.client.ui.MaterialIcon;
import gwt.material.design.client.ui.MaterialLabel;
import gwt.material.design.client.ui.MaterialTextBox;
import net.ionoff.center.client.locale.AdminLocale;

public class ModeSensorView extends MaterialCollapsible 
				implements ModeSensorPresenter.Display {
	
	private final MaterialIcon iconEnabled;
	private final MaterialIcon iconDelete;
	private final MaterialLabel lblModeName;
	private final MaterialLabel lblCondition;
	
	private final MaterialTextBox textBoxCondition;
	private final FlowPanel modeSensorScenesPanel;
	private final FlowPanel modeSensorUsersPanel;
	
	public ModeSensorView() {
		setStyleName("modeSensor");
		
		MaterialCollapsibleItem collapsibleItem = new MaterialCollapsibleItem();
		MaterialCollapsibleHeader collapsibleHeader = new MaterialCollapsibleHeader();
		collapsibleItem.add(collapsibleHeader);
		
		lblModeName = new MaterialLabel();
		lblModeName.setFontSize("15px");
		collapsibleHeader.add(lblModeName);
		
		lblCondition = new MaterialLabel();
		lblCondition.addStyleName("condition");
		collapsibleHeader.add(lblCondition);
		
		iconEnabled = new MaterialIcon();
		iconEnabled.setFloat(Float.LEFT);
		iconEnabled.setIconType(IconType.CHECK);
		iconEnabled.setIconSize(IconSize.MEDIUM);
		collapsibleHeader.add(iconEnabled);
		
		iconDelete = new MaterialIcon();
		iconDelete.setIconType(IconType.DELETE);
		iconDelete.setIconSize(IconSize.MEDIUM);
		iconDelete.setFloat(Float.RIGHT);
		collapsibleHeader.add(iconDelete);
		
		MaterialCollapsibleBody collapsibleBody = new MaterialCollapsibleBody();
		collapsibleItem.add(collapsibleBody);
		
		textBoxCondition = new MaterialTextBox();
		textBoxCondition.setLabel(AdminLocale.getAdminConst().condition());
		textBoxCondition.setPlaceholder(AdminLocale.getAdminConst().condition());
		collapsibleBody.add(textBoxCondition);
		
		modeSensorScenesPanel = new FlowPanel();
		modeSensorScenesPanel.setStyleName("modeSensorScenes");
		collapsibleBody.add(modeSensorScenesPanel);
		
		modeSensorUsersPanel = new FlowPanel();
		modeSensorUsersPanel.setStyleName("modeSensorUsers");
		collapsibleBody.add(modeSensorUsersPanel);
		
		add(collapsibleItem);
	}

	@Override
	public MaterialCollapsible asPanel() {
		return this;
	}

	@Override
	public MaterialIcon getIconEnabled() {
		return iconEnabled;
	}

	@Override
	public MaterialIcon getBtnDelete() {
		return iconDelete;
	}

	@Override
	public MaterialLabel getLblModeName() {
		return lblModeName;
	}

	@Override
	public MaterialLabel getLblCondition() {
		return lblCondition;
	}

	@Override
	public MaterialTextBox getTextBoxCondition() {
		return textBoxCondition;
	}

	@Override
	public FlowPanel getModeSensorScenesPanel() {
		return modeSensorScenesPanel;
	}

	@Override
	public FlowPanel getModeSensorUsersPanel() {
		return modeSensorUsersPanel;
	}
	
}
