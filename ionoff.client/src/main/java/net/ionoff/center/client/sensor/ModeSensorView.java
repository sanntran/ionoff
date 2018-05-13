package net.ionoff.center.client.sensor;

import com.google.gwt.dom.client.Style.Float;
import com.google.gwt.user.client.ui.FlowPanel;

import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.constants.WavesType;
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
	private final MaterialLabel lblModeName;
	private final MaterialLabel lblCondition;
	
	private final MaterialIcon iconDelete;
	private final MaterialTextBox textBoxCondition;
	private final MaterialTextBox textBoxMessage;
	private final FlowPanel modeSensorScenesPanel;
	private final FlowPanel modeSensorUsersPanel;
	
	public ModeSensorView() {
		setStyleName("modeSensor");
		
		MaterialCollapsibleItem collapsibleItem = new MaterialCollapsibleItem();
		MaterialCollapsibleHeader collapsibleHeader = new MaterialCollapsibleHeader();
		collapsibleItem.add(collapsibleHeader);
		
		iconDelete = new MaterialIcon();
		iconDelete.addStyleName("icon delete");
		iconDelete.setWaves(WavesType.DEFAULT);
		iconDelete.setIconType(IconType.DELETE);
		iconDelete.setFloat(Float.RIGHT);
		add(iconDelete);
		
		iconEnabled = new MaterialIcon();
		iconEnabled.addStyleName("icon enable");
		iconEnabled.setWaves(WavesType.DEFAULT);
		iconEnabled.setFloat(Float.RIGHT);
		iconEnabled.setIconType(IconType.CHECK);
		add(iconEnabled);
		
		lblModeName = new MaterialLabel();
		lblModeName.setFontSize("15px");
		collapsibleHeader.add(lblModeName);
		
		lblCondition = new MaterialLabel();
		lblCondition.addStyleName("condition");
		collapsibleHeader.add(lblCondition);
		
		MaterialCollapsibleBody collapsibleBody = new MaterialCollapsibleBody();
		collapsibleItem.add(collapsibleBody);
		
		textBoxCondition = new MaterialTextBox();
		textBoxCondition.setLabel(AdminLocale.getAdminConst().condition() 
							+ " " + AdminLocale.getAdminMessages().xIsSensorValue());
		textBoxCondition.setPlaceholder(AdminLocale.getAdminConst().conditionExample());
		collapsibleBody.add(textBoxCondition);
		
		textBoxMessage = new MaterialTextBox();
		textBoxMessage.setLabel(AdminLocale.getAdminConst().message());
		textBoxMessage.setPlaceholder(AdminLocale.getAdminConst().message());
		collapsibleBody.add(textBoxMessage);

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
	public MaterialTextBox getTextBoxMesage() {
		return textBoxMessage;
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
