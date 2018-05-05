package net.ionoff.center.client.base;

import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.InlineLabel;
import com.google.gwt.user.client.ui.Label;

import gwt.material.design.client.constants.ButtonType;
import gwt.material.design.client.constants.Color;
import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.constants.WavesType;
import gwt.material.design.client.ui.MaterialButton;
import gwt.material.design.client.ui.MaterialIcon;
import gwt.material.design.client.ui.MaterialTextBox;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.shared.dto.BaseDto;

public abstract class AbstractEditView<T extends BaseDto> extends FlowPanel implements IEditView<T> {
	
	protected MaterialIcon lblIcon;
	protected Label lblId;
	protected Label lblName;
	protected MaterialTextBox textBoxName;
	
	protected MaterialButton btnCancel;
	protected MaterialButton btnSave;
	protected MaterialButton btnClose;
	
	protected FlowPanel headerPanel;
	protected FlowPanel contentPanel;
	protected FlowPanel footerPanel;
	
	public AbstractEditView() {
		
		setStyleName("edit invisible");
		
		headerPanel = new FlowPanel();
		headerPanel.setStyleName("header");
		add(headerPanel);
		
		lblIcon = new MaterialIcon(); 
		lblIcon.setStyleName("icon");
		headerPanel.add(lblIcon);
		
		lblId = new InlineLabel(); 
		lblId.setStyleName("id");
		headerPanel.add(lblId);
		
		lblName = new InlineLabel(); 
		lblName.setStyleName("name");
		headerPanel.add(lblName);
		
		btnClose = new MaterialButton(ButtonType.FLAT);
		btnClose.addStyleName("close");
		btnClose.setIconType(IconType.CLOSE);
		btnClose.setWaves(WavesType.LIGHT);
		
		headerPanel.add(btnClose);
		
		contentPanel = new FlowPanel();
		contentPanel.setStyleName("body");
		add(contentPanel);
		
		textBoxName = new MaterialTextBox();
		textBoxName.setLabel(AdminLocale.getAdminConst().name());
		contentPanel.add(textBoxName);
		
		footerPanel = new FlowPanel();
		footerPanel.setStyleName("footer");
		add(footerPanel);
		
		btnSave = new MaterialButton(ButtonType.FLAT);
		btnSave.setWaves(WavesType.LIGHT);
		btnSave.setText(AdminLocale.getAdminConst().save());
		btnSave.setTextColor(Color.RED);
		btnSave.addStyleName("save");
		footerPanel.add(btnSave);
		
		btnCancel = new MaterialButton(ButtonType.FLAT);
		btnCancel.setText(AdminLocale.getAdminConst().close());
		btnCancel.setWaves(WavesType.LIGHT);
		btnCancel.addStyleName("save");
		btnCancel.setTextColor(Color.RED);
		footerPanel.add(btnCancel);
	}
	
	@Override
	public MaterialIcon getLblIcon() {
		return lblIcon;
	}

	@Override
	public Label getLblId() {
		return lblId;
	}

	@Override
	public Label getLblName() {
		return lblName;
	}

	@Override
	public MaterialTextBox getTextBoxName() {
		return textBoxName;
	}

	@Override
	public MaterialButton getBtnCancel() {
		return btnCancel;
	}

	@Override
	public MaterialButton getBtnSave() {
		return btnSave;
	}

	@Override
	public MaterialButton getBtnClose() {
		return btnClose;
	}

	@Override
	public FlowPanel getContentPanel() {
		return contentPanel;
	}
}
