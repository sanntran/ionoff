package net.ionoff.center.client.base;

import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.Widget;

import gwt.material.design.client.ui.MaterialButton;
import gwt.material.design.client.ui.MaterialIcon;
import gwt.material.design.client.ui.MaterialTextBox;

public interface IEditView<T> {
	
	Widget asWidget();
	
	MaterialIcon getLblIcon();
	
	Label getLblId();
	
	Label getLblName();
	
	MaterialTextBox getTextBoxName();
	
	MaterialButton getBtnCancel();
	
	MaterialButton getBtnSave();
	
	MaterialButton getBtnClose();
}
