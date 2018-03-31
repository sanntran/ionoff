package net.xapxinh.center.client.player.menu;

import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.FocusPanel;
import com.google.gwt.user.client.ui.Label;

public class MenuItemView extends FocusPanel {
	private final FlowPanel container;
	private final Label lblIcon;
	private final Label lblText;
	private boolean selected;

	public MenuItemView() {
		setStyleName("menuItem");
		container = new FlowPanel();
		setWidget(container);
		
		lblIcon = new Label("");
		container.add(lblIcon);
		
		lblText = new Label("");
		lblText.setStyleName("name");
		container.add(lblText);
	}

	public boolean isSelected() {
		return selected;
	}
	
	public void setSelected(boolean selected) {
		this.selected = selected;
		if (selected) {
			setStyleName("menuItem selected");
		}
		else {
			setStyleName("menuItem");
		}
	}

	public FlowPanel getContainer() {
		return container;
	}
	
	public void setText(String string) {
		lblText.setText(string);
	}

	public void setIconStyleName(String style) {
		lblIcon.setStyleName(style);
	}
}