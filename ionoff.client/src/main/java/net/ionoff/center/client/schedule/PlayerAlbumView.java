package net.ionoff.center.client.schedule;

import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.Label;

import net.xapxinh.center.shared.dto.MediaFile;
import net.xapxinh.center.shared.dto.PlayerApi;

public class PlayerAlbumView extends FlowPanel {
	
	private final Button btnIcon;
	private final Label lblName;
	private final Button btnSelect;
	
	public PlayerAlbumView(MediaFile file) {
		setStyleName("panelAlbum");
		btnIcon = new Button();
		
		if (PlayerApi.DIR.equalsIgnoreCase(file.getType())) {
			btnIcon.addStyleName("button folder");
		}
		else if (file.isAlbum()) {
			btnIcon.addStyleName("button album");
		}
		else {
			btnIcon.addStyleName("button file");
		}
		add(btnIcon);
		
		lblName = new Label();
		lblName.setStyleName("name");
		lblName.setText(file.getDisplayName());
		add(lblName);
		
		btnSelect = new Button();
		btnSelect.addStyleName("button select");
		add(btnSelect);
		
		if (PlayerApi.PARENT.equals(file.getName())) {
			btnSelect.setVisible(false);
		}
	}

	public Button getBtnIcon() {
		return this.btnIcon;
	}
	
	public Label getLblName() {
		return this.lblName;
	}
	
	public Button getBtnSelect() {
		return this.btnSelect;
	}
}
