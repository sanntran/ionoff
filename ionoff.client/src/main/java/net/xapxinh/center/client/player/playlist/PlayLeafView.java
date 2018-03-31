package net.xapxinh.center.client.player.playlist;

import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.Label;

import net.xapxinh.center.shared.dto.PlayLeafDto;

public class PlayLeafView extends FlowPanel implements PlayLeafPresenter.Display {
	
	private final Button btnPlay;
	private final Button btnEnqueue;
	private final Label lblName;
	private final Button btnRemove;
	private final Label lblIcon;
	
	public PlayLeafView(final PlayLeafDto playlistLeaf) {
		String style = "leaf";
		if (playlistLeaf.isCurrent()) {
			style = "leaf current";
		}
		setStyleName(style);
		
		lblIcon = new Label("");
		lblIcon.setStyleName("icon " + playlistLeaf.getType());
		add(lblIcon);
		
		lblName = new Label(playlistLeaf.getName());
		lblName.setStyleName("title");
		add(lblName);
		
		btnPlay = new Button("");
		btnPlay.addStyleName("play");
		add(btnPlay);
		
		btnEnqueue = new Button("");
		btnEnqueue.addStyleName("enqueue");
		add(btnEnqueue);

		btnRemove = new Button("");
		btnRemove.addStyleName("remove");
		btnRemove.setText("");
		add(btnRemove);
	}
	
	@Override
	public Button getBtnPlay() {
		return this.btnPlay;
	}
	
	@Override
	public Button getBtnEnqueue() {
		return this.btnEnqueue;
	}
	
	@Override
	public Label getLblName() {
		return this.lblName;
	}

	@Override
	public Button getBtnRemove() {
		return this.btnRemove;
	}
	
	@Override
	public FlowPanel asPanel() {
		return this;
	}
	@Override
	public Label getLblIcon() {
		return this.lblIcon;
	}
}
