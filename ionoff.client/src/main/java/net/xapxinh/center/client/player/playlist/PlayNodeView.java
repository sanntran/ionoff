package net.xapxinh.center.client.player.playlist;

import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.FocusPanel;
import com.google.gwt.user.client.ui.Image;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.Widget;

import net.xapxinh.center.shared.dto.PlayNodeDto;

public class PlayNodeView extends FlowPanel implements PlayNodePresenter.Display {
	
	private FocusPanel focusPanel;
	private Image imgIcon;
	private Label lblName;
	private Label lblIcon;
	private Button btnPlay;
	private Button btnEnqueue;
	private Button btnRemove;
	
	private final FlowPanel leafContainer;
	
	public PlayNodeView(final PlayNodeDto playlistNode) {
		String style = "node";
		if (playlistNode.isCurrent()) {
			style = "node current";
		}
		setStyleName(style);
		leafContainer = new FlowPanel();
		
		if (playlistNode.hasManyLeaf()) {
			FlowPanel nodePanel = new FlowPanel();
			nodePanel.setStyleName("control");
			add(nodePanel);
			
			focusPanel = new FocusPanel();
			nodePanel.add(focusPanel);
			focusPanel.setStyleName("focus");
			
			FlowPanel iconNamePanel = new FlowPanel();
			focusPanel.setWidget(iconNamePanel);
			
			if (playlistNode.isAlbum()) {
				imgIcon = new Image();
				if (playlistNode.getImage() == null) {
					imgIcon.setUrl("styles/images/play/cd.png");
				}
				else {
					imgIcon.setUrl(playlistNode.getImage());
				}
				imgIcon.setStyleName("img");
				iconNamePanel.add(imgIcon);
			}
			else {
				lblIcon = new Label();
				lblIcon.setStyleName("icon " + playlistNode.getType());
				iconNamePanel.add(lblIcon);
			}
			
			lblName = new Label(playlistNode.getName());
			lblName.setStyleName("title");
			iconNamePanel.add(lblName);
			
			FlowPanel panel = new FlowPanel();
			nodePanel.add(panel);
			
			btnPlay = new Button();
			btnPlay.addStyleName("play");
			panel.add(btnPlay);
			
			btnEnqueue = new Button();
			btnEnqueue.addStyleName("enqueue");
			panel.add(btnEnqueue);

			btnRemove = new Button();
			btnRemove.addStyleName("remove");
			panel.add(btnRemove);
			
			leafContainer.setVisible(false);
		}
		add(leafContainer);
	}
	
	@Override
	public FlowPanel asPanel() {
		return this;
	}
	
	@Override
	public FlowPanel getLeafContainer() {
		return this.leafContainer;
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
	public Widget asWidget() {
		return this;
	}
	@Override
	public FocusPanel getFocusPanel() {
		return this.focusPanel;
	}

}
