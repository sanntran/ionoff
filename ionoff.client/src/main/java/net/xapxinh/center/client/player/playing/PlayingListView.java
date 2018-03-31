package net.xapxinh.center.client.player.playing;

import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.PopupPanel;
import com.google.gwt.user.client.ui.TextBox;

import net.xapxinh.center.client.player.locale.PlayLocale;

public class PlayingListView extends FlowPanel implements PlayingListPresenter.Display {
	
	private final Button btnIcon;
	private final Label lblTitle;
	private final Button btnSave;
	private final Button btnSaveAs;
	private final Button btnPlayMode;
	private final Button btnRandom;
	private final Button btnRepeat;
	private final Button btnLoop;
	private final Button btnEmpty;
	private final PopupPanel popupPlayModes;
	private final PopupPanel popupSavePlaylist;
	private final FlowPanel popupSavePlaylistWrapper;
	private final TextBox textBoxName;
	private final Button btnOkSave;
	
	private final FlowPanel nodeContainer;

	public PlayingListView() {
		setStyleName("playlist");
		setSize("100%", "100%");

		FlowPanel playlistBar = new FlowPanel();
		playlistBar.setStyleName("header");
		add(playlistBar);		
		
		btnIcon = new Button("");
		btnIcon.addStyleName("icon");
		playlistBar.add(btnIcon);
		
		lblTitle = new Label(PlayLocale.getPlayConsts().playing());
		playlistBar.add(lblTitle);
		lblTitle.setStyleName("title");
		
		btnEmpty = new Button("");
		btnEmpty.addStyleName("empty");
		playlistBar.add(btnEmpty);

		btnPlayMode = new Button("");
		btnPlayMode.addStyleName("playMode");
		playlistBar.add(btnPlayMode);
		
		btnSaveAs = new Button("");
		btnSaveAs.addStyleName("saveAs");
		playlistBar.add(btnSaveAs);
		
		btnSave = new Button("");
		btnSave.addStyleName("save");
		playlistBar.add(btnSave);
		
		nodeContainer = new FlowPanel();
		nodeContainer.setStyleName("wrapper");
		add(nodeContainer);
		nodeContainer.setWidth("100%");
		
		popupPlayModes = new PopupPanel(true, true);
		popupPlayModes.setStyleName("popup playModes");
		
		FlowPanel container = new FlowPanel();
		container.setStyleName("wrapper");
		popupPlayModes.setWidget(container);
		
		btnRandom = new Button(PlayLocale.getPlayConsts().random());
		container.add(btnRandom);
		btnRandom.addStyleName("mode random");

		btnRepeat = new Button(PlayLocale.getPlayConsts().repeat());
		container.add(btnRepeat);
		btnRepeat.addStyleName("mode repeat");

		btnLoop = new Button(PlayLocale.getPlayConsts().loop());
		container.add(btnLoop);
		btnLoop.addStyleName("mode loop");
		
		popupSavePlaylist = new PopupPanel(true, true);
		popupSavePlaylist.setStyleName("popup savePlaylist");
		
		popupSavePlaylistWrapper = new FlowPanel();
		popupSavePlaylistWrapper.setStyleName("wrapper");
		popupSavePlaylist.setWidget(popupSavePlaylistWrapper);
		
		textBoxName = new TextBox();
		textBoxName.setText(PlayLocale.getPlayConsts().playlist());
		popupSavePlaylistWrapper.add(textBoxName);
		btnRandom.addStyleName("textBoxName");

		btnOkSave = new Button(PlayLocale.getPlayConsts().ok());
		popupSavePlaylistWrapper.add(btnOkSave);
		btnOkSave.addStyleName("okSave");
	}

	@Override
	public FlowPanel asWidget() {
		return this;
	}

	@Override
	public FlowPanel getNodeContainer() {
		return this.nodeContainer;
	}

	@Override
	public Button getBtnIcon() {
		return this.btnIcon;
	}
	
	@Override
	public Label getLblTitle() {
		return this.lblTitle;
	}
	
	@Override
	public Button getBtnPlayMode() {
		return this.btnPlayMode;
	}
	
	@Override
	public Button getBtnEmpty() {
		return this.btnEmpty;
	}
	
	@Override
	public Button getBtnSave() {
		return this.btnSave;
	}
	
	@Override
	public Button getBtnSaveAs() {
		return this.btnSaveAs;
	}
	
	@Override
	public Button getBtnOkSave() {
		return this.btnOkSave;
	}
	
	@Override
	public TextBox getTextBoxName() {
		return this.textBoxName;
	}
	
	@Override
	public Button getBtnRepeat() {
		return this.btnRepeat;
	}
	
	@Override
	public Button getBtnRandom() {
		return this.btnRandom;
	}
	
	@Override
	public Button getBtnLoop() {
		return this.btnLoop;
	}
	
	@Override
	public PopupPanel getPopupPlayModes() {
		return this.popupPlayModes;
	}
	
	@Override
	public PopupPanel getPopupSavePlaylist() {
		return this.popupSavePlaylist;
	}

	@Override
	public FlowPanel getPopupSavePlaylistWrapper() {
		return popupSavePlaylistWrapper;
	}
}
