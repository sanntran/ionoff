package net.xapxinh.center.client.player.playlist;

import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.TextBox;

import net.xapxinh.center.client.player.locale.PlayLocale;

public class PlaylistListView extends FlowPanel implements PlaylistListPresenter.Display {
 
	private final Label lblTitle;
	private final Button btnMyPlaylist;
	private final Button btnAllPlaylist;
	
	private final TextBox textBoxKey;
	private final Button btnSearch;
	private final Button btnShowMore;
	private final FlowPanel panelPlaylists;
	private final FlowPanel panelMyPlaylists;
	private final FlowPanel panelAllPlaylists;
	
	private final PlaylistEditView playlistEditView;

	public PlaylistListView() {
		setStyleName("albums playlists");

		final FlowPanel panelHeader = new FlowPanel();
		panelHeader.setStyleName("header");
		add(panelHeader);
		
		lblTitle = new Label(PlayLocale.getPlayConsts().playlist());
		panelHeader.add(lblTitle);
		lblTitle.addStyleName("title");
		
		btnAllPlaylist = new Button(PlayLocale.getPlayConsts().allPlaylist());
		panelHeader.add(btnAllPlaylist);
		btnAllPlaylist.addStyleName("tabBtn");
		
		btnMyPlaylist = new Button(PlayLocale.getPlayConsts().myPlaylist());
		panelHeader.add(btnMyPlaylist);
		btnMyPlaylist.addStyleName("tabBtn selected");

		panelPlaylists = new FlowPanel();
		panelPlaylists.setStyleName("playlistsWrapper row");
		add(panelPlaylists);
		
		panelMyPlaylists = new FlowPanel();
		panelMyPlaylists.setStyleName("albumList playlistList row");
		panelPlaylists.add(panelMyPlaylists);
		
		panelAllPlaylists = new FlowPanel();
		panelAllPlaylists.setStyleName("albumList playlistList");
		
		final FlowPanel panelSearch = new FlowPanel();
		panelSearch.setStyleName("header");
		
		textBoxKey = new TextBox();
		textBoxKey.addStyleName("textbox");
		panelSearch.add(textBoxKey);

		btnSearch = new Button("");
		btnSearch.setText("");
		panelSearch.add(btnSearch);
		btnSearch.addStyleName("search");
		panelAllPlaylists.add(panelSearch);
		
		// Paging
		btnShowMore = new Button(PlayLocale.getPlayConsts().showMore());
		btnShowMore.setStyleName("showMore");
		btnShowMore.setVisible(false);
		
		playlistEditView = new PlaylistEditView();
	}

	@Override
	public FlowPanel asWidget() {
		return this;
	}

	@Override
	public FlowPanel getPanelPlaylists() {
		return this.panelPlaylists;
	}
	
	@Override
	public FlowPanel getPanelMyPlaylists() {
		return this.panelMyPlaylists;
	}
	
	@Override
	public FlowPanel getPanelAllPlaylists() {
		return this.panelAllPlaylists;
	}

	@Override
	public TextBox getTextBoxKey() {
		return this.textBoxKey;
	}

	@Override
	public Button getBtnSearch() {
		return this.btnSearch;
	}
	
	@Override
	public Button getBtnShowMore() {
		return this.btnShowMore;
	}
	
	@Override
	public Button getBtnMyPlaylistTab() {
		return this.btnMyPlaylist;
	}
	
	@Override
	public Button getBtnAllPlaylistTab() {
		return this.btnAllPlaylist;
	}
	
	@Override
	public PlaylistEditView getPlaylistEditView() {
		return this.playlistEditView;
	}
	
}
