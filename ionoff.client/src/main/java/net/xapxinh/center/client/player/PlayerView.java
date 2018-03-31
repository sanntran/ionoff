package net.xapxinh.center.client.player;

import com.google.gwt.user.client.ui.FlowPanel;

import net.xapxinh.center.client.player.album.AlbumListView;
import net.xapxinh.center.client.player.browse.FilesBrowseView;
import net.xapxinh.center.client.player.menu.PlayerMenuView;
import net.xapxinh.center.client.player.playing.PlayerControlView;
import net.xapxinh.center.client.player.playlist.PlaylistListView;
import net.xapxinh.center.client.player.youtube.YoutubeVideoListView;

public class PlayerView extends FlowPanel implements IPlayerView {

	private PlayerMenuView menuView;
	protected PlayerControlView playingView;
	protected FlowPanel centerPanel;
	
	private AlbumListView albumsView = null;
	private FilesBrowseView fileBrowseView = null;
	private YoutubeVideoListView youtubesView = null;
	private PlaylistListView playlistsView = null;
	
	public PlayerView() {
		setStyleName("player");

		centerPanel = new FlowPanel();
		
		centerPanel.setStyleName("centre");
		add(centerPanel);
		
		menuView = new PlayerMenuView();
		add(menuView);
		
		playingView = new PlayerControlView();
		add(playingView);
		
		albumsView = new AlbumListView();
	}
	
	@Override
	public FlowPanel asPanel() {
		return this;
	}
	
	@Override
	public FlowPanel getCenterPanel() {
		return centerPanel;
	}

	@Override
	public AlbumListView getAlbumListView() {
		return albumsView;
	}
	
	@Override
	public FilesBrowseView getFileBrowseView() {
		if (fileBrowseView == null) {
			fileBrowseView = new FilesBrowseView();
		}
		return fileBrowseView;
	}

	@Override
	public YoutubeVideoListView getYoutubeVideoListView() {
		if (youtubesView == null) {
			youtubesView = new YoutubeVideoListView();
		}
		return youtubesView;
	}

	@Override
	public PlayerMenuView getMenuView() {
		return this.menuView;
	}

	@Override
	public PlayerControlView getPlayingView() {
		return playingView;
	}

	@Override
	public PlaylistListView getPlaylistListView() {
		if (playlistsView == null) {
			playlistsView = new PlaylistListView();
		}
		return playlistsView;
	}
}
