package net.xapxinh.center.client.player;

import net.xapxinh.center.client.player.album.AlbumListView;
import net.xapxinh.center.client.player.browse.FilesBrowseView;
import net.xapxinh.center.client.player.menu.PlayerMenuView;
import net.xapxinh.center.client.player.playing.PlayerControlView;
import net.xapxinh.center.client.player.playlist.PlaylistListView;
import net.xapxinh.center.client.player.youtube.YoutubeVideoListView;

import com.google.gwt.user.client.ui.FlowPanel;

public interface IPlayerView {
	FlowPanel asPanel();		
	PlayerMenuView getMenuView();
	PlayerControlView getPlayingView();
	AlbumListView getAlbumListView();
	FilesBrowseView getFileBrowseView();
	YoutubeVideoListView getYoutubeVideoListView();
	PlaylistListView getPlaylistListView();
	FlowPanel getCenterPanel();
}
