package net.ionoff.center.client.mediaplayer;

import net.ionoff.center.client.mediaplayer.album.AlbumListView;
import net.ionoff.center.client.mediaplayer.browse.FilesBrowseView;
import net.ionoff.center.client.mediaplayer.menu.PlayerMenuView;
import net.ionoff.center.client.mediaplayer.playing.PlayerControlView;
import net.ionoff.center.client.mediaplayer.playlist.PlaylistListView;
import net.ionoff.center.client.mediaplayer.youtube.YoutubeVideoListView;

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
