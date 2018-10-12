package net.ionoff.center.server.mediadata.connector;

import java.util.List;

import net.ionoff.center.server.mediaplayer.model.MediaPlayer;
import net.ionoff.center.shared.dto.player.Album;
import net.ionoff.center.shared.dto.player.SongDto;
import net.ionoff.center.shared.dto.player.YoutubeVideosDto;

public interface IMediaDataConnector {
	
	String getDataServiceUrl();
	
	void insertPlayer(MediaPlayer player);
	
	Album getAlbum(MediaPlayer player, Long albumId);
	
	SongDto getSong(MediaPlayer player, Long songId);
	
	List<Album> getSpecialAlbums(MediaPlayer player);
	
	List<Album> searchAlbums(MediaPlayer player, String searchKey, String searchScope, int pageNumber, int pageSize);
	
	YoutubeVideosDto searchYoutubeVideos(MediaPlayer player, final String key, final String pageToken);
	
	void increaseAlbumListenCount(MediaPlayer player, Long albumId);
	
	String getYoutubeVideoUrl(MediaPlayer player, String videoId);
	
	String getYoutubeAudioUrl(MediaPlayer player, String videoId);
}
