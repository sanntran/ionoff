package net.ionoff.center.server.mediadata.service;

import net.ionoff.center.server.mediaplayer.model.MediaPlayer;
import net.ionoff.center.shared.dto.player.Album;
import net.ionoff.center.shared.dto.player.YoutubeVideosDto;

import java.util.List;

public interface IMediaDataService {
	
	String getDataServiceUrl();
	
	void insertPlayer(MediaPlayer player);
	
	List<Album> searchAlbums(MediaPlayer player, String searchKey,
	                         String searchScope, int pageNumber, int pageSize);
	
	void increaseAlbumListenCount(MediaPlayer player, Long albumId);
	
	Album getAlbum(MediaPlayer player, Long albumId);
	
	List<Album> getSpecialAlbums(MediaPlayer player);
	
	YoutubeVideosDto searchYoutubeVideos(MediaPlayer player, final String key, final String pageToken);
}
