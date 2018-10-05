package net.ionoff.center.server.mediadata.service;

import net.ionoff.center.server.mediaplayer.model.MediaPlayer;
import net.xapxinh.center.shared.dto.Album;
import net.xapxinh.center.shared.dto.YoutubeVideosDto;

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
