package net.ionoff.center.server.mediadata.connector;

import java.util.List;

import net.ionoff.center.server.mediaplayer.model.MediaPlayer;
import net.xapxinh.center.shared.dto.Album;
import net.xapxinh.center.shared.dto.SongDto;
import net.xapxinh.center.shared.dto.YoutubeVideosDto;

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
