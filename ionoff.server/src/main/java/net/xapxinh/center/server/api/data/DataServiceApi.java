package net.xapxinh.center.server.api.data;

import java.util.List;

import net.xapxinh.center.server.entity.Player;
import net.xapxinh.center.shared.dto.Album;
import net.xapxinh.center.shared.dto.SongDto;
import net.xapxinh.center.shared.dto.YoutubeVideosDto;

public interface DataServiceApi {
	
	String getDataServiceUrl();
	
	void insertPlayer(Player player);
	
	Album getAlbum(Player player, Long albumId);
	
	SongDto getSong(Player player, Long songId);
	
	List<Album> getSpecialAlbums(Player player);
	
	List<Album> searchAlbums(Player player, String searchKey, String searchScope, int pageNumber, int pageSize);
	
	YoutubeVideosDto searchYoutubeVideos(Player player, final String key, final String pageToken);
	
	void increaseAlbumListenCount(Player player, Long albumId);
	
	String getYoutubeVideoUrl(Player player, String videoId);
	
	String getYoutubeAudioUrl(Player player, String videoId);
}
