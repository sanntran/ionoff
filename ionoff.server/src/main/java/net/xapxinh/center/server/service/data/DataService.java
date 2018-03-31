package net.xapxinh.center.server.service.data;

import java.io.IOException;
import java.util.List;

import net.xapxinh.center.server.entity.Player;
import net.xapxinh.center.server.exception.DataServiceException;
import net.xapxinh.center.server.exception.PlayerConnectException;
import net.xapxinh.center.shared.dto.Album;
import net.xapxinh.center.shared.dto.YoutubeVideos;

public interface DataService {
	
	String getDataServiceUrl();
	
	void insertPlayer(Player player) throws DataServiceException;
	
	List<Album> searchAlbums(Player player, String searchKey, String searchScope, int pageNumber, int pageSize)
			throws PlayerConnectException, DataServiceException;
	
	void increaseAlbumListenCount(Player player, Long albumId) throws DataServiceException;
	
	Album getAlbum(Player player, Long albumId) throws PlayerConnectException, DataServiceException;
	
	List<Album> getSpecialAlbums(Player player) throws PlayerConnectException, DataServiceException;
	
	YoutubeVideos searchYoutubeVideos(Player player, final String key, final String pageToken) throws PlayerConnectException, IOException;
}
