package net.xapxinh.center.server.service.data;

import java.io.IOException;
import java.util.List;

import net.xapxinh.center.server.api.data.DataServiceApi;
import net.xapxinh.center.server.api.data.UnknownPlayerException;
import net.xapxinh.center.server.entity.Player;
import net.xapxinh.center.server.exception.DataServiceException;
import net.xapxinh.center.server.exception.PlayerConnectException;
import net.xapxinh.center.server.service.player.IPlayerService;
import net.xapxinh.center.shared.dto.Album;
import net.xapxinh.center.shared.dto.YoutubeVideos;

public class DataServiceImpl implements DataService {

	private final DataServiceApi dataService;
	private final IPlayerService playerService;

	public DataServiceImpl(DataServiceApi cloudApi, IPlayerService playerService) {
		this.dataService = cloudApi;
		this.playerService = playerService;
	}

	@Override
	public List<Album> searchAlbums(Player player, String searchKey, String searchScope, int pageNumber, int pageSize) throws PlayerConnectException, DataServiceException {
		if (!playerService.hasConnection(player)) {
			throw new PlayerConnectException();
		}
		try {
			return dataService.searchAlbums(player, searchKey, searchScope, pageNumber, pageSize);
		}
		catch (final UnknownPlayerException e) {
			insertPlayer(player);
			return dataService.searchAlbums(player, searchKey, searchScope, pageNumber, pageSize);
		}
	}

	@Override
	public void insertPlayer(Player player) throws DataServiceException {
		dataService.insertPlayer(player);
	}

	@Override
	public String getDataServiceUrl() {
		return dataService.getDataServiceUrl();
	}

	@Override
	public Album getAlbum(Player player, Long albumId) throws PlayerConnectException, DataServiceException {
		if (!playerService.hasConnection(player)) {
			throw new PlayerConnectException();
		}
		try {
			return dataService.getAlbum(player, albumId);
		}
		catch (final UnknownPlayerException e) {
			insertPlayer(player);
			return dataService.getAlbum(player, albumId);
		}
	}

	@Override
	public List<Album> getSpecialAlbums(Player player) throws PlayerConnectException, DataServiceException {
		if (!playerService.hasConnection(player)) {
			throw new PlayerConnectException();
		}
		try {
			return dataService.getSpecialAlbums(player);
		}
		catch (final UnknownPlayerException e) {
			insertPlayer(player);
			return dataService.getSpecialAlbums(player);
		}
	}

	@Override
	public void increaseAlbumListenCount(Player player, Long albumId) throws DataServiceException {
		try {
			dataService.increaseAlbumListenCount(player, albumId);
		}
		catch (final UnknownPlayerException e) {
			insertPlayer(player);
			dataService.increaseAlbumListenCount(player, albumId);
		}
	}

	@Override
	public YoutubeVideos searchYoutubeVideos(Player player, String key, String pageToken)
			throws PlayerConnectException, IOException {
		if (!playerService.hasConnection(player)) {
			throw new PlayerConnectException();
		}
		try {
			return dataService.searchYoutubeVideos(player, key, pageToken);
		}
		catch (final UnknownPlayerException e) {
			insertPlayer(player);
			return dataService.searchYoutubeVideos(player, key, pageToken);
		}
	}
}
