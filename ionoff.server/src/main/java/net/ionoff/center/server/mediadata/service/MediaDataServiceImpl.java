package net.ionoff.center.server.mediadata.service;

import net.ionoff.center.server.mediadata.connector.IMediaDataConnector;
import net.ionoff.center.server.mediadata.exception.PlayerNotFoundException;
import net.ionoff.center.server.mediaplayer.exception.MediaPlayerConnectException;
import net.ionoff.center.server.mediaplayer.model.MediaPlayer;
import net.ionoff.center.server.mediaplayer.service.IMediaPlayerService;
import net.ionoff.center.shared.dto.player.Album;
import net.ionoff.center.shared.dto.player.YoutubeVideosDto;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class MediaDataServiceImpl implements IMediaDataService {

	private final IMediaDataConnector mediaDataConnector;
	private final IMediaPlayerService playerService;

	public MediaDataServiceImpl(IMediaDataConnector mediaDataConnector, IMediaPlayerService playerService) {
		this.mediaDataConnector = mediaDataConnector;
		this.playerService = playerService;
	}

	@Override
	public List<Album> searchAlbums(MediaPlayer player, String searchKey,
	                                String searchScope, int pageNumber, int pageSize) {
		if (!player.isOnline()) {
			throw new MediaPlayerConnectException("Player " + player.getMac() + " is not connected");
		}
		try {
			return mediaDataConnector.searchAlbums(player, searchKey, searchScope, pageNumber, pageSize);
		} catch (final PlayerNotFoundException e) {
			insertPlayer(player);
			return mediaDataConnector.searchAlbums(player, searchKey, searchScope, pageNumber, pageSize);
		}
	}

	@Override
	public void insertPlayer(MediaPlayer player) {
		mediaDataConnector.insertPlayer(player);
	}

	@Override
	public String getDataServiceUrl() {
		return mediaDataConnector.getDataServiceUrl();
	}

	@Override
	public Album getAlbum(MediaPlayer player, Long albumId) {
		if (!player.isOnline()) {
			throw new MediaPlayerConnectException("Player " + player.getMac() + " is not connected");
		}
		try {
			return mediaDataConnector.getAlbum(player, albumId);
		} catch (final PlayerNotFoundException e) {
			insertPlayer(player);
			return mediaDataConnector.getAlbum(player, albumId);
		}
	}

	@Override
	public List<Album> getSpecialAlbums(MediaPlayer player) {
		if (!player.isOnline()) {
			throw new MediaPlayerConnectException("Player " + player.getMac() + " is not connected");
		}
		try {
			return mediaDataConnector.getSpecialAlbums(player);
		}
		catch (final PlayerNotFoundException e) {
			insertPlayer(player);
			return mediaDataConnector.getSpecialAlbums(player);
		}
	}

	@Override
	public void increaseAlbumListenCount(MediaPlayer player, Long albumId) {
		try {
			mediaDataConnector.increaseAlbumListenCount(player, albumId);
		} catch (final PlayerNotFoundException e) {
			insertPlayer(player);
			mediaDataConnector.increaseAlbumListenCount(player, albumId);
		}
	}

	@Override
	public YoutubeVideosDto searchYoutubeVideos(MediaPlayer player, String key, String pageToken) {
		if (!player.isOnline()) {
			throw new MediaPlayerConnectException("Player " + player.getMac() + " is not connected");
		}
		try {
			return mediaDataConnector.searchYoutubeVideos(player, key, pageToken);
		} catch (final PlayerNotFoundException e) {
			insertPlayer(player);
			return mediaDataConnector.searchYoutubeVideos(player, key, pageToken);
		}
	}
}
