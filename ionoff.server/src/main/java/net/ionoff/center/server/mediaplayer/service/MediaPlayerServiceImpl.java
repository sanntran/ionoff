package net.ionoff.center.server.mediaplayer.service;

import com.google.gson.Gson;
import net.ionoff.center.server.entity.PlayLeaf;
import net.ionoff.center.server.entity.PlayList;
import net.ionoff.center.server.entity.PlayNode;
import net.ionoff.center.server.mediadata.connector.IMediaDataConnector;
import net.ionoff.center.server.mediadata.exception.MediaDataRequestException;
import net.ionoff.center.server.mediaplayer.cache.PlayerCaches;
import net.ionoff.center.server.mediaplayer.connector.IMediaPlayerConnector;
import net.ionoff.center.server.mediaplayer.exception.MediaPlayerConnectException;
import net.ionoff.center.server.mediaplayer.model.MediaPlayer;
import net.ionoff.center.server.persistence.mapper.PlayListMapper;
import net.ionoff.center.server.persistence.service.IPlayListService;
import net.ionoff.center.server.persistence.service.IPlayNodeService;
import net.ionoff.center.shared.dto.player.Album;
import net.ionoff.center.shared.dto.player.MediaFile;
import net.ionoff.center.shared.dto.player.PlayLeafDto;
import net.ionoff.center.shared.dto.player.PlayListDto;
import net.ionoff.center.shared.dto.player.PlayNodeDto;
import net.ionoff.center.shared.dto.player.ScheduleDto;
import net.ionoff.center.shared.dto.player.SongDto;
import net.ionoff.center.shared.dto.player.StatusDto;
import net.ionoff.center.shared.dto.player.YoutubeVideoDto;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Service
public class MediaPlayerServiceImpl implements IMediaPlayerService {

	protected static final String INPUT = "input";
	protected static final String INPUT_TYPE = "input_type";
	protected static final String YOUTUBE = "youtube";
	protected static final String TITLE = "title";
	protected static final String ALBUM = "album";
	protected static final String COMMAND = "command";
	protected static final String IN_PLAY = "in_play";
	protected static final String IN_ENQUEUE = "in_enqueue";
	protected static final String PLAYLIST = "playlist";
	protected static final String PLAYNODE = "playnode";
	protected static final String PLAYLEAF = "playleaf";
	protected static final String TRACK = "track";

	@Autowired
	private IMediaPlayerConnector playerConnector;
	
	@Autowired
	private PlayListMapper playListMapper;
	
	@Autowired
	private IPlayListService playListService;
	
	@Autowired
	private IPlayNodeService playNodeService;


	protected final Gson gson;
	private final PlayerCaches playerCaches;
	protected final IMediaDataConnector mediaDataServiceConnector;

	public MediaPlayerServiceImpl(IMediaPlayerConnector playerConnector,
	                                 PlayerCaches playerCaches, IMediaDataConnector mediaDataServiceConnector) {
		gson = new Gson();
		this.playerConnector = playerConnector;
		this.playerCaches = playerCaches;
		this.mediaDataServiceConnector = mediaDataServiceConnector;
	}

	@Override
	public StatusDto requesStatus(MediaPlayer player, Map<String, Object> params) {
		if (params == null) {
			params = new HashMap<>();
		}
		if (params.isEmpty()) {
			if (playerCaches.getStatus(player.getId()) != null) {
				return playerCaches.getStatus(player.getId());
			}
		}
		if (isInputAlbum(params)) {
			Long albumId = Long.parseLong((String)params.get(INPUT));
			Album album = mediaDataServiceConnector.getAlbum(player, albumId);
			mediaDataServiceConnector.increaseAlbumListenCount(player, albumId);
			putAlbumParam(player, params, album);
		}
		else if (isInputYoutube(params)) {
			YoutubeVideoDto youtubeVideo = new YoutubeVideoDto();
			youtubeVideo.setId((String)params.get(INPUT));
			youtubeVideo.setTitle((String)params.get(TITLE));
			String mrl = getActualYoutubeVideoUrl(player, youtubeVideo.getId());
			youtubeVideo.setMrl(mrl);
			params.put(INPUT, youtubeVideo);
		}
		else if (isInputPlaylist(params)) {
			Long playlistId = Long.parseLong((String)params.get(INPUT));
			PlayListDto playlist = getPlayList(player, playlistId);
			putPlayListParam(player, params, playlist);
		}
		else if (isInputAlbumTrack(params)) {
			Long songId = Long.parseLong((String)params.get(INPUT));
			SongDto song = mediaDataServiceConnector.getSong(player, songId);
			params.put(INPUT, song);
		}
		else if (isInputPlayNode(params)) {
			Long nodeId = Long.parseLong((String)params.get(INPUT));
			PlayNodeDto playNode = getPlayNode(player, nodeId);
			params.put(INPUT, playNode);
		}
		else if (isInputPlayLeaf(params)) {
			Long leafId = Long.parseLong((String)params.get(INPUT));
			PlayLeafDto playLeaf = getPlayLeaf(player, leafId);
			params.put(INPUT, playLeaf);
		}
		try {
			final StatusDto status = getPlayerConnector(player).requestStatus(player, params);
			playerCaches.storeStatus(player.getId(), status);
			return status;
		}
		catch (final MediaPlayerConnectException e) {
			playerCaches.storeStatus(player.getId(), null);
			throw e;
		}
	}


	@Override
	public PlayListDto requestPlaylist(MediaPlayer player) throws MediaPlayerConnectException {
		if (playerCaches.getPlaylist(player.getId()) != null) {
			return playerCaches.getPlaylist(player.getId());
		}
		final PlayListDto playlist =  getPlayerConnector(player).requestPlaylist(player);
		playerCaches.storePlaylist(player.getId(), playlist);
		return playlist;
	}

	@Override
	public List<MediaFile> requestMediaFiles(MediaPlayer player, Map<String, Object> params) {
		final List<MediaFile> mediaFiles = getPlayerConnector(player).requestBrowse(player, params);
		return mediaFiles;
	}

	@Override
	public ScheduleDto requestSchedule(MediaPlayer player, Map<String, Object> params) {
		return getPlayerConnector(player).requestSchedule(player, params);
	}

	@Override
	public PlayListDto updatePlaylist(MediaPlayer player, PlayListDto playlist) {
		Map<String, Object> params = new HashMap<String, Object>();
		params.put(PLAYLIST, playlist);
		params.put("command", "pl_update");
		final PlayListDto pl =  getPlayerConnector(player).updatePlaylist(player, params);
		playerCaches.storePlaylist(player.getId(), pl);
		return pl;
	}

	private boolean isInputAlbumTrack(Map<String, Object> params) {
		if ((IN_PLAY.equals(params.get(COMMAND)) || IN_ENQUEUE.equals(params.get(COMMAND)))
				&& TRACK.equals(params.get(INPUT_TYPE))) {
			return true;
		}
		return false;
	}

	private boolean isInputPlayNode(Map<String, Object> params) {
		if ((IN_PLAY.equals(params.get(COMMAND)) || IN_ENQUEUE.equals(params.get(COMMAND)))
				&& PLAYNODE.equals(params.get(INPUT_TYPE))) {
			return true;
		}
		return false;
	}

	private boolean isInputPlayLeaf(Map<String, Object> params) {
		if ((IN_PLAY.equals(params.get(COMMAND)) || IN_ENQUEUE.equals(params.get(COMMAND)))
				&& PLAYLEAF.equals(params.get(INPUT_TYPE))) {
			return true;
		}
		return false;
	}

	private void putPlayListParam(MediaPlayer player, Map<String, Object> params, PlayListDto playlist) {
		params.put(INPUT, playlist);
	}


	private boolean isInputAlbum(Map<String, Object> params) {
		if ((IN_PLAY.equals(params.get(COMMAND))
				|| IN_ENQUEUE.equals(params.get(COMMAND)))
				&& ALBUM.equals(params.get(INPUT_TYPE))) {
			return true;
		}
		Object input = params.get(INPUT);
		if (isAlbum(input)) {
			params.put(INPUT, getAlbumId(input));
			params.put(INPUT_TYPE, ALBUM);
			return true;
		}
		return false;
	}

	private boolean isInputYoutube(Map<String, Object> params) {
		if ((IN_PLAY.equals(params.get(COMMAND)) || IN_ENQUEUE.equals(params.get(COMMAND)))
				&& YOUTUBE.equals(params.get(INPUT_TYPE))) {
			return true;
		}
		Object input = params.get(INPUT);
		if (isAlbum(input)) {
			params.put(INPUT, getAlbumId(input));
			return true;
		}
		return false;
	}

	private boolean isInputPlaylist(Map<String, Object> params) {
		if ((IN_PLAY.equals(params.get(COMMAND)) || IN_ENQUEUE.equals(params.get(COMMAND)))
				&& PLAYLIST.equals(params.get(INPUT_TYPE))) {
			return true;
		}
		return false;
	}

	private static String getAlbumId(Object input) {
		return input.toString().substring(input.toString().lastIndexOf('#') + 1,
				input.toString().lastIndexOf(".album"));
	}

	private static boolean isAlbum(Object input) {
		if (input == null) {
			return false;
		}
		return input instanceof String && input.toString().contains("#") && input.toString().endsWith(".album");
	}

	
	protected IMediaPlayerConnector getPlayerConnector(MediaPlayer player) {
		return playerConnector;
	}
	
	protected void putAlbumParam(MediaPlayer player, Map<String, Object> params, Album album) {
		params.put(INPUT, album);
	}
	
	protected String getActualYoutubeVideoUrl(MediaPlayer player, String videoId) throws MediaDataRequestException {
		if (net.ionoff.center.server.entity.MediaPlayer.IMP.equals(player.getModel())) {
			return mediaDataServiceConnector.getYoutubeAudioUrl(player, videoId);
		}
		else {
			return mediaDataServiceConnector.getYoutubeVideoUrl(player, videoId);
		}
		
	}

	protected PlayListDto getPlayList(MediaPlayer player, Long playLisId) {
		PlayList playlist = playListService.findById(playLisId);
		return toPlayListDto(playlist);
	}

	private PlayListDto toPlayListDto(PlayList playlist) {
		PlayListDto playListDto = playListMapper.createPlayListDto(playlist);
		for (PlayNode node : playlist.getNodes()) {
			playListDto.getNodes().add(playListMapper.createPlayNodeDto(node));
		}
		return playListDto;
	}

	protected PlayNodeDto getPlayNode(MediaPlayer player, Long playNodeId) {
		PlayNode playnode = playNodeService.findById(playNodeId);
		return playListMapper.createPlayNodeDto(playnode);
	}

	protected PlayLeafDto getPlayLeaf(MediaPlayer player, Long playLeafId) {
		PlayLeaf playleaf = playNodeService.findLeafById(playLeafId);
		return playListMapper.createPlayLeafDto(playleaf);
	}

}
