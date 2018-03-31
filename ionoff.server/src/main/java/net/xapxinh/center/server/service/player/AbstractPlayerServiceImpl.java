package net.xapxinh.center.server.service.player;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.google.gson.Gson;

import net.xapxinh.center.server.api.data.DataServiceApi;
import net.xapxinh.center.server.api.player.IPlayerApi;
import net.xapxinh.center.server.entity.Player;
import net.xapxinh.center.server.exception.PlayerConnectException;
import net.xapxinh.center.shared.dto.Album;
import net.xapxinh.center.shared.dto.MediaFile;
import net.xapxinh.center.shared.dto.PlayLeafDto;
import net.xapxinh.center.shared.dto.PlayListDto;
import net.xapxinh.center.shared.dto.PlayNodeDto;
import net.xapxinh.center.shared.dto.Schedule;
import net.xapxinh.center.shared.dto.Song;
import net.xapxinh.center.shared.dto.Status;
import net.xapxinh.center.shared.dto.YoutubeVideo;

public abstract class AbstractPlayerServiceImpl implements IPlayerService {
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
	
	protected final Gson gson;
	private final IPlayerApi xmpPlayerApi;
	private final PlayerCaches playerCaches;
	protected final DataServiceApi dataServiceApi;

	public AbstractPlayerServiceImpl(IPlayerApi xmpPlayerApi, AbstractPlayerConnectionPool playerConnectionPool, 
			PlayerCaches playerCaches, DataServiceApi dataServiceApi) {
		gson = new Gson();
		this.xmpPlayerApi = xmpPlayerApi;
		this.playerCaches = playerCaches;
		this.dataServiceApi = dataServiceApi;
	}

	abstract protected PlayListDto getPlayList(Player player, Long playLisId);
	abstract protected PlayNodeDto getPlayNode(Player player, Long playNodeId);
	abstract protected PlayLeafDto getPlayLeaf(Player player, Long playLeafId);
	
	@Override
	public Status requesStatus(Player player, Map<String, Object> params) {
		if (params == null) {
			params = new HashMap<String, Object>();
		}
		if (params.isEmpty()) {
			if (playerCaches.getStatus(player.getId()) != null) {
				return playerCaches.getStatus(player.getId());
			}
		}
		if (isInputAlbum(params)) {
			Long albumId = Long.parseLong((String)params.get(INPUT));		
			Album album = dataServiceApi.getAlbum(player, albumId);
			dataServiceApi.increaseAlbumListenCount(player, albumId);
			putAlbumParam(player, params, album);
		}
		else if (isInputYoutube(params)) {
			YoutubeVideo youtubeVideo = new YoutubeVideo();
			youtubeVideo.setId((String)params.get(INPUT));
			youtubeVideo.setTitle((String)params.get(TITLE));
			String mrl = getActualYoutubeVideoUrl(player, youtubeVideo.getId());
			youtubeVideo.setMrl(mrl);
			params.put(INPUT, gson.toJson(youtubeVideo));
		}
		else if (isInputPlaylist(params)) {
			Long playlistId = Long.parseLong((String)params.get(INPUT));		
			PlayListDto playlist = getPlayList(player, playlistId);
			putPlayListParam(player, params, playlist);
		}
		else if (isInputAlbumTrack(params)) {
			Long songId = Long.parseLong((String)params.get(INPUT));		
			Song song = dataServiceApi.getSong(player, songId);
			params.put(INPUT, gson.toJson(song));
		}
		else if (isInputPlayNode(params)) {
			Long nodeId = Long.parseLong((String)params.get(INPUT));	
			PlayNodeDto playNode = getPlayNode(player, nodeId);
			params.put(INPUT, gson.toJson(playNode));
		}
		else if (isInputPlayLeaf(params)) {
			Long leafId = Long.parseLong((String)params.get(INPUT));	
			PlayLeafDto playLeaf = getPlayLeaf(player, leafId);
			params.put(INPUT, gson.toJson(playLeaf));
		}
		try {
			final Status status = getPlayerApi(player).requestStatus(player, params);
			playerCaches.storeStatus(player.getId(), status);
			return status;
		}
		catch (final PlayerConnectException e) {
			playerCaches.storeStatus(player.getId(), null);
			throw e;
		}
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

	private void putPlayListParam(Player player, Map<String, Object> params, PlayListDto playlist) {
		params.put(INPUT, gson.toJson(playlist));
	}

	protected void putAlbumParam(Player player, Map<String, Object> params, Album album) {
		params.put(INPUT, gson.toJson(album));
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

	@Override
	public PlayListDto requestPlaylist(Player player) throws PlayerConnectException {
		if (playerCaches.getPlaylist(player.getId()) != null) {
			return playerCaches.getPlaylist(player.getId());
		}
		final PlayListDto playlist =  getPlayerApi(player).requestPlaylist(player);
		playerCaches.storePlaylist(player.getId(), playlist);
		return playlist;
	}

	@Override
	public List<MediaFile> requestMediaFiles(Player player, Map<String, Object> params) {
		final List<MediaFile> mediaFiles = getPlayerApi(player).requestBrowse(player, params);
		return mediaFiles;
	}

	@Override
	public boolean hasStatusCache(Player player) {
		return playerCaches.hasStatus(player.getId());
	}

	@Override
	public boolean hasConnection(Player player) {
		return getPlayerApi(player).hasPlayerConnection(player);
	}

	@Override
	public Schedule requestSchedule(Player player, Map<String, Object> params) {
		final Schedule schedule = getPlayerApi(player).requestSchedule(player, params);
		return schedule;
	}
	
	protected IPlayerApi getPlayerApi(Player player) {
		return xmpPlayerApi;
	}
	
	protected String getActualYoutubeVideoUrl(Player player, String videoId) {
		return dataServiceApi.getYoutubeVideoUrl(player, videoId);
	}

	@Override
	public PlayListDto updatePlaylist(Player player, PlayListDto playlist) {
		Map<String, Object> params = new HashMap<String, Object>();
		params.put(PLAYLIST, gson.toJson(playlist));
		params.put("command", "pl_update");
		final PlayListDto pl =  getPlayerApi(player).updatePlaylist(player, params);
		playerCaches.storePlaylist(player.getId(), pl);
		return pl;
	}
}
