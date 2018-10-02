package net.ionoff.player.handler;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import com.google.gson.Gson;
import net.ionoff.player.connection.MqttRequestMessage;
import net.ionoff.player.connection.MqttResponseMessage;
import org.apache.log4j.Logger;

import net.ionoff.player.DeamonPlayer;
import net.ionoff.player.config.UserConfig;
import net.ionoff.player.exception.UnknownContextException;
import net.ionoff.player.model.Album;
import net.ionoff.player.model.MediaFile;
import net.ionoff.player.model.PlayLeaf;
import net.ionoff.player.model.PlayList;
import net.ionoff.player.model.PlayNode;
import net.ionoff.player.model.Schedule;
import net.ionoff.player.model.Song;
import net.ionoff.player.model.Status;
import net.ionoff.player.model.YoutubeVideo;

public class RequestHandler {
	private static final Logger LOGGER = Logger.getLogger(RequestHandler.class.getName());

	private static final Gson GSON = new Gson();

	public static final String PLAYLEAF = "playleaf";
	public static final String PLAYNODE = "playnode";
	public static final String PLAYLIST = "playlist";

	private static final String COMMAND = "command";
	private static final String CONTEXT_STATUS = "/status";
	private static final String CONTEXT_PLAYLIST = "/playlist";
	private static final String CONTEXT_BROWSE = "/browse";
	private static final String CONTEXT_SCHEDULE = "/scheduler";

	private RequestHandler() {
	}

	public static String handleRequest(MqttRequestMessage request) {
		Map<String, String> params = request.getParameters();
		return handleRequest(params);
	}

	private static String handleRequest(Map<String, String> params) {
		final String context = params.get(RequestContext.CONTEXT);
		params.remove(RequestContext.CONTEXT);

		if (context.equals(RequestContext.REQUESTS + CONTEXT_STATUS)) {
			return handleStatusRequest(params);
		}
		if (context.equals(RequestContext.REQUESTS + CONTEXT_PLAYLIST)) {
			return handlePlaylistRequest(params);
		}
		if (context.equals(RequestContext.REQUESTS + CONTEXT_BROWSE)) {
			return handleBrowseRequest(params);
		}
		if (context.equals(RequestContext.REQUESTS + CONTEXT_SCHEDULE)) {
			return handleScheduleRequest(params);
		}
		throw new UnknownContextException(context);
	}

	private static String handleScheduleRequest(Map<String, String> params) {
		Schedule schedule = new ScheduleRequestHandler().handleRequest(params);
		return new MqttResponseMessage("schedule", schedule).toJSONString();
	}

	private static String handleStatusRequest(final Map<String, String> params) {
		String command = params.get(COMMAND);
		final Status status = handleStatusRequest(command, params);
		return new MqttResponseMessage("status", status).toJSONString();
	}

	private static String handlePlaylistRequest(final Map<String, String> params) {
		String command = params.get(COMMAND);
		if (!params.isEmpty() && "pl_update".equals(command)) {
			updatePlaylist(params);
		}
		PlayList playlist = getPlayer().getPlaylist();
		return new MqttResponseMessage("playlist", playlist).toJSONString();
	}

	private static String handleBrowseRequest(final Map<String, String> params) {
		List<MediaFile> files = getBrowse(params);
		return new MqttResponseMessage("files", files).toJSONString();
	}

	private static List<MediaFile> getBrowse(Map<String, String> params) {
		String dir = params.get("dir");
		String command = params.get(COMMAND);
		return getMediaFiles(dir, command);
	}

	private static List<MediaFile> getMediaFiles(String dir, String command) {
		if (dir == null || dir.isEmpty()) {
			if ("refresh".equals(command)) {
				getPlayer().updateFileDatabase();
			}
			List<MediaFile> mediaFiles = new ArrayList<>();
			MediaFile mFile = new MediaFile();
			mFile.setName(".albums");
			mFile.setPath(".albums");
			mFile.setType(MediaFile.TYPE.dir.toString());
			mediaFiles.add(mFile);
			mediaFiles.addAll(getPlayer().browseFiles(dir));
			return mediaFiles;
		}
		else if (".albums".equals(dir)) {
			List<MediaFile> mediaFiles = new ArrayList<>();
			File file = new File(UserConfig.getInstance().ROOT_BROWSE_DIR + File.separator + dir);

			if (!file.exists() || file.isFile()) {
				return mediaFiles;
			}
			MediaFile parent = new MediaFile();
			parent.setName("..");
			parent.setPath("");
			parent.setType(MediaFile.TYPE.dir.toString());
			mediaFiles.add(parent);

			for (File f : file.listFiles()) {
				MediaFile mFile = new MediaFile();
				mFile.setName(f.getName());
				mFile.setPath(".albums" + File.separator + f.getName());
				if (f.isDirectory()) {
					mFile.setType(MediaFile.TYPE.dir.toString());
				} else {
					mFile.setType(MediaFile.TYPE.file.toString());
				}
				if (mFile.isAlbum()) {
					mediaFiles.add(mFile);
				}
			}
			return mediaFiles;
		}
		else {
			if ("refresh".equals(command)) {
				getPlayer().updateFileDatabase();
			}
			if (dir.endsWith("/..")) {
				dir = dir.substring(0, dir.lastIndexOf("/"));
				if (dir.contains("/")) {
					dir = dir.substring(0, dir.lastIndexOf("/"));
				}
				else {
					dir = ""; // root
				}
			}
			if (dir == null || dir.isEmpty()) {
				List<MediaFile> mediaFiles = new ArrayList<>();
				MediaFile mFile = new MediaFile();
				mFile.setName(".albums");
				mFile.setPath(".albums");
				mFile.setType(MediaFile.TYPE.dir.toString());
				mediaFiles.add(mFile);
				mediaFiles.addAll(getPlayer().browseFiles(dir));
				return mediaFiles;
			}
			return getPlayer().browseFiles(dir);
		}
	}

	private static DeamonPlayer deamonPlayer;

	private static synchronized DeamonPlayer getPlayer() {
		if (deamonPlayer == null) {
			deamonPlayer = new DeamonPlayer();
		}
		return deamonPlayer;
	}

	public static Status handleStatusRequest() {
		return handleStatusRequest(null, Collections.emptyMap());
	}

	private static Status handleStatusRequest(String command, Map<String, String> parameters) {
		if (command == null || parameters.isEmpty()) {
			return getPlayer().loadStatus();
		}
		if ("pl_play".equals(command)) {
			pl_play(parameters);
		} else if ("in_play".equals(command)) {
			in_play(parameters);
		} else if ("in_enqueue".equals(command)) {
			in_enqueue(parameters, false);
		} else if ("pl_previous".equals(command)) {
			pl_previous(parameters);
		} else if ("pl_next".equals(command)) {
			pl_next(parameters);
		} else if ("pl_delete".equals(command)) {
			pl_delete(parameters);
		} else if ("pl_repeat".equals(command)) {
			pl_repeat(parameters);
		} else if ("seek".equals(command)) {
			seek(parameters);
		} else if ("pl_pause".equals(command)) {
			pl_pause(parameters);
		} else if ("pl_forceresume".equals(command)) {
			pl_forceresume(parameters);
		} else if ("pl_resume".equals(command)) {
			pl_resume(parameters);
		} else if ("pl_forcepause".equals(command)) {
			pl_forcepause(parameters);
		} else if ("pl_stop".equals(command)) {
			pl_stop(parameters);
		} else if ("pl_empty".equals(command)) {
			pl_empty(parameters);
		} else if ("volume".equals(command)) {
			volume(parameters);
		} else if ("pl_loop".equals(command)) {
			pl_loop(parameters);
		} else if ("pl_random".equals(command)) {
			pl_random(parameters);
		} else if ("fullscreen".equals(command)) {
			fullscreen(parameters);
		}
		return getPlayer().loadStatus();
	}

	private static void fullscreen(Map<String, String> parameters) {
		return;
	}

	private static void volume(Map<String, String> parameters) {
		String val = parameters.get("val");
		if (val.equals("mute")) {
			getPlayer().mute();
		} else if (val.equals("+")) {
			int vol = getPlayer().getVolume() + 4;
			getPlayer().setVolume(vol);
		} else if (val.equals("-")) {
			int vol = getPlayer().getVolume() - 4;
			getPlayer().setVolume(vol);
		} else {
			int vol = Integer.parseInt(val);
			getPlayer().setVolume(vol);
		}
	}

	private static void pl_repeat(Map<String, String> parameters) {
		getPlayer().plRepeat();
	}

	private static void pl_random(Map<String, String> parameters) {
		getPlayer().randomizePlay();
	}

	private static void pl_loop(Map<String, String> parameters) {
		// does nothing // ???
	}

	private static void pl_empty(Map<String, String> parameters) {
		getPlayer().emtyPlaylist();
		getPlayer().getPlaylist().setId(0L);
		getPlayer().getPlaylist().setName(null);
	}

	private static void pl_stop(Map<String, String> parameters) {
		getPlayer().stop();
	}

	private static void pl_forcepause(Map<String, String> parameters) {
		getPlayer().pause();
	}

	private static void pl_resume(Map<String, String> parameters) {
		getPlayer().play();
	}

	private static void pl_forceresume(Map<String, String> parameters) {
		getPlayer().play();
	}

	private static void pl_pause(Map<String, String> parameters) {
		getPlayer().pause();
	}

	private static void seek(Map<String, String> parameters) {
		String val = parameters.get("val");
		if (val.equals("+")) {
			getPlayer().seekFw();
		} else if (val.equals("-")) {
			getPlayer().seekRw();
		}
	}

	private static void pl_delete(Map<String, String> parameters) {
		String id = parameters.get("id");
		String type = parameters.get("type");
		if (PLAYLEAF.equals(type)) {
			getPlayer().deleteLeaf(Long.parseLong(id));
		} else if (PLAYNODE.equals(type)) {
			getPlayer().deleteNode(Long.parseLong(id));
		}
	}

	private static void pl_next(Map<String, String> parameters) {
		getPlayer().playNext();
	}

	private static void pl_previous(Map<String, String> parameters) {
		getPlayer().playPrevious();
	}

	private static void in_enqueue(Map<String, String> parameters, boolean isPlay) {
		String input = parameters.get("input");
		String inputType = parameters.get("input_type");
		
		if (PlayNode.TYPE.album.toString().equals(inputType)) {
			Album album = GSON.fromJson(input, Album.class);
			addAlbumFile(album);
			getPlayer().inEnqueue(album, isPlay);
		} 
		else if (PlayNode.TYPE.dir.toString().equals(inputType)) {
			getPlayer().inEnqueue(input, true, isPlay);
		} 
		else if (MediaFile.TYPE.file.toString().equals(inputType)) { 
			getPlayer().inEnqueue(input, false, isPlay);
		}
		else if (PlayLeaf.TYPE.track.toString().equals(inputType)) {
			Song song = GSON.fromJson(input, Song.class);
			getPlayer().inEnqueue(song, isPlay);
		}
		else if (PlayLeaf.TYPE.youtube.toString().equals(inputType)) {
			YoutubeVideo video = GSON.fromJson(input, YoutubeVideo.class);
			getPlayer().inEnqueue(video, isPlay);
		}
		
		else if (PLAYLIST.equals(inputType)) {
			PlayList playlist = GSON.fromJson(input, PlayList.class);
			getPlayer().inEnqueue(playlist, isPlay);
		}
		else if (PLAYNODE.equals(inputType)) {
			PlayNode playnode = GSON.fromJson(input, PlayNode.class);
			getPlayer().inEnqueue(playnode, isPlay);
		}
		else if (PLAYLEAF.equals(inputType)) {
			PlayLeaf playleaf = GSON.fromJson(input, PlayLeaf.class);
			getPlayer().inEnqueue(playleaf, isPlay);
		}
	}

	private static void in_play(Map<String, String> parameters) {
		in_enqueue(parameters, true);
	}

	private static void addAlbumFile(Album album) {
		File albumFolder = new File(UserConfig.getInstance().getAlbumFolder());
		String albumFileName = getAlbumFileName(album);
		if (!albumFolder.exists()) {
			albumFolder.mkdir();
		}
		if (!hasAlbum(albumFolder, albumFileName)) {
			File file = new File(albumFolder + File.separator + albumFileName);
			try {
				file.createNewFile();
			} catch (IOException e) {
				LOGGER.error(e.getMessage(), e);
			}
		}
	}

	private static boolean hasAlbum(File albumFolder, String albumFileName) {
		for (File file : albumFolder.listFiles()) {
			if (file.getName().equals(albumFileName)) {
				return true;
			}
		}
		return false;
	}

	private static String getAlbumFileName(Album album) {
		return AccentRemover.removeAccent(album.getTitle()) + "#" + album.getId() + ".album";
	}


	private static void pl_play(Map<String, String> parameters) {
		String id = parameters.get("id");
		String type = parameters.get("type");
		if (PLAYLEAF.equals(type)) {
			getPlayer().playPlaylistLeaf(Long.parseLong(id));
		} else if (PLAYNODE.equals(type)) {
			getPlayer().playPlaylistNode(Long.parseLong(id));
		} else {
			getPlayer().playPlaylist();
		}
	}
	
	private static void updatePlaylist(Map<String, String> params) {
 		String plJson = params.get("playlist");
 		PlayList playlist = GSON.fromJson(plJson, PlayList.class);
		PlayList playingList = getPlayer().getPlaylist();
		playingList.setId(playlist.getId());
		playingList.setName(playlist.getName());
		for (PlayNode node : playlist.getNodes()) {
			for (PlayNode playingNode : playingList.getNodes()) {
				if (playingNode.getIdx() == node.getIdx()) {
					updatePlayNode(playingNode, node);
				}
			}
		}
	}

	private static void updatePlayNode(PlayNode playingNode, PlayNode node) {
		playingNode.setId(node.getId());
		for (PlayLeaf leaf : node.getLeafs()) {
			for (PlayLeaf playingLeaf : playingNode.getLeafs()) {
				if (playingLeaf.getIdx() == leaf.getIdx()) {
					playingLeaf.setId(leaf.getId());
				}
			}
		}
 	}

	public static PlayList handlePlayListRequest() {
		PlayList playlist = getPlayer().getPlaylist();
		return playlist;
	}
}
