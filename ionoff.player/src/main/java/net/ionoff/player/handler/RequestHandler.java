package net.ionoff.player.handler;

import com.google.gson.Gson;
import com.google.gson.JsonElement;
import net.ionoff.player.DeamonPlayer;
import net.ionoff.player.config.UserConfig;
import net.ionoff.player.connection.MqttRequestMessage;
import net.ionoff.player.connection.MqttResponseMessage;
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
import org.apache.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class RequestHandler {
	private static final Logger LOGGER = Logger.getLogger(RequestHandler.class.getName());

	private static final Gson GSON = new Gson();

	public static final String PLAYLEAF = "playleaf";
	public static final String PLAYNODE = "playnode";
	public static final String PLAYLIST = "playlist";

	private static final String CONTEXT_STATUS = "/requests/status";
	private static final String CONTEXT_PLAYLIST = "/requests/playlist";
	private static final String CONTEXT_BROWSE = "/requests/browse";
	private static final String CONTEXT_SCHEDULE = "/requests/scheduler";

	public static MqttResponseMessage handleRequest(MqttRequestMessage request) {
		final String context = request.getContext();
		if (CONTEXT_STATUS.equals(context)) {
			return handleStatusRequest(request);
		}
		if (CONTEXT_PLAYLIST.equals(context)) {
			return handlePlaylistRequest(request);
		}
		if (CONTEXT_BROWSE.equals(context)) {
			return handleBrowseRequest(request);
		}
		if (CONTEXT_SCHEDULE.equals(context)) {
			return handleScheduleRequest(request);
		}
		throw new UnknownContextException(context);
	}

	private static MqttResponseMessage handleScheduleRequest(MqttRequestMessage request) {
		Schedule schedule = new ScheduleRequestHandler().handleRequest(request);
		return new MqttResponseMessage("schedule", schedule);
	}

	private static MqttResponseMessage handleStatusRequest(MqttRequestMessage request) {
		String command = request.getCommand();
		final Status status = handleStatusRequest(command, request);
		return new MqttResponseMessage("status", status);
	}

	private static MqttResponseMessage handlePlaylistRequest(MqttRequestMessage request) {
		String command = request.getCommand();
		if ("pl_update".equals(command)) {
			updatePlaylist(request);
		}
		PlayList playlist = getPlayer().getPlaylist();
		return new MqttResponseMessage("playlist", playlist);
	}

	private static MqttResponseMessage handleBrowseRequest(MqttRequestMessage request) {
		List<MediaFile> files = getBrowse(request);
		return new MqttResponseMessage("files", files);
	}

	private static List<MediaFile> getBrowse(MqttRequestMessage request) {
		String command = request.getCommand();
		String dir = request.getAsString("dir");
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
		return handleStatusRequest(null, null);
	}

	private static Status handleStatusRequest(String command, MqttRequestMessage request) {
		if (command == null || request == null) {
			return getPlayer().loadStatus();
		}
		if ("pl_play".equals(command)) {
			pl_play(request);
		} else if ("in_play".equals(command)) {
			in_play(request);
		} else if ("in_enqueue".equals(command)) {
			in_enqueue(request, false);
		} else if ("pl_previous".equals(command)) {
			pl_previous(request);
		} else if ("pl_next".equals(command)) {
			pl_next(request);
		} else if ("pl_delete".equals(command)) {
			pl_delete(request);
		} else if ("pl_repeat".equals(command)) {
			pl_repeat(request);
		} else if ("seek".equals(command)) {
			seek(request);
		} else if ("pl_pause".equals(command)) {
			pl_pause(request);
		} else if ("pl_forceresume".equals(command)) {
			pl_forceresume(request);
		} else if ("pl_resume".equals(command)) {
			pl_resume(request);
		} else if ("pl_forcepause".equals(command)) {
			pl_forcepause(request);
		} else if ("pl_stop".equals(command)) {
			pl_stop(request);
		} else if ("pl_empty".equals(command)) {
			pl_empty(request);
		} else if ("volume".equals(command)) {
			volume(request);
		} else if ("pl_loop".equals(command)) {
			pl_loop(request);
		} else if ("pl_random".equals(command)) {
			pl_random(request);
		} else if ("fullscreen".equals(command)) {
			fullscreen(request);
		}
		return getPlayer().loadStatus();
	}

	private static void fullscreen(MqttRequestMessage request) {
		return;
	}

	private static void volume(MqttRequestMessage request) {
		String val = request.getAsString("val");
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

	private static void pl_repeat(MqttRequestMessage request) {
		getPlayer().plRepeat();
	}

	private static void pl_random(MqttRequestMessage request) {
		getPlayer().randomizePlay();
	}

	private static void pl_loop(MqttRequestMessage request) {
		// does nothing // ???
	}

	private static void pl_empty(MqttRequestMessage request) {
		getPlayer().emtyPlaylist();
		getPlayer().getPlaylist().setId(0L);
		getPlayer().getPlaylist().setName(null);
	}

	private static void pl_stop(MqttRequestMessage request) {
		getPlayer().stop();
	}

	private static void pl_forcepause(MqttRequestMessage request) {
		getPlayer().pause();
	}

	private static void pl_resume(MqttRequestMessage request) {
		getPlayer().play();
	}

	private static void pl_forceresume(MqttRequestMessage request) {
		getPlayer().play();
	}

	private static void pl_pause(MqttRequestMessage request) {
		getPlayer().pause();
	}

	private static void seek(MqttRequestMessage request) {
		String val = request.getAsString("val");
		if (val.equals("+")) {
			getPlayer().seekFw();
		} else if (val.equals("-")) {
			getPlayer().seekRw();
		}
	}

	private static void pl_delete(MqttRequestMessage request) {
		String id = request.getAsString("id");
		String type = request.getAsString("type");
		if (PLAYLEAF.equals(type)) {
			getPlayer().deleteLeaf(Long.parseLong(id));
		} else if (PLAYNODE.equals(type)) {
			getPlayer().deleteNode(Long.parseLong(id));
		}
	}

	private static void pl_next(MqttRequestMessage request) {
		getPlayer().playNext();
	}

	private static void pl_previous(MqttRequestMessage request) {
		getPlayer().playPrevious();
	}

	private static void in_enqueue(MqttRequestMessage request, boolean isPlay) {

		JsonElement input = request.getAsJsonElement("input");
		String inputType = request.getAsString("input_type");
		
		if (PlayNode.TYPE.album.toString().equals(inputType)) {
			Album album = GSON.fromJson(input, Album.class);
			addAlbumFile(album);
			getPlayer().inEnqueue(album, isPlay);
		} 
		else if (PlayNode.TYPE.dir.toString().equals(inputType)) {
			getPlayer().inEnqueue(input.getAsString(), true, isPlay);
		} 
		else if (MediaFile.TYPE.file.toString().equals(inputType)) { 
			getPlayer().inEnqueue(input.getAsString(), false, isPlay);
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

	private static void in_play(MqttRequestMessage request) {
		in_enqueue(request, true);
	}

	private static void addAlbumFile(Album album) {
		File albumFolder = new File(UserConfig.getInstance().getAlbumFolder());
		String albumFileName = getAlbumFileName(album);
		if (!albumFolder.exists()) {
			albumFolder.mkdirs();
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
		File[] files = albumFolder.listFiles();
		if (files == null || files.length == 0) {
			return false;
		}
		for (File file : files) {
			if (file.getName().equals(albumFileName)) {
				return true;
			}
		}
		return false;
	}

	private static String getAlbumFileName(Album album) {
		return AccentRemover.removeAccent(album.getTitle()) + "#" + album.getId() + ".album";
	}


	private static void pl_play(MqttRequestMessage request) {
		String id = request.getAsString("id");
		String type = request.getAsString("type");
		if (PLAYLEAF.equals(type)) {
			getPlayer().playPlaylistLeaf(Long.parseLong(id));
		} else if (PLAYNODE.equals(type)) {
			getPlayer().playPlaylistNode(Long.parseLong(id));
		} else {
			getPlayer().playPlaylist();
		}
	}
	
	private static void updatePlaylist(MqttRequestMessage request) {
 		JsonElement plJson = request.getAsJsonElement("playlist");
		if (plJson == null) {
			return;
		}
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
