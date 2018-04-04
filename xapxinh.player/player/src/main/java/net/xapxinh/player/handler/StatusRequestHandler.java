package net.xapxinh.player.handler;

import static net.xapxinh.player.Application.application;

import java.io.File;
import java.io.IOException;
import java.util.Map;

import org.apache.log4j.Logger;

import com.google.gson.Gson;

import net.xapxinh.player.AppProperties;
import net.xapxinh.player.EmbeddedMediaPlayerPanel;
import net.xapxinh.player.config.UserConfig;
import net.xapxinh.player.event.VolumeChangedEvent;
import net.xapxinh.player.model.Album;
import net.xapxinh.player.model.MediaFile;
import net.xapxinh.player.model.PlayLeaf;
import net.xapxinh.player.model.PlayList;
import net.xapxinh.player.model.PlayNode;
import net.xapxinh.player.model.Song;
import net.xapxinh.player.model.Status;
import net.xapxinh.player.model.YoutubeVideo;

public class StatusRequestHandler {
	
	
	public static final String PLAYLEAF = "playleaf";
	public static final String PLAYNODE = "playnode";
	public static final String PLAYLIST = "playlist";
	
	private static final Logger LOGGER = Logger.getLogger(StatusRequestHandler.class.getName());
	private final EmbeddedMediaPlayerPanel mediaPlayerPanel;
	private final Gson gson;
	
	public StatusRequestHandler(EmbeddedMediaPlayerPanel mediaPlayerPanel) {
		gson = new Gson();
		this.mediaPlayerPanel = mediaPlayerPanel;
	}

	public Status handleRequest(Map<String, String> parameters) {
		String command = parameters.get("command");
		return handleRequest(command, parameters);
	}
	
	private Status handleRequest(String command, Map<String, String> parameters) {
		if (command == null || parameters.isEmpty()) {
			return mediaPlayerPanel.getStatus();
		}
		if ("pl_play".equals(command)) {
			pl_play(parameters);
		}
		else if ("in_play".equals(command)) {
			in_play(parameters);
		}
		else if ("in_enqueue".equals(command)) {
			in_enqueue(parameters, false);
		}
		else if ("pl_previous".equals(command)) {
			pl_previous(parameters);
		}
		else if ("pl_next".equals(command)) {
			pl_next(parameters);
		}
		else if ("pl_delete".equals(command)) {
			pl_delete(parameters);
		}
		else if ("pl_repeat".equals(command)) {
			pl_repeat(parameters);
		}
		else if ("seek".equals(command)) {
			seek(parameters);
		}
		else if ("pl_pause".equals(command)) {
			pl_pause(parameters);
		}
		else if ("pl_forceresume".equals(command)) {
			pl_forceresume(parameters);
		}
		else if ("pl_resume".equals(command)) {
			pl_resume(parameters);
		}
		else if ("pl_forcepause".equals(command)) {
			pl_forcepause(parameters);
		}
		else if ("pl_stop".equals(command)) {
			pl_stop(parameters);
		}
		else if ("pl_empty".equals(command)) {
			pl_empty(parameters);
		}	
		else if ("volume".equals(command)) {
			volume(parameters);
		}
		else if ("pl_loop".equals(command)) {
			pl_loop(parameters);
		}
		else if ("pl_repeat".equals(command)) {
			pl_repeat(parameters);
		}
		else if ("pl_random".equals(command)) {
			pl_random(parameters);
		}
		else if ("fullscreen".equals(command)) {
			fullscreen(parameters);
		}
		return mediaPlayerPanel.getStatus();
	}

	private void fullscreen(Map<String, String> parameters) {
		if (mediaPlayerPanel.getMediaPlayer().isFullScreen()) {
			mediaPlayerPanel.getMediaPlayer().toggleFullScreen();
		}
		else {
			try {
				maximazeWindow();
				Thread.sleep(500);
			} catch (IOException e) {
				//
			} catch (InterruptedException e) {
				//
			}
			mediaPlayerPanel.getMediaPlayer().toggleFullScreen();
		}
	}
	
	private void maximazeWindow() throws IOException {
		String run = AppProperties.getNirCmdPath() + " win max process javaw.exe";
		Runtime.getRuntime().exec(run);
	}
	
	private void volume(Map<String, String> parameters) {
		String val = parameters.get("val");
		if (val.equals("mute")) {
			volumeMute();
		}
		else if (val.equals("+")) {
			volumeUp();
		}
		else if (val.equals("-")) {
			volumeDown();
		}
		else {
			volumeSet(Integer.parseInt(val));
		}
	}

	private void volumeSet(int val) {
		mediaPlayerPanel.getMediaPlayer().setVolume(val);
		application().post(VolumeChangedEvent.instance());
	}

	private void volumeDown() {
		int vol = mediaPlayerPanel.getMediaPlayer().getVolume();
		volumeSet(vol - 5);
	}

	private void volumeUp() {
		int vol = mediaPlayerPanel.getMediaPlayer().getVolume();
		volumeSet(vol + 5);
	}

	private void volumeMute() {
		mediaPlayerPanel.getMediaPlayer().setVolume(0);
	}
	
	private void pl_repeat(Map<String, String> parameters) {
		mediaPlayerPanel.repeatPlaylist();
	}

	private void pl_random(Map<String, String> parameters) {
		mediaPlayerPanel.randomPlaylist();
	}

	private void pl_loop(Map<String, String> parameters) {
		mediaPlayerPanel.loopPlaylist();
	}

	private void pl_empty(Map<String, String> parameters) {
		mediaPlayerPanel.emptyPlaylist();
		mediaPlayerPanel.getPlaylist().setId(0L);
		mediaPlayerPanel.getPlaylist().setName(null);
	}

	private void pl_stop(Map<String, String> parameters) {
		mediaPlayerPanel.stop();
	}

	private void pl_forcepause(Map<String, String> parameters) {
		mediaPlayerPanel.pause();
	}

	private void pl_resume(Map<String, String> parameters) {
		mediaPlayerPanel.resume();
	}

	private void pl_forceresume(Map<String, String> parameters) {
		mediaPlayerPanel.resume();
	}

	private void pl_pause(Map<String, String> parameters) {
		mediaPlayerPanel.pause();
	}

	private void seek(Map<String, String> parameters) {
		String val = parameters.get("val");
		if (val.equals("+")) {
			mediaPlayerPanel.seekForward();
		}
		else if (val.equals("-")) {
			mediaPlayerPanel.seekReward();
		}
	}

	private void pl_delete(Map<String, String> parameters) {
		String id = parameters.get("id");
		String type = parameters.get("type");
		if (PLAYLEAF.equals(type)) {
			mediaPlayerPanel.removePlaylistLeaf(Long.parseLong(id));
		}
		else if (PLAYNODE.equals(type)) {
			mediaPlayerPanel.removePlaylistNode(Long.parseLong(id));
		}
	}

	private void pl_next(Map<String, String> parameters) {
		mediaPlayerPanel.getMediaListPlayer().playNext();
	}

	private void pl_previous(Map<String, String> parameters) {
		mediaPlayerPanel.getMediaListPlayer().playPrevious();
	}
	
	private void in_enqueue(Map<String, String> parameters, boolean isPlay) {
		String input = parameters.get("input");
		String inputType = parameters.get("input_type");
		if (PlayNode.TYPE.album.toString().equals(inputType)) {
			Album album = gson.fromJson(input, Album.class);
			addAlbumFile(album);
			mediaPlayerPanel.inEnqueue(album, isPlay);
		}
		else if (PlayNode.TYPE.dir.toString().equals(inputType) 
				|| PlayLeaf.TYPE.file.toString().equals(inputType)) {
			mediaPlayerPanel.inEnqueue(createMediaFile(input), isPlay);
		}
		else if (PlayNode.TYPE.youtube.toString().equals(inputType)) {
			YoutubeVideo video = gson.fromJson(input, YoutubeVideo.class);
			mediaPlayerPanel.inEnqueue(video, isPlay);
		}
		else if (PlayLeaf.TYPE.track.toString().equals(inputType)) {
			Song song = gson.fromJson(input, Song.class);
			mediaPlayerPanel.inEnqueue(song, isPlay);
		}
		else if (PLAYLIST.equals(inputType)) {
			PlayList playlist = gson.fromJson(input, PlayList.class);
			mediaPlayerPanel.inEnqueue(playlist, isPlay);
		}
		else if (PLAYNODE.equals(inputType)) {
			PlayNode playnode = gson.fromJson(input, PlayNode.class);
			mediaPlayerPanel.inEnqueue(playnode, isPlay);
		}
		else if (PLAYLEAF.equals(inputType)) {
			PlayLeaf playleaf = gson.fromJson(input, PlayLeaf.class);
			mediaPlayerPanel.inEnqueue(playleaf, isPlay);
		}
	}

	private void in_play(Map<String, String> parameters) {
		in_enqueue(parameters, true);
	}

	private void addAlbumFile(Album album) {
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

	private boolean hasAlbum(File albumFolder, String albumFileName) {
		for (File file : albumFolder.listFiles()) {
			if (file.getName().equals(albumFileName)) {
				return true;
			}
		}
		return false;
	}

	private String getAlbumFileName(Album album) {
		return album.getTitle() + "#" + album.getId() + ".album";
	}

	private MediaFile createMediaFile(String input) {
		MediaFile mediaFile = new MediaFile();
		File file = new File(input);
		if (!file.exists()) {
			return mediaFile;
		}
		mediaFile.setPath(input);
		if (file.isDirectory()) {
			mediaFile.setType(MediaFile.TYPE.dir.toString());
		}
		else {
			mediaFile.setType(MediaFile.TYPE.file.toString());
		}
		mediaFile.setName(file.getName());
		return mediaFile;
	}

	private void pl_play(Map<String, String> parameters) {
		String id = parameters.get("id");
		String type = parameters.get("type");
		if (PLAYLEAF.equals(type)) {
			mediaPlayerPanel.playPlaylistLeaf(Long.parseLong(id));
		}
		else if (PLAYNODE.equals(type)) {
			mediaPlayerPanel.playPlaylistNode(Long.parseLong(id));
		}
		else {
			mediaPlayerPanel.playPlaylist();
		}
	}
}
