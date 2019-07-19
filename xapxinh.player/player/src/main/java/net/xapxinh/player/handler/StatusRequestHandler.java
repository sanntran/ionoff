package net.xapxinh.player.handler;

import static net.xapxinh.player.Application.application;

import java.io.File;
import java.io.IOException;
import java.util.Map;

import com.google.gson.JsonElement;
import net.xapxinh.player.connection.MqttRequestMessage;
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

	public Status handleRequest(MqttRequestMessage request) {
		String command = request.getAsString("command");
		return handleRequest(command, request);
	}

	public Status handleRequest(String command, MqttRequestMessage request) {
		if (command == null) {
			return mediaPlayerPanel.getStatus();
		}
		if ("pl_play".equals(command)) {
			pl_play(request);
		}
		else if ("in_play".equals(command)) {
			in_play(request);
		}
		else if ("in_enqueue".equals(command)) {
			in_enqueue(request, false);
		}
		else if ("pl_previous".equals(command)) {
			pl_previous(request);
		}
		else if ("pl_next".equals(command)) {
			pl_next(request);
		}
		else if ("pl_delete".equals(command)) {
			pl_delete(request);
		}
		else if ("pl_repeat".equals(command)) {
			pl_repeat(request);
		}
		else if ("seek".equals(command)) {
			seek(request);
		}
		else if ("pl_pause".equals(command)) {
			pl_pause(request);
		}
		else if ("pl_forceresume".equals(command)) {
			pl_forceresume(request);
		}
		else if ("pl_resume".equals(command)) {
			pl_resume(request);
		}
		else if ("pl_forcepause".equals(command)) {
			pl_forcepause(request);
		}
		else if ("pl_stop".equals(command)) {
			pl_stop(request);
		}
		else if ("pl_empty".equals(command)) {
			pl_empty(request);
		}	
		else if ("volume".equals(command)) {
			volume(request);
		}
		else if ("pl_loop".equals(command)) {
			pl_loop(request);
		}
		else if ("pl_repeat".equals(command)) {
			pl_repeat(request);
		}
		else if ("pl_random".equals(command)) {
			pl_random(request);
		}
		else if ("fullscreen".equals(command)) {
			fullscreen(request);
		}
		return mediaPlayerPanel.getStatus();
	}

	private void fullscreen(MqttRequestMessage request) {
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
	
	private void volume(MqttRequestMessage request) {
		String val = request.getAsString("val");
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
	
	private void pl_repeat(MqttRequestMessage request) {
		mediaPlayerPanel.repeatPlaylist();
	}

	private void pl_random(MqttRequestMessage request) {
		mediaPlayerPanel.randomPlaylist();
	}

	private void pl_loop(MqttRequestMessage request) {
		mediaPlayerPanel.loopPlaylist();
	}

	private void pl_empty(MqttRequestMessage request) {
		mediaPlayerPanel.emptyPlaylist();
		mediaPlayerPanel.getPlaylist().setId(0L);
		mediaPlayerPanel.getPlaylist().setName(null);
	}

	private void pl_stop(MqttRequestMessage request) {
		mediaPlayerPanel.stop();
	}

	private void pl_forcepause(MqttRequestMessage request) {
		mediaPlayerPanel.pause();
	}

	private void pl_resume(MqttRequestMessage request) {
		mediaPlayerPanel.resume();
	}

	private void pl_forceresume(MqttRequestMessage request) {
		mediaPlayerPanel.resume();
	}

	private void pl_pause(MqttRequestMessage request) {
		mediaPlayerPanel.pause();
	}

	private void seek(MqttRequestMessage request) {
		String val = request.getAsString("val");
		if (val.equals("+")) {
			mediaPlayerPanel.seekForward();
		}
		else if (val.equals("-")) {
			mediaPlayerPanel.seekReward();
		}
	}

	private void pl_delete(MqttRequestMessage request) {
		String id = request.getAsString("id");
		String type = request.getAsString("type");
		if (PLAYLEAF.equals(type)) {
			mediaPlayerPanel.removePlaylistLeaf(Long.parseLong(id));
		}
		else if (PLAYNODE.equals(type)) {
			mediaPlayerPanel.removePlaylistNode(Long.parseLong(id));
		}
	}

	private void pl_next(MqttRequestMessage request) {
		mediaPlayerPanel.getMediaListPlayer().playNext();
	}

	private void pl_previous(MqttRequestMessage request) {
		mediaPlayerPanel.getMediaListPlayer().playPrevious();
	}
	
	private void in_enqueue(MqttRequestMessage request, boolean isPlay) {
		JsonElement input = request.getAsJsonElement("input");
		String inputType = request.getAsString("input_type");
		if (PlayNode.TYPE.album.toString().equals(inputType)) {
			Album album = gson.fromJson(input, Album.class);
			addAlbumFile(album);
			mediaPlayerPanel.inEnqueue(album, isPlay);
		}
		else if (PlayNode.TYPE.dir.toString().equals(inputType) 
				|| PlayLeaf.TYPE.file.toString().equals(inputType)) {
			mediaPlayerPanel.inEnqueue(createMediaFile(input.getAsString()), isPlay);
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

	private void in_play(MqttRequestMessage request) {
		in_enqueue(request, true);
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

	private void pl_play(MqttRequestMessage request) {
		String id = request.getAsString("id");
		String type = request.getAsString("type");
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
