package net.ionoff.player;

import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.apache.log4j.Logger;
import org.bff.javampd.command.MPDCommandExecutor;
import org.bff.javampd.file.MPDFile;
import org.bff.javampd.playlist.PlaylistChangeEvent;
import org.bff.javampd.playlist.PlaylistChangeListener;
import org.bff.javampd.server.MPD;
import org.bff.javampd.song.MPDSong;

import net.ionoff.player.config.AppConfig;
import net.ionoff.player.config.UserConfig;
import net.ionoff.player.exception.MpdConnectException;
import net.ionoff.player.model.Album;
import net.ionoff.player.model.MediaFile;
import net.ionoff.player.model.PlayLeaf;
import net.ionoff.player.model.PlayList;
import net.ionoff.player.model.PlayNode;
import net.ionoff.player.model.Song;
import net.ionoff.player.model.State;
import net.ionoff.player.model.Status;
import net.ionoff.player.model.YoutubeVideo;

public class DeamonPlayer {
	
	private static final Logger LOGGER = Logger.getLogger(DeamonPlayer.class.getName());
	
	private MPD mpd;
	private int volume;
	private Status status;
	private PlayList playlist;
	private long playlistVersion;

	private List<MPDSong> songs;
	private final MPDCommandExecutor commandExecutor;
	
	public DeamonPlayer() throws MpdConnectException {
		this.playlist = new PlayList();
		playlist.setNodes(new ArrayList<PlayNode>());
		try {
			mpd = new MPD.Builder().server(AppConfig.getInstance().MPD_HOST)
					.port(AppConfig.getInstance().MPD_PORT).build();
			commandExecutor = new MPDCommandExecutor();
			commandExecutor.setMpd(mpd);
			
			mpd.getPlaylist().addPlaylistChangeListener(new PlaylistChangeListener() {
				@Override
				public void playlistChanged(PlaylistChangeEvent event) {
					songs = mpd.getPlaylist().getSongList();
					playlistVersion = mpd.getPlaylist().getVersion();
				}
			});
			mpd.getPlaylist().clearPlaylist();
		} catch (Exception e) {
			throw new MpdConnectException(AppConfig.getInstance().MPD_HOST);
		}
	}

	public Status getStatus() {
		return status;
	}

	public PlayList getPlaylist() {
		if (playlistVersion != mpd.getPlaylist().getVersion()) {
			playlist.getNodes().clear();
			mpd.getPlaylist().clearPlaylist();
		}
		return playlist;
	}

	public Status loadStatus() {
		if (status == null) {
			status = new Status();
		}
		status.setState(toState(mpd.getPlayer().getStatus()));
		status.setTime(mpd.getPlayer().getElapsedTime());
		status.setLength(mpd.getPlayer().getTotalTime());
		int mpdVolume = mpd.getPlayer().getVolume();
		if (volume == 0) {
			volume = mpdVolume;
		}
		status.setVolume(volume);
		if (status.getLength() == 0) {
			status.setPosition(0f);
		}
		else {
			status.setPosition((float) status.getTime() / status.getLength());
		}
		status.setFullscreen(false);
		status.setRandom(mpd.getPlayer().isRandom());
		status.setRepeat(mpd.getPlayer().isRepeat());
		status.setLoop(false);

		MPDSong currentSong = mpd.getPlayer().getCurrentSong();
		if (currentSong == null) {
			status.setPlayNode(null);
		} else {
			PlayNode currentNode = getCurrentNode(mpd, currentSong);
			status.setPlayNode(currentNode);
		}
		
		return status;
	}

	private PlayNode getCurrentNode(MPD mpd, MPDSong currentSong) {
		for (PlayLeaf leaf : playlist.getLeafs()) {
			leaf.setCurrent(false);
		}
		if (currentSong == null) {
			return null;
		}
		PlayNode currentNode = newPlaylistNode();
		PlayLeaf leaf = getPlaylistLeaf(currentSong);
		leaf.setCurrent(true);
		currentNode.getLeafs().add(leaf);
		for (PlayNode node : playlist.getNodes()) {
			for (PlayLeaf l : node.getLeafs()) {
				if (l.getIdx() == leaf.getIdx()) {
					currentNode.setType(node.getType());
					currentNode.setImage(node.getImage());
					currentNode.setName(node.getName());
				}
			}
		}
		return currentNode;
	}

	private PlayLeaf getPlaylistLeaf(MPDSong currentSong) {
		if (playlist == null) {
			return null;
		}
		for (PlayLeaf leaf : playlist.getLeafs()) {
			if (leaf.getMrl().equals(currentSong.getFile())) {
				return leaf;
			}
		}
		return null;
	}

	private PlayNode newPlaylistNode() {
		PlayNode node = new PlayNode();
		node.setLeafs(new ArrayList<PlayLeaf>());
		return node;
	}

	private String toState(org.bff.javampd.player.Player.Status status) {
		if (org.bff.javampd.player.Player.Status.STATUS_PLAYING.equals(status)) {
			return State.playing.toString();
		} else if (org.bff.javampd.player.Player.Status.STATUS_PAUSED.equals(status)) {
			return State.paused.toString();
		}
		return State.stopped.toString();
	}

	public void playPlaylistLeaf(long leafIdx) {
		PlayLeaf leaf = playlist.getLeaf(leafIdx);
		if (leaf == null || songs == null || songs.isEmpty()) {
			return;
		}
		MPDSong leafSong = getMPDSong(leaf);
		if (leafSong != null) {
			mpd.getPlayer().playSong(leafSong);
		}
	}

	private MPDSong getMPDSong(PlayLeaf leaf) {
		for (MPDSong song : songs) {
			if (song.getFile().equals(leaf.getMrl())) {
				return song;
			}
		}
		return null;
	}

	public void playPlaylistNode(long nodeIdx) {
		PlayNode node = playlist.getNode(nodeIdx);
		if (node == null || !node.hasLeaf() || songs == null || songs.isEmpty()) {
			return;
		}
		for (MPDSong song : songs) {
			if (song.getFile().equals(node.getLeafs().get(0).getMrl())) {
				mpd.getPlayer().playSong(song);
			}
		}
	}

	public void playPlaylist() {
		mpd.getPlayer().play();
	}

	public void inEnqueue(Album album, boolean isPlay) {
		PlayNode newNode = createNode(album);
		addPlaylistNode(newNode);
		List<MPDSong> newSongs = new ArrayList<>();
		for (Song song : album.getSongs()) {
			MPDSong mpdSong = new MPDSong(song.getUrl(), song.getName());
			newSongs.add(mpdSong);
		}
		mpd.getPlaylist().addSongs(newSongs);
		if (isPlay) {
			playPlaylistNode(newNode.getIdx());
		}
	}

	private void addPlaylistNode(PlayNode node) {
		if (playlist.hasNode(node)) {
			return;
		}
		long maxNodeIdx = playlist.getNodeIndex();
		node.setIdx(maxNodeIdx + 1);
		long maxLeafId = playlist.getLeafIndex();
		int nodeLeafs = node.getLeafs().size();
		for (int i = 0; i < nodeLeafs; i++) {
			long leafIdx = maxLeafId + i + 1;
			node.getLeafs().get(i).setIdx(leafIdx);
			playlist.setLeafIndex(leafIdx);
		}
		playlist.getNodes().add(node);
		playlist.setNodeIndex(node.getIdx());
	}

	private PlayNode createNode(Album album) {
		PlayNode node = new PlayNode();
		node.setName(album.getTitle());
		node.setType(PlayNode.TYPE.album.toString());
		node.setImage(album.getImage());
		List<PlayLeaf> leafs = new ArrayList<PlayLeaf>();
		for (Song albumItem : album.getSongs()) {
			leafs.add(createLeaf(albumItem));
		}
		node.setLeafs(leafs);
		return node;
	}

	private PlayLeaf createLeaf(Song song) {
		PlayLeaf leaf = new PlayLeaf();
		leaf.setName(song.getTitle());
		leaf.setType(PlayLeaf.TYPE.track.toString());
		leaf.setImage(song.getImage());
		leaf.setMrl(song.getUrl());
		leaf.setArtists(song.getArtists());
		leaf.setAuthor(song.getAuthor());
		return leaf;
	}

	private PlayNode createNode(String dir, Collection<MPDFile> files) {
		PlayNode node = new PlayNode();
		node.setName(dir);
		node.setType(PlayNode.TYPE.dir.toString());
		List<PlayLeaf> leafs = new ArrayList<PlayLeaf>();
		for (MPDFile file : files) {
			leafs.add(createLeaf(file.getPath()));
		}
		node.setLeafs(leafs);
		return node;
	}

	private PlayNode createNode(String file) {
		PlayNode node = new PlayNode();
		if (file.contains("/")) {
			node.setName(file.substring(0, file.lastIndexOf("/")));
		}
		else {
			node.setName("/");
		}
		node.setType(PlayNode.TYPE.dir.toString());
		List<PlayLeaf> leafs = new ArrayList<PlayLeaf>();
		leafs.add(createLeaf(file));
		node.setLeafs(leafs);
		return node;
	}

	private PlayLeaf createLeaf(String file) {
		PlayLeaf leaf = new PlayLeaf();
		leaf.setName(file);
		leaf.setMrl(file);
		leaf.setType(PlayLeaf.TYPE.file.toString());
		return leaf;
	}

	public void inEnqueue(String file, boolean isDirectory, boolean isPlay) {
		PlayNode newNode;
		if (isDirectory) {
			MPDFile dir = new MPDFile(file);
			dir.setDirectory(true);
			Collection<MPDFile> files = mpd.getMusicDatabase().getFileDatabase().listDirectory(dir);
			newNode = createNode(file, files);
			addPlaylistNode(newNode);
		} else {
			newNode = createNode(file);
			addPlaylistNode(newNode);
		}
		List<MPDSong> newSongs = new ArrayList<>();
		for (PlayLeaf leaf : newNode.getLeafs()) {
			MPDSong mpdSong = new MPDSong(leaf.getMrl(), leaf.getName());
			newSongs.add(mpdSong);
		}
		mpd.getPlaylist().addSongs(newSongs);
		if (isPlay) {
			playPlaylistNode(newNode.getIdx());
		}
	}
	
	public void inEnqueue(YoutubeVideo video, boolean isPlay) {
		PlayNode newNode = createNode(video);
		addPlaylistNode(newNode);
		List<MPDSong> newSongs = new ArrayList<>();
		for (PlayLeaf leaf : newNode.getLeafs()) {
			MPDSong mpdSong = new MPDSong(leaf.getMrl(), leaf.getName());
			newSongs.add(mpdSong);
		}
		mpd.getPlaylist().addSongs(newSongs);
		if (isPlay) {
			playPlaylistNode(newNode.getIdx());
		}
	}

	public void inEnqueue(PlayList playlist, boolean isPlay) {
		emtyPlaylist();
		
		PlayList playingList = getPlaylist();
		playingList.setId(playlist.getId());
		playingList.setName(playlist.getName());
		if (playlist.getNodes().isEmpty()) {
			return;
		}
		for (PlayNode node : playlist.getNodes()) {
			addPlaylistNode(node);
			List<MPDSong> newSongs = new ArrayList<>();
			for (PlayLeaf leaf : node.getLeafs()) {
				if (PlayLeaf.TYPE.youtube.toString().equals(leaf.getType())) {
					leaf.setMrl(getYoutubeMrl(leaf.getUrl()));
				}
				MPDSong mpdSong = new MPDSong(leaf.getMrl(), leaf.getName());
				newSongs.add(mpdSong);
			}
			mpd.getPlaylist().addSongs(newSongs);
		}
		if (isPlay) {
			playPlaylistNode(playingList.getNodes().get(0).getIdx());
		}
	}

	private PlayNode createNode(YoutubeVideo youtubeVideo) {
		PlayNode node = new PlayNode();
		node.setType(PlayNode.TYPE.youtube.toString());
		List<PlayLeaf> leafs = new ArrayList<PlayLeaf>();
		PlayLeaf leaf = new PlayLeaf();
		leaf.setName(youtubeVideo.getTitle());
		leaf.setType(PlayLeaf.TYPE.youtube.toString());
		leaf.setUrl(youtubeVideo.getId());
		leaf.setMrl(youtubeVideo.getMrl());
		leafs.add(leaf);
		node.setLeafs(leafs);
		return node;
	}

	public void playNext() {
		mpd.getPlayer().playNext();
	}

	public void plRepeat() {
		if (mpd.getPlayer().isRepeat()) {
			mpd.getPlayer().setRepeat(false);
		}
		else {
			mpd.getPlayer().setRepeat(true);
		}
	}

	public void seekFw() {
		mpd.getPlayer().seek(mpd.getPlayer().getElapsedTime() + 10);
	}

	public void seekRw() {
		mpd.getPlayer().seek(mpd.getPlayer().getElapsedTime() - 10);
	}

	public void pause() {
		mpd.getPlayer().pause();
	}

	public void play() {
		mpd.getPlayer().play();
	}

	public void stop() {
		mpd.getPlayer().stop();
	}

	public void mute() {
		mpd.getPlayer().mute();
	}

	public void setVolume(int vol) {
		this.volume = vol;
		mpd.getPlayer().setVolume(vol);
	}

	public int getVolume() {
		if (volume == 0) {
			volume = mpd.getPlayer().getVolume();
		}
		return this.volume;
	}

	public void emtyPlaylist() {
		playlist.getNodes().clear();
		mpd.getPlaylist().clearPlaylist();
	}

	public void randomizePlay() {
		if (mpd.getPlayer().isRandom()) {
			mpd.getPlayer().unRandomizePlay();
		}
		else {
			mpd.getPlayer().randomizePlay();
		}
	}

	public void playPrevious() {
		mpd.getPlayer().playPrevious();
	}

	public void deleteLeaf(long leafId) {
		PlayLeaf leaf = playlist.getLeaf(leafId);
		if (leaf != null) {
			MPDSong song = getMPDSong(leaf);
			if (song != null) {
				mpd.getPlaylist().removeSong(song);
			}
			for (PlayNode node : playlist.getNodes()) {
				for (PlayLeaf l : node.getLeafs()) {
					if (l.getIdx() == leaf.getIdx()) {
						node.getLeafs().remove(l);
						if (node.getLeafs().isEmpty()) {
							playlist.getNodes().remove(node);
						}
						break;
					}
				}
			}
		}
	}

	public void deleteNode(long nodeId) {
		for (PlayNode node : playlist.getNodes()) {
			if (node.getIdx() == nodeId) {
				for (int i = 0; i < node.getLeafs().size();) {
					MPDSong song = getMPDSong(node.getLeafs().get(i));
					if (song != null) {
						mpd.getPlaylist().removeSong(song);
					}
					node.getLeafs().remove(i);
				}
				break;
			}
		}
	}

	public List<MediaFile> browseFiles(String dir) {
		MPDFile folder = new MPDFile(dir);
		folder.setDirectory(true);
		Collection<MPDFile> files = mpd.getMusicDatabase().getFileDatabase().listDirectory(folder);
		return getMediaFiles(dir, files);
	}

	private static List<MediaFile> getMediaFiles(String dir, Collection<MPDFile> files) {
		
		List<MediaFile> mediaFiles = new ArrayList<>();
		if (dir == null || dir.isEmpty()) {
			// no parent
		}
		else {
			MediaFile parent = new MediaFile();
			parent.setName("..");
			parent.setType(MediaFile.TYPE.dir.toString());
			mediaFiles.add(parent);
			parent.setPath(dir + "/..");
		}
		
		for (MPDFile f : files) {
			MediaFile mFile = new MediaFile();
			mFile.setName(f.getPath());
			mFile.setPath(f.getPath());
			if (f.isDirectory()) {
				mFile.setType(MediaFile.TYPE.dir.toString());
			} else {
				mFile.setType(MediaFile.TYPE.file.toString());
			}
			if (!".albums".equals(mFile.getName())) {
				mediaFiles.add(mFile);
			}
		}
		return mediaFiles;
	}

	public void updateFileDatabase() {
		commandExecutor.sendCommand("update");
	}

	public void inEnqueue(PlayNode playNode, boolean isPlay) {
		addPlaylistNode(playNode);
		List<MPDSong> newSongs = new ArrayList<>();
		for (PlayLeaf leaf : playNode.getLeafs()) {
			if (PlayLeaf.TYPE.youtube.toString().equals(leaf.getType())) {
				leaf.setMrl(getYoutubeMrl(leaf.getUrl()));
			}
			MPDSong mpdSong = new MPDSong(leaf.getMrl(), leaf.getName());
			newSongs.add(mpdSong);
		}
		mpd.getPlaylist().addSongs(newSongs);
		if (isPlay) {
			playPlaylistNode(playNode.getIdx());
		}
	}

	public void inEnqueue(PlayLeaf playLeaf, boolean isPlay) {
		PlayNode newNode = newPlaylistNode();
		if (PlayLeaf.TYPE.file.toString().equals(playLeaf.getType())) {
			newNode.setType(PlayNode.TYPE.dir.toString());
		}
		else if (PlayLeaf.TYPE.track.toString().equals(playLeaf.getType())) {
			newNode.setType(PlayNode.TYPE.album.toString());
		}
		else if (PlayLeaf.TYPE.youtube.toString().equals(playLeaf.getType())) {
			playLeaf.setMrl(getYoutubeMrl(playLeaf.getUrl()));
			newNode.setType(PlayNode.TYPE.youtube.toString());
		}
		newNode.getLeafs().add(playLeaf);
		inEnqueue(newNode, isPlay);
	}

	public void inEnqueue(Song song, boolean isPlay) {
		PlayNode newNode = newPlaylistNode();
		newNode.setType(PlayNode.TYPE.album.toString());
		PlayLeaf leaf = createLeaf(song);
		newNode.getLeafs().add(leaf);
		inEnqueue(newNode, isPlay);
	}	

	private String getYoutubeMrl(String youtubeId) {
		try {
			String mrl = HttpRequestUtil.sendHttpGETRequest(AppConfig.getInstance().DATA_SERVER_URL 
					+ "videos/" + youtubeId + "/audiourl?mac=" + UserConfig.getInstance().LICENSE_KEY);
			return mrl;
		}
		catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			return null;
		}
	}
}
