package net.xapxinh.player;

import static net.xapxinh.player.Application.application;

import java.util.ArrayList;

import net.xapxinh.player.thread.PlayLeafDownloader;
import org.apache.log4j.Logger;

import net.xapxinh.player.config.AppConfig;
import net.xapxinh.player.event.ErrorEvent;
import net.xapxinh.player.event.FinishedEvent;
import net.xapxinh.player.model.PlayLeaf;
import net.xapxinh.player.model.PlayList;
import net.xapxinh.player.model.PlayNode;
import net.xapxinh.player.model.State;
import uk.co.caprica.vlcj.player.embedded.EmbeddedMediaPlayer;

public class EmbeddedMediaListPlayer {
	
	private static final Logger LOGGER = Logger.getLogger(EmbeddedMediaListPlayer.class.getName());
	
	private boolean random;
	private PlaylistMode mode;
	private PlayLeaf current;
	private final PlayList playlist;
	private final EmbeddedMediaPlayer mediaPlayer;
	private State state;
	 
	EmbeddedMediaListPlayer(EmbeddedMediaPlayer mediaPlayer) {
		this.mediaPlayer = mediaPlayer;
		mediaPlayer.setRepeat(true);
		playlist = new PlayList();
		playlist.setNodes(new ArrayList<PlayNode>());
		setMode(PlaylistMode.DEFAULT);
	}
	
	PlaylistMode getMode() {
		return mode;
	}

	void setMode(PlaylistMode mode) {
		this.mode = mode;
	}
	
	void enqueueNode(PlayNode node, boolean isPlay) {
		addNode(node);
		if (isPlay) {
			if (!node.hasLeaf()) {
				playLeaf(null);
			}
			playLeaf(node.getLeafs().get(0));
		}
	}
	
	void onPlaying() {
		state = State.playing;
		if (current != null && !current.isCurrent()) {
			current.setCurrent(true);
		}
	}
	
	void onStopped() {
		state = State.stopped;
		playlist.resetCurrent();
	}

	private PlayLeaf getNext(boolean autoPlay) {
		if (playlist.isEmpty()) {
			return null;
		}
		if (PlaylistMode.LOOP.toString().equals(mode) && playlist.isFinished()) {
			playlist.resetPlayed();
		}
		if (PlaylistMode.REPEAT.toString().equals(mode) && autoPlay) {
			return current;
		}
		if (random) {
			if (!autoPlay && playlist.getRandomUnPlayedLeaf() == null) {
				playlist.resetPlayed();
			}
			return playlist.getRandomUnPlayedLeaf();
		}
		else {
			long id = 0;
			if (current != null) {
				id = current.getIdx();
			}
			if (!autoPlay && playlist.getDefaultUnPlayedLeaf(id) == null) {
				playlist.resetPlayed();
			}
			return playlist.getDefaultUnPlayedLeaf(id);
		}
	}
	
	private void addNode(PlayNode node) {
		if (playlist.hasNode(node)) {
			return;
		}
		long maxNodeIdx = playlist.getNodeIndex();
		node.setIdx(maxNodeIdx + 1);
		long maxLeafIdx = playlist.getLeafIndex();
		int nodeLeafs = node.getLeafs().size();
		for (int i = 0; i < nodeLeafs; i++) {
			long leafIdx = maxLeafIdx + i + 1;
			node.getLeafs().get(i).setIdx(leafIdx);
			playlist.setLeafIndex(leafIdx);
		}
		playlist.getNodes().add(node);
		playlist.setNodeIndex(node.getIdx());
	}

	void setRandom(boolean random) {
		this.random = random;
	}
	
	boolean isRandom() {
		return random;
	}
	
	boolean isRepeat() {
		return PlaylistMode.REPEAT.equals(mode);
	}
	
	boolean isLoop() {
		return PlaylistMode.LOOP.equals(mode);
	}

	public PlayList getPlaylist() {
		return playlist;
	}

	public void playNext() {
		if (current != null) {
			current.setPlayed(true);
		}
		playLeaf(getNext(false));
	}

	public void playPrevious() {
		if (current != null) {
			current.setPlayed(false);
		}
		playLeaf(getPrev(false));
	}
	
	public PlayLeaf getCurent() {
		return current;
	}

	private PlayLeaf getPrev(boolean autoPlay) {
		if (playlist.isEmpty()) {
			return null;
		}
		if (PlaylistMode.LOOP.toString().equals(mode) && playlist.isFinished()) {
			playlist.resetPlayed();
		}
		if (PlaylistMode.REPEAT.toString().equals(mode) && autoPlay) {
			return current;
		}
		if (random) {
			return playlist.getRandomPlayedLeaf();
		}
		else {
			return playlist.getDefaultPlayedLeaf();
		}
	}

	public void removeLeaf(long leafIdx) {
		if (current != null && current.getIdx() == leafIdx) {
			stop();
		}
		playlist.removeLeaf(leafIdx);
	}
	

	public void removeNode(long nodeId) {
		PlayNode node = playlist.getNode(nodeId);
		if (node != null && node.isCurrent()) {
			stop();
		}
		playlist.removeNode(nodeId);
	}
	

	public void mediaChanged() {
		if (State.opening.equals(state) || State.stopped.equals(state)) {
			mediaPlayer.play();
		}
	}

	public void onFinished() {
		state = State.finished;
		application().post(FinishedEvent.instance());
		current.setPlayed(true);
		playLeaf(getNext(true));
	}

	public void onError() {
		state = State.error;
		if (current != null && current.isYoutube()) {
			return; // don't care this case
		}
		application().post(ErrorEvent.instance());
		current.setPlayed(true);
		playLeaf(getNext(true));
	}
	
	public void playLeaf(long idx) {
		PlayLeaf leaf = playlist.getLeaf(idx);
		playLeaf(leaf);
	}

	public void downloadLeaf(long idx) {
		PlayLeaf leaf = playlist.getLeaf(idx);
		downloadLeaf(leaf);
	}

	public void playNode(long idx) {
		PlayNode node = playlist.getNode(idx);
		playLeaf(node.getLeafs().get(0));
	}

	private void playLeaf(PlayLeaf leaf) {
		mediaPlayer.stop(); // stop to avoid LoggingSubscriberExceptionHandler
		current = leaf;
		if (leaf != null) {
			current.setPlayed(false);
			current.setCurrent(true);
			if (PlayLeaf.TYPE.youtube.toString().equals(leaf.getType())
					&& leaf.getMrl() == null) {
				try {
					String mrl = HttpRequestUtil.sendHttpGETRequest(AppConfig.getInstance().DATA_SERVER_URL 
							+ "videos/" + leaf.getUrl() + "/videourl?mac=" + AppProperties.getHardwareId());
					leaf.setMrl(mrl);
					String[] options = {
			                ":file-caching=0",
			                ":network-caching=300",
			                ":sout = #transcode{vcodec=x264,vb=800,scale=0.25,acodec=none,fps=23}"};
					mediaPlayer.playMedia(current.getMrl(), options);
				}
				catch (Exception e) {
					LOGGER.error(e.getMessage(), e);
					playNext();
				}
			}
			else {
				String[] options = {
		                ":file-caching=0",
		                ":network-caching=300",
		                ":sout = #transcode{vcodec=x264,vb=800,scale=0.25,acodec=none,fps=23}"};
				mediaPlayer.playMedia(current.getMrl(), options);
			}
		}
	}

	public void downloadLeaf(PlayLeaf leaf) {
		if (leaf == null || leaf.getMrl() == null) {
			return;
		}
		new PlayLeafDownloader(leaf).start();
	}

	public void playPlaylist() {
		if (!playlist.getLeafs().isEmpty()) {
			playlist.resetPlayed();
			playLeaf(playlist.getLeafs().get(0));
		}
	}

	public void repeat() {
		if (isRepeat()) {
			mode = PlaylistMode.DEFAULT;
		}
		else {
			mode = PlaylistMode.REPEAT;
		}
	}
	
	public void loop() {
		if (isLoop()) {
			mode = PlaylistMode.DEFAULT;
		}
		else {
			mode = PlaylistMode.LOOP;
		}
	}

	public void random() {
		if (isRandom()) {
			random = false;
		}
		else {
			random = true;
		}
	}

	public void empty() {
		mediaPlayer.stop();
		playlist.clear();
	}

	public void stop() {
		mediaPlayer.stop();
	}

	public void pause() {
		mediaPlayer.setPause(true);
	}

	public void resume() {
		mediaPlayer.play();
	}

	public EmbeddedMediaPlayer getMediaPlayer() {
		return mediaPlayer;
	}
}
