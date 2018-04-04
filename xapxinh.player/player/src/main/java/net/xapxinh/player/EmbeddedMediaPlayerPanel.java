package net.xapxinh.player;

import static net.xapxinh.player.Application.application;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import net.xapxinh.player.event.PausedEvent;
import net.xapxinh.player.event.PlayingEvent;
import net.xapxinh.player.event.StoppedEvent;
import net.xapxinh.player.model.Album;
import net.xapxinh.player.model.MediaFile;
import net.xapxinh.player.model.PlayLeaf;
import net.xapxinh.player.model.PlayList;
import net.xapxinh.player.model.PlayNode;
import net.xapxinh.player.model.Song;
import net.xapxinh.player.model.State;
import net.xapxinh.player.model.Status;
import net.xapxinh.player.model.YoutubeVideo;
import uk.co.caprica.vlcj.binding.internal.libvlc_media_t;
import uk.co.caprica.vlcj.component.EmbeddedMediaPlayerComponent;
import uk.co.caprica.vlcj.filter.MediaFileFilter;
import uk.co.caprica.vlcj.player.MediaPlayer;

@SuppressWarnings("serial")
public class EmbeddedMediaPlayerPanel extends EmbeddedMediaPlayerComponent {
	
	private final EmbeddedMediaListPlayer mediaListPlayer;
	
	public EmbeddedMediaPlayerPanel() {
		super();
		mediaListPlayer = new EmbeddedMediaListPlayer(getMediaPlayer());
		getMediaPlayer().setRepeat(false);
		getMediaPlayer().setPlaySubItems(true); // <--- This is very important for YouTube media
	}

	@Override
    public void mediaChanged(MediaPlayer mediaPlayer, libvlc_media_t media, String mrl) {
		System.out.println("mediaChanged");
		System.out.println(mrl);
		mediaListPlayer.mediaChanged();
    }

	@Override
    public void mediaStateChanged(MediaPlayer mediaPlayer, int newState) {
		State state = State.getState(newState);
		if (State.playing.equals(state)) {
			System.out.println("playing");
	    	application().post(PlayingEvent.instance());
	    	mediaListPlayer.onPlaying();
		}
		else if (State.stopped.equals(state)) {
			System.out.println("stopped");
	    	application().post(StoppedEvent.instance());
	    	mediaListPlayer.onStopped();
		}
		else if (State.paused.equals(state)) {
			System.out.println("paused");
	    	application().post(PausedEvent.instance());
		}
		else if (State.error.equals(state)) {
			System.out.println("error");
	    	mediaListPlayer.onError();
		}
		else if (State.opening.equals(state)) {
			System.out.println("opening");
		}
    }
	
	@Override
    public void finished(MediaPlayer mediaPlayer) {
		System.out.println("finished");
		mediaListPlayer.onFinished();
    }
    
    @Override
    public void mediaSubItemAdded(MediaPlayer mediaPlayer, libvlc_media_t subItem) {
        System.out.println("mediaSubItemAdded: " + mediaPlayer.mrl(subItem));
    }
	
	public void inEnqueue(MediaFile mediaFile, boolean isPlay) {
		mediaListPlayer.enqueueNode(createNode(mediaFile), isPlay);
	}
	
	public void inEnqueue(YoutubeVideo youtubeVideo, boolean isPlay) {
		mediaListPlayer.enqueueNode(createNode(youtubeVideo), isPlay);
	}
	
	public void inEnqueue(Album album, boolean isPlay) {
		mediaListPlayer.enqueueNode(createNode(album), isPlay);
	}
	
	public void inEnqueue(File[] files, boolean isPlay) {
		if (files.length == 0) {
			return;
		}
		mediaListPlayer.enqueueNode(createNode(files), isPlay);
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
	
	private PlayNode createNode(MediaFile mediaFile) {
		PlayNode node = new PlayNode();
		List<PlayLeaf> leafs = new ArrayList<PlayLeaf>();
		if (MediaFile.TYPE.dir.toString().equals(mediaFile.getType())) {
			File dir = new File(mediaFile.getPath());
			node.setName(dir.getAbsolutePath());
			node.setType(PlayNode.TYPE.dir.toString());
			for (File file : dir.listFiles()) {
				if (file.isFile() && MediaFileFilter.INSTANCE.accept(file)) {
					leafs.add(createLeaf(file));
				}
			}
		}
		else {
			File file = new File(mediaFile.getPath());
			node.setName(file.getParentFile().getAbsolutePath());
			node.setType(PlayNode.TYPE.dir.toString());
			if (file.isFile() && MediaFileFilter.INSTANCE.accept(file)) {
				leafs.add(createLeaf(file));
			}
		}
		node.setLeafs(leafs);
		return node;
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
		leaf.setType(PlayLeaf.TYPE.file.toString());
		leaf.setImage(song.getImage());
		leaf.setMrl(song.getUrl());
		leaf.setArtists(song.getArtists());
		leaf.setAuthor(song.getAuthor());
		return leaf;
	}

	private PlayNode createNode(File[] files) {
		File folder = files[0].getParentFile();		
		PlayNode node = new PlayNode();
		node.setName(folder.getName());
		node.setType(PlayNode.TYPE.dir.toString());
		List<PlayLeaf> leafs = new ArrayList<PlayLeaf>();
		for (File file : files) {
			leafs.add(createLeaf(file));
		}
		node.setLeafs(leafs);
		return node;
	}

	private PlayLeaf createLeaf(File file) {
		PlayLeaf leaf = new PlayLeaf();
		leaf.setName(file.getName());
		leaf.setMrl(file.getAbsolutePath());
		leaf.setType(PlayLeaf.TYPE.file.toString());
		return leaf;
	}

	public Status getStatus() {
		Status status = new Status();		
		status.setTime(toSecond(getMediaPlayer().getTime()));
		status.setLength(toSecond(getMediaPlayer().getLength()));
		int vol = getMediaPlayer().getVolume();
		if (vol == -1) {
			vol = AppProperties.getVolume();
		}
		status.setVolume(vol);
		status.setPosition(getMediaPlayer().getPosition());
		status.setFullscreen(getMediaPlayer().isFullScreen());
		status.setRandom(mediaListPlayer.isRandom());
		status.setRepeat(mediaListPlayer.isRepeat());
		status.setLoop(mediaListPlayer.isLoop());
		PlayNode currentNode = mediaListPlayer.getPlaylist().getCurrentNode();
		status.setPlayNode(currentNode);
		if (getMediaPlayer().getMediaState() != null) {
			State state = State.getState(getMediaPlayer().getMediaState().intValue());
			if (state == null) {
				status.setState(State.stopped.toString());
			}
			else {
				status.setState(state.toString());
			}
		}
		else {
			status.setState(State.stopped.toString());
		}
		
		return status;
	}
	
	private long toSecond(long timeMilliSeconds) {
		return timeMilliSeconds/1000;
	}

	public PlayList getPlaylist() {
		return mediaListPlayer.getPlaylist();
	}

	public EmbeddedMediaListPlayer getMediaListPlayer() {
		return mediaListPlayer;
	}

	public void removePlaylistLeaf(long leafId) {
		mediaListPlayer.removeLeaf(leafId);
	}
	
	public void removePlaylistNode(long nodeId) {
		mediaListPlayer.removeNode(nodeId);
	}

	public void playPlaylistLeaf(long id) {
		mediaListPlayer.playLeaf(id);
	}

	public void playPlaylistNode(long id) {
		mediaListPlayer.playNode(id);
	}

	public void playPlaylist() {
		mediaListPlayer.playPlaylist();
	}

	public void repeatPlaylist() {
		mediaListPlayer.repeat();
	}

	public void randomPlaylist() {
		mediaListPlayer.random();
	}

	public void loopPlaylist() {
		mediaListPlayer.loop();
	}

	public void emptyPlaylist() {
		mediaListPlayer.empty();
	}

	public void stop() {
		mediaListPlayer.stop();
	}

	public void pause() {
		mediaListPlayer.pause();
	}

	public void resume() {
		mediaListPlayer.resume();
	}

	public void seekForward() {
		if (getMediaPlayer().isPlaying()) {
			float currentPosition = getMediaPlayer().getPosition();
			getMediaPlayer().setPosition((float) (currentPosition + 0.1));
		}
	}

	public void seekReward() {
		if (getMediaPlayer().isPlaying()) {
			float currentPosition = getMediaPlayer().getPosition();
			getMediaPlayer().setPosition((float) (currentPosition - 0.1));
		}
	}

	public void inEnqueue(PlayList playlist, boolean isPlay) {
		emptyPlaylist();
		getPlaylist().setId(playlist.getId());
		getPlaylist().setName(playlist.getName());
		for (PlayNode node : playlist.getNodes()) {
			mediaListPlayer.enqueueNode(node, false);
		}
		if (isPlay) {
			playPlaylist();
		}
	}

	public void inEnqueue(PlayNode playnode, boolean isPlay) {
		mediaListPlayer.enqueueNode(playnode, isPlay);
	}

	public void inEnqueue(PlayLeaf playLeaf, boolean isPlay) {
		PlayNode node = new PlayNode();
		node.setLeafs(new ArrayList<PlayLeaf>());
		if (PlayLeaf.TYPE.file.toString().equals(playLeaf.getType())) {
			node.setType(PlayNode.TYPE.dir.toString());
		}
		else if (PlayLeaf.TYPE.track.toString().equals(playLeaf.getType())) {
			node.setType(PlayNode.TYPE.album.toString());
		}
		else if (PlayLeaf.TYPE.youtube.toString().equals(playLeaf.getType())) {
			node.setType(PlayNode.TYPE.youtube.toString());
		}
		node.getLeafs().add(playLeaf);
		inEnqueue(node, isPlay);
	}

	public void inEnqueue(Song song, boolean isPlay) {
		PlayNode node = new PlayNode();
		node.setLeafs(new ArrayList<PlayLeaf>());
		node.setType(PlayNode.TYPE.album.toString());
		PlayLeaf leaf = createLeaf(song);
		node.getLeafs().add(leaf);
		inEnqueue(node, isPlay);
	}
}
