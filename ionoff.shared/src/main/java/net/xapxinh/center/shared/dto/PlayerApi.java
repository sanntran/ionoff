package net.xapxinh.center.shared.dto;

public class PlayerApi {
	
	public static final String FILE = "file";
	public static final String DIR = "dir";
	public static final String PARENT = "..";
	public static final String IN_PLAY = "in_play";
	public static final String ALBUM = "album";
	public static final String YOUTUBE = "youtube";
	public static final String PLAYLEAF = "playleaf";
	public static final String PLAYNODE = "playnode";
	public static final String PLAYLIST = "playlist";
	public static final int VOL_MAX = 100;
	
	public static final String PARENT_SUFFIX = "/..";
	
	public static Command fullScreen() {
		return new Command()
				.setCommand("fullscreen");
	}
	
	public static Command volumeDown() {
		return new Command()
				.setCommand("volume")
				.setVal("-");
	}
	
	public static Command volumeUp() {
		return new Command()
				.setCommand("volume")
				.setVal("+");
	}
	
	public static Command volumeSet(int vol) {
		return new Command()
				.setCommand("volume")
				.setVal(vol + "");
	}
	
	public static Command playPlaylistLeaf(long idx) {
		return new Command()
				.setCommand("pl_play")
				.setId(idx + "")
				.setType(PLAYLEAF);
	}
	
	public static Command prevPlaylistLeaf() {
		return new Command()
				.setCommand("pl_previous");
	}
	
	public static Command nextPlaylistLeaf() {
		return new Command()
		.setCommand("pl_next");
	}
	
	public static Command deletePlaylistLeaf(long idx) {
		return new Command()
		.setCommand("pl_delete")
		.setId(idx + "")
		.setType(PLAYLEAF);
	}

	public static Command togglePause(int idx) {
		return new Command()
		.setCommand("pl_pause")
		.setId("" + idx + "");
	}
	
	public static Command resumePlaylistLeaf() {
		return new Command()
		.setCommand("pl_forceresume");
	}
	
	public static Command pausePlaylistLeaf() {
		return new Command()
		.setCommand("pl_forcepause");
	}
	
	public static Command playPlaylist() {
		return new Command()
		.setCommand("pl_play");
	}
	
	public static Command stopPlaylist() {
		return new Command()
		.setCommand("pl_stop");
	}
	
	public static Command emptyPlaylist() {
		return new Command()
		.setCommand("pl_empty");
	}

	public static Command seekForward() {
		return new Command()
		.setCommand("seek")
		.setVal("+");
	}
	
	public static Command seekReward() {
		return new Command()
		.setCommand("seek")
		.setVal("-");
	}

	public static Command loopPlaylist() {
		return new Command()
		.setCommand("pl_loop");
	}
	
	public static Command randomPlaylist() {
		return new Command()
		.setCommand("pl_random");
	}
	
	public static Command repeatPlaylistLeaf() {
		return new Command()
		.setCommand("pl_repeat");
	}
	
	public static Command playMediaLibraryLeaf(String url) {
		return new Command()
				.setCommand("in_play")
				.setInput(url);
	}

	public static Command playMediaLibraryLeaf(int id) {
		return new Command()
				.setCommand("pl_play")
				.setId(id + "");
	}

	public static Command playMediaLibraryNode(int id) {
		return new Command()
				.setCommand("pl_play")
				.setId(id + "");
	}

	public static Command addPlayFile(String url) {
		return new Command()
				.setCommand("in_play")
				.setInput(url)
				.setInput_type(FILE);
	}
	
	public static Command addPlayDir(String url) {
		return new Command()
				.setCommand("in_play")
				.setInput(url)
				.setInput_type(DIR);
	}
	
	public static Command addPlayYoutube(String  url) {
		return new Command()
				.setCommand("in_play")
				.setInput(url)
				.setInput_type(YOUTUBE);
	}
	
	public static Command addEnqueueYoutube(String url) {
		return new Command()
				.setCommand("in_enqueue")
				.setInput(url)
				.setInput_type(YOUTUBE);
	}
	
	public static Command addPlayAlbum(long albumId) {
		return new Command()
				.setCommand("in_play")
				.setInput(albumId + "")
				.setInput_type(ALBUM);
	}
	
	public static Command addEnqueueAlbum(long albumId) {
		return new Command()
				.setCommand("in_enqueue")
				.setInput(albumId + "")
				.setInput_type(ALBUM);
	}

	public static Command addEnqueueFile(String url) {
		return new Command()
				.setCommand("in_enqueue")
				.setInput(url)
				.setInput_type(FILE);
	}
	
	public static Command addEnqueueDir(String url) {
		return new Command()
				.setCommand("in_enqueue")
				.setInput(url)
				.setInput_type(DIR);
	}

	public static Command volumeMute() {
		return new Command()
				.setCommand("volume")
				.setVal("0");
	}

	public static Command playPlaylistNode(long idx) {
		return new Command()
		.setCommand("pl_play")
		.setId(idx + "")
		.setType(PLAYNODE);
	}

	public static Command deletePlaylistNode(long idx) {
		return new Command()
		.setCommand("pl_delete")
		.setId(idx + "")
		.setType(PLAYNODE);
	}

	public static Command addPlayPlaylist(Long id) {
		return new Command()
				.setCommand("in_play")
				.setInput(id + "")
				.setInput_type(PLAYLIST);
	}
	
	public static Command addEnqueuePlaylist(Long id) {
		return new Command()
		.setCommand("in_enqueue")
		.setInput(id + "")
		.setInput_type(PLAYLIST);
	}
	

	public static Command addPlayPlayLeaf(Long id) {
		return new Command()
				.setCommand("in_play")
				.setInput(id + "")
				.setInput_type(PLAYLEAF);
	}
	
	public static Command addEnqueuePlayLeaf(Long id) {
		return new Command()
				.setCommand("in_enqueue")
				.setInput(id + "")
				.setInput_type(PLAYLEAF);
	}
	
	public static Command addPlayPlayNode(Long id) {
		return new Command()
				.setCommand("in_play")
				.setInput(id + "")
				.setInput_type(PLAYNODE);
	}
	
	public static Command addEnqueuePlayNode(Long id) {
		return new Command()
				.setCommand("in_enqueue")
				.setInput(id + "")
				.setInput_type(PLAYNODE);
	}
}
