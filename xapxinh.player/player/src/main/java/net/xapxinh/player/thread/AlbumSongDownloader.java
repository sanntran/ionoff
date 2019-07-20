package net.xapxinh.player.thread;

import com.google.gson.Gson;
import net.xapxinh.player.HttpRequestUtil;
import net.xapxinh.player.config.AppConfig;
import net.xapxinh.player.config.UserConfig;
import net.xapxinh.player.model.Song;
import org.apache.log4j.Logger;

import java.io.IOException;

public class AlbumSongDownloader extends Thread {

	private static final Logger LOGGER = Logger.getLogger(AlbumSongDownloader.class.getName());

	private Long id;
	private Song song;

	public AlbumSongDownloader(Song song) {
		this.song = song;
		this.id = song.getId();
	}

	public AlbumSongDownloader(Long id) {
		this.id = id;
	}

	@Override
	public void run() {
		try {
			if (song == null) {
				song = getSong(id);
			}
			downloadSong(song);
		} catch (Throwable t) {
			LOGGER.error(t.getMessage(), t);
		}
	}

	private Song getSong(Long id) throws IOException {
		String sourceUrl = AppConfig.getInstance().DATA_SERVER_URL + "songs/" + id + "?mac=" + UserConfig.getInstance().HARDWARE_ID;
		Gson gson = new Gson();
		String result = HttpRequestUtil.sendHttpGETRequest(sourceUrl);
		Song song = gson.fromJson(result, Song.class);
		return song;
	}

	private void downloadSong(Song song) throws IOException {
		FileDownloader.downloadFile(song.getTitle(), song.getArtists(),
				song.getAuthor(), song.getUrl());
	}

}
