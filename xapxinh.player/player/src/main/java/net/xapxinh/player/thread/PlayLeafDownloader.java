package net.xapxinh.player.thread;

import net.xapxinh.player.model.PlayLeaf;
import org.apache.log4j.Logger;

import java.io.IOException;

public class PlayLeafDownloader extends Thread {
	
	private static final Logger LOGGER = Logger.getLogger(PlayLeafDownloader.class.getName());

	private PlayLeaf playLeaf;

	public PlayLeafDownloader(PlayLeaf playLeaf) {
		this.playLeaf = playLeaf;
	}

	@Override
	public void run() {
		try {
			dowloadPlayLeaf();
		} catch (Throwable t) {
			LOGGER.error(t.getMessage(), t);
		}
	}

	private void dowloadPlayLeaf() throws IOException {
		FileDownloader.downloadFile(playLeaf.getName(), playLeaf.getArtists(),
				playLeaf.getAuthor(), playLeaf.getMrl());
	}
}
