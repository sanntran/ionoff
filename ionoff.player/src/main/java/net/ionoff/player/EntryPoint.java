package net.ionoff.player;

import net.ionoff.player.config.AppConfig;
import net.ionoff.player.handler.TcpConnection;
import net.ionoff.player.scheduler.InformStatusThread;
import net.ionoff.player.scheduler.LatestVersionDownloader;

/**
 * Application entry-point.
 */
public class EntryPoint {

	public static void main(String[] args) {
		System.setProperty("app.dir", AppConfig.INSTANCE.APP_DIR);
		long heapSize = Runtime.getRuntime().totalMemory();
		System.out.println("Heap Size = " + heapSize);
		new LatestVersionDownloader().start();
		if (AppConfig.INSTANCE.INTERVAL_UPDATE) {
			new InformStatusThread().start();
		}
		new TcpConnection().start();
	}
}
