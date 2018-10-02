package net.ionoff.player;

import net.ionoff.player.config.AppConfig;
import net.ionoff.player.connection.MqttConnection;
import net.ionoff.player.scheduler.CheckForUpdate;

/**
 * Application entry-point.
 */
public class MainRunner {

	public static void main(String[] args) {
		System.setProperty("app.dir", AppConfig.INSTANCE.APP_DIR);
		long heapSize = Runtime.getRuntime().totalMemory();
		System.out.println("Heap Size = " + heapSize);
        new MqttConnection().start();
		new CheckForUpdate().start();
	}
}
