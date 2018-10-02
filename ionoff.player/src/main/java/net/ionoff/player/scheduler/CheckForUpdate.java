package net.ionoff.player.scheduler;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;

import org.apache.log4j.Logger;

import net.ionoff.player.util.FileUtil;
import net.ionoff.player.util.HttpClient;
import net.ionoff.player.config.AppConfig;

public class CheckForUpdate extends Thread {
	
	private static final Logger LOGGER = Logger.getLogger(CheckForUpdate.class.getName());
	
	private static final long INTERVAL = 3600000; // 1 hour

	@Override
	public void run() {
		for (; true;) {
			try {
				downloadLatestVersion();
				sleep(INTERVAL);
			} catch (Throwable t) {
				LOGGER.error(t.getMessage(), t);
				try {
					sleep(INTERVAL);
				} catch (InterruptedException e) {
					LOGGER.error("InterruptedException " + e.getMessage());
				}
			}
		}
	}

	private void downloadLatestVersion() throws IOException {
		String latestVersion = retrieveLatestVersion();
		LOGGER.info("Latest version from update site: " + latestVersion);
		if (AppConfig.INSTANCE.VERSION.equals(latestVersion)) {
			LOGGER.info("Current version is the latest version");
			return;
		}
		String targetDirectory = getDownloadFolder();
		File targetDir = new File(targetDirectory);
		if (targetDir.exists() && targetDir.isDirectory()) {
			for (File f : targetDir.listFiles()) {
				if (f.getName().equals(latestVersion + ".zip")) {
					LOGGER.info("The latest version was downloaded");
					return;
				}
			}
		}
		LOGGER.info("Downloading new version: " + latestVersion);
		if (!targetDir.exists()) {
			targetDir.mkdirs();
		}
		String sourceUrl = AppConfig.INSTANCE.UPDATE_SITE + "/" + latestVersion + ".zip";
		URL url = new URL(sourceUrl);
		String fileName = latestVersion + ".zip";
		Path targetPath = new File(targetDirectory + File.separator + fileName).toPath();
		Files.copy(url.openStream(), targetPath, StandardCopyOption.REPLACE_EXISTING);
		
		
		String updateFolder = getUpdateFolder();
		FileUtil.deleteFile(updateFolder);
		LOGGER.info("Extracting file " + latestVersion + ".zip to " + updateFolder);
		FileUtil.unzip(targetPath.toString(), updateFolder);
	}
	
	private String retrieveLatestVersion() throws IOException {
		String sourceUrl = AppConfig.INSTANCE.UPDATE_SITE + "/latest.json";
		return HttpClient.sendHttpGETRequest(sourceUrl);
	}

	public static String getUpdateFolder() {
		return AppConfig.INSTANCE.APP_DIR + File.separator + "ext";
	}

	public static String getDownloadFolder() {
		return AppConfig.INSTANCE.APP_DIR + File.separator + "dist";
	}
}
