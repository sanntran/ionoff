package net.xapxinh.player.thread;

import net.xapxinh.player.config.AppUtil;
import net.xapxinh.player.config.UserConfig;
import org.apache.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;

public class FileDownloader extends Thread {

	private static final Logger LOGGER = Logger.getLogger(FileDownloader.class.getName());

	static void downloadFile(String name, String artist, String author, String mrl) throws IOException {
		String folder = artist;
		if (folder == null) {
			folder = author;
		}
		if (folder == null) {
			folder = "";
		}
		if (folder.contains(",")) {
			folder = folder.split(",")[0];
		}
		folder = AccentRemover.removeAccent(folder);
		String[] parts = mrl.split("\\.");
		String fileExt = parts[parts.length - 1];
		String fileName = AccentRemover.removeAccent(name) + "." + fileExt;
		String targetDirectory = UserConfig.getInstance().ROOT_BROWSE_DIR +
				(!folder.isEmpty() ? (File.separator + folder) : "");

		File targetDir = new File(targetDirectory);
		if (targetDir.exists() && targetDir.isDirectory()) {
			for (File f : targetDir.listFiles()) {
				if (fileName.contains(f.getName())) {
					LOGGER.info("The file was already downloaded");
					return;
				}
			}
		}
		LOGGER.info("Downloading file " + fileName + " to folder " + targetDirectory);
		if (!targetDir.exists()) {
			AppUtil.mkDir(targetDir);
		}
		String sourceUrl = mrl;
		URL url = new URL(sourceUrl);
		Path targetPath = new File(targetDirectory + File.separator + fileName).toPath();
		Files.copy(url.openStream(), targetPath, StandardCopyOption.REPLACE_EXISTING);
		LOGGER.info("Finish downloading file " + fileName + " to folder " + targetDirectory);
	}
}
