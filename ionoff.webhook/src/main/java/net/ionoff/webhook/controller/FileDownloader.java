package net.ionoff.webhook.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.text.Normalizer;
import java.util.regex.Pattern;

public class FileDownloader extends Thread {

	private static final Logger LOGGER = LoggerFactory.getLogger(FileDownloader.class);

	static String downloadFile(String name, String folder, String mrl) throws IOException {
		folder = AccentRemover.removeAccent(folder);
		String[] parts = mrl.split("\\.");
		String fileExt = parts[parts.length - 1];
		String fileName = AccentRemover.removeAccent(name) + "." + fileExt;
		fileName.replaceAll(" ", "-");
		String targetDirectory = "F:\\Albums\\media" +
				(!folder.isEmpty() ? (File.separator + folder) : "");

		File targetDir = new File(targetDirectory);
		if (targetDir.exists() && targetDir.isDirectory()) {
			for (File f : targetDir.listFiles()) {
				if (fileName.contains(f.getName())) {
					LOGGER.info("The file was already downloaded");
					return f.getAbsolutePath();
				}
			}
		}
		LOGGER.info("Downloading file " + fileName + " to folder " + targetDirectory);
		if (!targetDir.exists()) {
			targetDir.mkdirs();
		}
		String sourceUrl = mrl;
		URL url = new URL(sourceUrl);
		Path targetPath = new File(targetDirectory + File.separator + fileName).toPath();
		Files.copy(url.openStream(), targetPath, StandardCopyOption.REPLACE_EXISTING);
		LOGGER.info("Finish downloading file " + fileName + " to folder " + targetDirectory);
		return targetPath.toString();
	}

	public static void main(String[] args) {
		System.out.println(AccentRemover.removeAccent("Đi về nơi xa"));
	}
}
