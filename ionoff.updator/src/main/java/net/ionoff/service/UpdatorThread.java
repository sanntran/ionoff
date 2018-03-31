package net.ionoff.service;

import java.io.File;
import java.io.IOException;

import org.apache.log4j.Logger;

import com.google.gson.Gson;

public class UpdatorThread extends Thread {

	private static final long INTERVAL = 1000 * 60 * 60 * 24; // 1 day
	private static final Logger LOGGER = Logger.getLogger(UpdatorThread.class.getName());
	private static final Gson GSON = new Gson();
	private boolean updating = false;
	
	@Override
	public void run() {
		downloadLatestVersion();
		for (; true;) {
			try {
				sleep(INTERVAL);
				downloadLatestVersion();
			} catch (Throwable e) {
				if (e instanceof OutOfMemoryError) {
					try {
						System.gc();
					} catch (Throwable t) {
						LOGGER.error(t.getMessage(), t);
					}
				} else {
					LOGGER.error(e.getMessage(), e);
				}
			}
		}
	}

	public void downloadLatestVersion() {
		if (updating == true) {
			return;
		}
		LOGGER.info("Checking to download latest version");
		try {
			File updateFolder = new File(UpdatorConfig.getInstance().LOCAL_UPDATE_FOLDER);
			if (!updateFolder.exists()) {
				File parent = updateFolder.getParentFile();
				if (!parent.exists()) {
					parent.mkdir();
				}
				updateFolder.mkdir();
			}
			
			String updateSiteLatestVersionJson = HttpRequestUtil
					.sendHttpGETRequest(UpdatorConfig.getInstance().LATESTVERSION_FILE_URL);
			Version latestVersion = GSON.fromJson(updateSiteLatestVersionJson, Version.class);
			
			File localDownloadedVersionJsonFile = new File(UpdatorConfig.getInstance().LOCAL_LATESTVERSION_FILE);
			
			if (!localDownloadedVersionJsonFile.exists()) {
				downloadLatestVersionZipFile(latestVersion.getFileName());
				LOGGER.info("Create local latest version json file: " + UpdatorConfig.getInstance().LOCAL_LATESTVERSION_FILE);
				FileManager.writeToFile(updateSiteLatestVersionJson, UpdatorConfig.getInstance().LOCAL_LATESTVERSION_FILE);
				
			} else {
				String downloadedVersionFileContent = FileManager.readFile(localDownloadedVersionJsonFile);
				Version downloadedVersion = GSON.fromJson(downloadedVersionFileContent, Version.class);
				if (latestVersion.getFileName() != null) {
					if (!latestVersion.getFileName().equals(downloadedVersion.getFileName())) {
						downloadLatestVersionZipFile(latestVersion.getFileName());
						LOGGER.info("Update local latest version json file: " + UpdatorConfig.getInstance().LOCAL_LATESTVERSION_FILE);
						FileManager.writeToFile(updateSiteLatestVersionJson, UpdatorConfig.getInstance().LOCAL_LATESTVERSION_FILE);
					}
					else {
						LOGGER.info("Local latest version json file is up to date");
					}
				}
			}
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
		}
	}

	private static String downloadLatestVersionZipFile(String zipFileName) throws IOException {
		LOGGER.info("Downloading latest version zip file: " + zipFileName);
		FileManager.deleteFile(UpdatorConfig.getInstance().LOCAL_UPDATE_FOLDER);
		File updateFolder = new File(UpdatorConfig.getInstance().LOCAL_UPDATE_FOLDER);
		updateFolder.mkdir();
		String zipFileUrl = UpdatorConfig.getInstance().UPDATE_SITE + "/" + zipFileName;
		String localDownloadFile = UpdatorConfig.getInstance().LOCAL_UPDATE_FOLDER + File.separator + zipFileName;
		String zipFile = FileDownloadUtil.downloadFile(zipFileUrl, localDownloadFile);
		return zipFile;
	}

	public void setUpdating(boolean updating) {
		this.updating = updating;
	}
}
