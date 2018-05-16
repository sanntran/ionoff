package net.ionoff.center.server.scheduler;

import java.io.File;
import java.io.IOException;
import java.util.Date;

import org.apache.log4j.Logger;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;

import com.google.gson.Gson;

import net.ionoff.center.server.config.AppConfig;
import net.ionoff.center.server.entity.Version;
import net.ionoff.center.server.util.DateTimeUtil;
import net.ionoff.center.server.util.FileDownloadUtil;
import net.ionoff.center.server.util.FileManagementUtil;
import net.ionoff.center.server.util.HttpRequestUtil;


@EnableScheduling
public class LatestVersionUpdator {
	
	private static final Logger LOGGER = Logger.getLogger(LatestVersionUpdator.class.getName());
	
	private static final Gson GSON = new Gson();
	
	@Scheduled(fixedDelay = 3600000) // 1 hour
    public void downloadLatestVersion() {
		try {
			Version latestVersion = HttpRequestUtil.sendHttpGETLatestVersion();
			downloadLatestVersion(latestVersion);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
		}
	}
	
	private static void downloadLatestVersion(Version latestVersion) throws IOException {
		File updateFolder = new File(AppConfig.getInstance().UPDATE_FOLDER);
		if (!updateFolder.exists()) {
			updateFolder.mkdirs();
		}
		
		String updateSiteLatestVersionJson = GSON.toJson(latestVersion);
		File localDownloadedVersionJsonFile = new File(AppConfig.getInstance().LATEST_VERSION_FILE);
		
		if (!localDownloadedVersionJsonFile.exists()) {
			downloadLatestVersionZipFile(latestVersion.getFileName());
			LOGGER.info("Create local latest version json file: " + AppConfig.getInstance().LATEST_VERSION_FILE);
			FileManagementUtil.writeToFile(updateSiteLatestVersionJson, AppConfig.getInstance().LATEST_VERSION_FILE);
			
		} else {
			String downloadedVersionFileContent = FileManagementUtil.readFile(localDownloadedVersionJsonFile);
			Version downloadedVersion = GSON.fromJson(downloadedVersionFileContent, Version.class);
			if (latestVersion.getFileName() != null) {
				if (!latestVersion.getFileName().equals(downloadedVersion.getFileName())) {
					downloadLatestVersionZipFile(latestVersion.getFileName());
					LOGGER.info("Update local latest version json file: " + AppConfig.getInstance().LATEST_VERSION_FILE);
					FileManagementUtil.writeToFile(updateSiteLatestVersionJson, AppConfig.getInstance().LATEST_VERSION_FILE);
				}
				else {
					LOGGER.info(" Latest version is downloaed. Local version json file is up to date.");
				}
			}
		}
	}

	private static String downloadLatestVersionZipFile(String zipFileName) throws IOException {
		LOGGER.info("Downloading latest version zip file: " + zipFileName);
		FileManagementUtil.deleteFile(AppConfig.getInstance().UPDATE_FOLDER);
		File updateFolder = new File(AppConfig.getInstance().UPDATE_FOLDER);
		updateFolder.mkdir();
		String zipFileUrl = AppConfig.getInstance().UPDATE_SITE + "/" + zipFileName;
		String localDownloadFile = AppConfig.getInstance().UPDATE_FOLDER + File.separator + zipFileName;
		String zipFile = FileDownloadUtil.downloadFile(zipFileUrl, localDownloadFile);
		return zipFile;
	}

	public static void upgradeToLatestVersion(Version latestVersion) throws IOException {
		LOGGER.info("Updating to latest version: " + latestVersion.getName());
		downloadLatestVersion(latestVersion);
		unzipLatestVersion(latestVersion);
		File updateFolder = new File(AppConfig.getInstance().UPDATE_FOLDER);
		for (File file : updateFolder.listFiles()) {
			if (file.isDirectory() || file.getName().endsWith(".zip")) {
				FileManagementUtil.deleteFile(file);
			}
		}
		triggerCronToUpdate();
	}

	private static void triggerCronToUpdate() throws IOException {
		File cronFolder = new File(AppConfig.getInstance().CRON_FOLDER);
		if (!cronFolder.exists()) {
			cronFolder.mkdirs();
		}
		String fileTrigger = DateTimeUtil.yyyyMMdd_HHmmssFormatter.format(new Date()) + ".log";
		File trigger = new File(AppConfig.getInstance().CRON_FOLDER + File.separator + fileTrigger);
		trigger.createNewFile();
	}

	private static void unzipLatestVersion(Version latestVersion) throws IOException {
		File zipFile = new File(AppConfig.getInstance().UPDATE_FOLDER + File.separator + latestVersion.getFileName());
		if (!zipFile.exists()) {
			return;
		}
		if (zipFile.getName().endsWith(".zip")) {
			LOGGER.info("Unzip file " + zipFile.getAbsolutePath());
			FileManagementUtil.unzipFile(zipFile.getAbsolutePath(), AppConfig.getInstance().UPDATE_FOLDER);
		}
	}
}
