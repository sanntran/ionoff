package net.ionoff.center.server.scheduler;

import java.io.File;
import java.io.IOException;
import java.util.Calendar;
import java.util.Date;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;

import com.google.gson.Gson;

import net.ionoff.center.server.util.DateTimeUtil;
import net.ionoff.center.server.util.FileDownloadUtil;
import net.ionoff.center.server.util.FileManagementUtil;
import net.ionoff.center.server.util.HttpRequestUtil;
import net.ionoff.center.shared.dto.VersionDto;

@EnableScheduling
public class LatestVersionUpdator {
	
	private static final Logger LOGGER = Logger.getLogger(LatestVersionUpdator.class.getName());

	private static final Gson GSON = new Gson();

	private String appVersion;
	private String poolFolderPath;
	private String latestJsonPath;
	private String cronFolderPath;
	private String latestVesionUrl;

	public LatestVersionUpdator(@Value("${version}") String appVersion,
								@Value("${tomcat.update.cron}") String cronFolder,
								@Value("${tomcat.update.pool}") String poolFolder,
								@Value("${tomcat.update.latest}") String latestJson,
								@Value("${service.release.latest}") String latestVesionUrl) {
		this.appVersion = appVersion;
		String catalinaBase = getCatalinaBase();
		this.poolFolderPath = catalinaBase + File.separator + poolFolder;
		this.latestJsonPath = catalinaBase + File.separator + latestJson;
		this.cronFolderPath = catalinaBase + File.separator + cronFolder;
		this.latestVesionUrl = latestVesionUrl;
	}

	@Scheduled(fixedDelay = 3600000) // 1 hour
    public void downloadLatestVersion() {
		try {
			VersionDto latestVersion = getLatestVersion();
			downloadLatestVersion(latestVersion);
			if (appVersion.equals(latestVersion.getName())) {
				Calendar cal = Calendar.getInstance();
				if (cal.get(Calendar.HOUR_OF_DAY) == 1) { // 1AM
					upgradeLatestVersion(latestVersion);
				}
			}
			
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
		}
	}

	public String getAppVersion() {
		return appVersion;
	}

	public VersionDto getLatestVersion() throws IOException {
		String updateSiteLatestVersionJson = HttpRequestUtil.sendGetRequest(latestVesionUrl);
		LOGGER.info("Latest version from update site: \n" + updateSiteLatestVersionJson);
		VersionDto latestVersion = new Gson().fromJson(updateSiteLatestVersionJson, VersionDto.class);
		return latestVersion;
	}
	
	private void downloadLatestVersion(VersionDto latestVersion) throws IOException {
		File poolFolder = new File(poolFolderPath);
		if (!poolFolder.exists()) {
			poolFolder.mkdirs();
		}
		
		String updateSiteLatestVersionJson = GSON.toJson(latestVersion);
		File localDownloadedVersionJsonFile = new File(latestJsonPath);
		
		if (!localDownloadedVersionJsonFile.exists()) {
			downloadLatestVersionZipFile(latestVersion.getFileName());
			LOGGER.info("Create local latest version json file: " + latestJsonPath);
			FileManagementUtil.writeToFile(updateSiteLatestVersionJson, latestJsonPath);
		} else {
			String downloadedVersionFileContent = FileManagementUtil.readFile(localDownloadedVersionJsonFile);
			VersionDto downloadedVersion = GSON.fromJson(downloadedVersionFileContent, VersionDto.class);
			if (latestVersion.getFileName() != null) {
				if (!latestVersion.getFileName().equals(downloadedVersion.getFileName())) {
					downloadLatestVersionZipFile(latestVersion.getFileName());
					LOGGER.info("Update local latest version json file: " + latestJsonPath);
					FileManagementUtil.writeToFile(updateSiteLatestVersionJson, latestJsonPath);
				}
				else {
					LOGGER.info(" Latest version is downloaed. Local version json file is up to date.");
				}
			}
			
		}
	}

	private String downloadLatestVersionZipFile(String zipFileName) throws IOException {
		LOGGER.info("Downloading latest version zip file: " + zipFileName);
		FileManagementUtil.deleteFile(poolFolderPath);
		File updateFolder = new File(poolFolderPath);
		updateFolder.mkdir();
		String zipFileUrl = poolFolderPath + File.separator + zipFileName;
		String localDownloadFile = poolFolderPath + File.separator + zipFileName;
		String zipFile = FileDownloadUtil.downloadFile(zipFileUrl, localDownloadFile);
		return zipFile;
	}

	public void upgradeLatestVersion(VersionDto latestVersion) throws IOException {
		LOGGER.info("Updating to latest version: " + latestVersion.getName());
		downloadLatestVersion(latestVersion);
		unzipLatestVersion(latestVersion);
		File updateFolder = new File(poolFolderPath);
		for (File file : updateFolder.listFiles()) {
			if (file.isDirectory() || file.getName().endsWith(".zip")) {
				FileManagementUtil.deleteFile(file);
			}
		}
		triggerCronToUpdate();
	}

	private void triggerCronToUpdate() throws IOException {
		LOGGER.info("Trigger crom to run script to update...");
		File cronFolder = new File(cronFolderPath);
		if (!cronFolder.exists()) {
			cronFolder.mkdirs();
		}
		String fileTrigger = DateTimeUtil.yyyyMMdd_HHmmssFormatter.format(new Date()) + ".log";
		File trigger = new File(cronFolderPath+ File.separator + fileTrigger);
		LOGGER.info("Create new file " + trigger.getAbsolutePath());
		trigger.createNewFile();
	}

	private void unzipLatestVersion(VersionDto latestVersion) throws IOException {
		File zipFile = new File(poolFolderPath + File.separator + latestVersion.getFileName());
		if (!zipFile.exists()) {
			return;
		}
		if (zipFile.getName().endsWith(".zip")) {
			LOGGER.info("Unzip file " + zipFile.getAbsolutePath());
			FileManagementUtil.unzipFile(zipFile.getAbsolutePath(), poolFolderPath);
		}
	}

	public static String getCatalinaBase() {
		return System.getProperty("catalina.base");
	}
}
