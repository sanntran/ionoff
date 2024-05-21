package net.ionoff.center.server.restcontroller;

import java.io.IOException;

import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.scheduler.LatestVersionUpdator;
import net.ionoff.center.shared.dto.VersionDto;

@RestController
@Transactional
public class VersionServiceController {

	@Autowired
	LatestVersionUpdator latestVersionUpdator;


	@RequestMapping(value = "versions/upgrade",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")

	public VersionDto upgradeLatestVersion(HttpServletRequest request) throws IOException {
		VersionDto latestVersion = latestVersionUpdator.getLatestVersion();
		VersionDto currentVersion = getCurrentVersion();
		if (!currentVersion.getName().equals(latestVersion.getName())) {
			latestVersionUpdator.upgradeLatestVersion(latestVersion);
		}
		return getCurrentVersion();
	}
	
	@RequestMapping(value = "version.json",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")

	public VersionDto getVersionJson() {
		return getCurrentVersion();
	}

	@RequestMapping(value = "versions/current",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")

	public VersionDto getCurrentVersion() {
		VersionDto versionDto = new VersionDto();
		versionDto.setName(latestVersionUpdator.getAppVersion());
		return versionDto;
	}
	
	@RequestMapping(value = "versions/latest",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")

	public VersionDto getLatestVersion(HttpServletRequest request) throws IOException {
		VersionDto latestVersion = latestVersionUpdator.getLatestVersion();
		return latestVersion;
	}
	
	@RequestMapping(value = "versions/check",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")

	public VersionDto checkLatestVersion(HttpServletRequest request) throws IOException {
		VersionDto latestVersion = getLatestVersion(request);
		VersionDto currentVersion = getCurrentVersion();
		if (!currentVersion.getName().equals(latestVersion.getName())) {
			return latestVersion;
		}
		return new VersionDto();
	}

}
