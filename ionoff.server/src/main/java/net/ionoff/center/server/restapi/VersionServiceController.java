package net.ionoff.center.server.restapi;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

import com.google.gson.Gson;

import net.ionoff.center.server.config.AppConfig;
import net.ionoff.center.server.scheduler.LatestVersionUpdator;
import net.ionoff.center.server.util.HttpRequestUtil;
import net.ionoff.center.shared.dto.VersionDto;

@RestController
@EnableWebMvc
public class VersionServiceController {
	
	@RequestMapping(value = "versions/upgrade",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public VersionDto upgradeLatestVersion(HttpServletRequest request) throws IOException {
		VersionDto latestVersion = HttpRequestUtil.getLatestVersion();
		VersionDto currentVersion = getCurrentVersion();
		if (currentVersion.getName().equals(latestVersion.getName())) {
			LatestVersionUpdator.upgradeLatestVersion(latestVersion);
		}
		return getCurrentVersion();
	}
	
	@RequestMapping(value = "version.json",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public VersionDto getVersionJson() {
		return getCurrentVersion();
	}

	@RequestMapping(value = "versions/current",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public VersionDto getCurrentVersion() {
		VersionDto versionDto = new VersionDto();
		versionDto.setName(AppConfig.getInstance().VERSION);
		return versionDto;
	}
	
	@RequestMapping(value = "versions/latest",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public VersionDto getLatestVersion(HttpServletRequest request) throws IOException {
		VersionDto latestVersion = HttpRequestUtil.getLatestVersion();
		return latestVersion;
	}
	
	@RequestMapping(value = "versions/check",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public VersionDto checkLatestVersion(HttpServletRequest request) throws IOException {
		VersionDto latestVersion = getLatestVersion(request);
		VersionDto currentVersion = getCurrentVersion();
		if (!currentVersion.getName().equals(latestVersion.getName())) {
			return latestVersion;
		}
		return new VersionDto();
	}

}
