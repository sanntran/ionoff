package net.ionoff.center.server.restapi;

import java.io.IOException;
import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

import net.ionoff.center.server.entity.Version;
import net.ionoff.center.server.persistence.service.IVersionService;
import net.ionoff.center.server.scheduler.LatestVersionUpdator;
import net.ionoff.center.server.util.HttpRequestUtil;
import net.ionoff.center.shared.dto.VersionDto;

@RestController
@EnableWebMvc
public class VersionServiceController {
	
	private final Logger logger = Logger.getLogger(VersionServiceController.class.getName());

	@Autowired
	private IVersionService versionService;
	
	@RequestMapping(value = "versions/upgrade",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public VersionDto upgradeToLatestVersion(HttpServletRequest request) throws IOException {
		Version latestVersion = HttpRequestUtil.sendHttpGETLatestVersion();
		VersionDto currentVersion = getCurrentVersion();
		if (currentVersion.getDateTime() != null && latestVersion.getDateTime() != null 
				&& currentVersion.getDateTime().compareTo(latestVersion.getDateTime()) < 0) {
			
			LatestVersionUpdator.upgradeToLatestVersion(latestVersion);
		}
		return getCurrentVersion();
	}

	@RequestMapping(value = "versions/current",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public VersionDto getCurrentVersion() {
		VersionDto versionDto = new VersionDto();
		List<Version> versions = versionService.loadAll();
		if (versions == null || versions.isEmpty()) {
			Version v = new Version();
			v.setName("1.0.0");
			v.setDateTime("201609131010");
			versionService.insert(v);
			if (v.getId() != 1L) {
				v.setId(1L);
				versionService.update(v);
			}
			versions.add(v);
		}
		Version version = versions.get(0); 
		versionDto.setId(version.getId());
		versionDto.setName(versionDto.getName() + "-" + version.getDateTime());
		versionDto.setDateTime(version.getDateTime());
		
		return versionDto;
	}
	
	@RequestMapping(value = "versions/latest",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public VersionDto getLatestVersion(HttpServletRequest request) throws IOException {
		VersionDto latestVersionDto = new VersionDto();
		Version latestVersion = HttpRequestUtil.sendHttpGETLatestVersion();
		latestVersionDto.setDateTime(latestVersion.getDateTime());
		latestVersionDto.setName(latestVersion.getName());
		
		return latestVersionDto;
	}
	
	@RequestMapping(value = "versions/check",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public VersionDto checkLatestVersion(HttpServletRequest request) throws IOException {
		VersionDto latestVersion = getLatestVersion(request);
		VersionDto currentVersion = getCurrentVersion();
		if (currentVersion.getDateTime() != null && latestVersion.getDateTime() != null 
				&& currentVersion.getDateTime().compareTo(latestVersion.getDateTime()) < 0) {
			
			return latestVersion;
		}
		return new VersionDto();
	}
}
