package net.ionoff.center.server.restapi;

import java.io.File;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.apache.log4j.Logger;
import org.hibernate.SessionFactory;
import org.hibernate.c3p0.internal.C3P0ConnectionProvider;
import org.hibernate.engine.jdbc.connections.spi.ConnectionProvider;
import org.hibernate.internal.SessionFactoryImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.orm.hibernate4.LocalSessionFactoryBean;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

import com.google.gson.Gson;

import net.ionoff.center.server.config.AppConfig;
import net.ionoff.center.server.entity.Version;
import net.ionoff.center.server.persistence.service.IVersionService;
import net.ionoff.center.server.thread.ServerThreadPool;
import net.ionoff.center.shared.dto.DateTimeDto;
import net.ionoff.center.shared.dto.ServerInfoDto;
import net.ionoff.center.shared.dto.VersionDto;

@RestController
@EnableWebMvc
public class SystemServiceController {
	
	private final Logger logger = Logger.getLogger(SystemServiceController.class.getName());

	private static final SimpleDateFormat TIME_FORMATTER = new SimpleDateFormat("HH:mm");
	private static final SimpleDateFormat DATE_FORMATTER = new SimpleDateFormat("dd/MM/yyyy");

	@Autowired
	private IVersionService versionService;
	
	@Autowired
	private LocalSessionFactoryBean sessionFactory;
	
	@Autowired
	private ServerThreadPool serverThreadPool;
	
	@RequestMapping(value = "system/info", method = RequestMethod.GET, produces = "application/json; charset=utf-8")
	@ResponseBody
	public ServerInfoDto getServerInfo() {
		
		ServerInfoDto serverInfo = new ServerInfoDto();
		
		Date date = new Date();
		serverInfo.setServerDate(DATE_FORMATTER.format(date));
		serverInfo.setServerTime(TIME_FORMATTER.format(date));
		
		// Total number of processors or cores available to the JVM
		// Runtime.getRuntime().availableProcessors())
		
		
		long totalMem = Runtime.getRuntime().totalMemory();
		long freeMem = Runtime.getRuntime().freeMemory();
		
		int usedMemPercent = (int) (100 - freeMem * 100 /totalMem);
		
		serverInfo.setMemoryUsedPercent(usedMemPercent);
		
		// Total amount of free memory available to the JVM
		serverInfo.setFreeMemory(formatMBSize(freeMem));
		
		/* Total memory currently available to the JVM */
		serverInfo.setTotalMemory(formatMBSize(totalMem));
		
		/* Get a list of all filesystem roots on this system */
		File[] roots = File.listRoots();

		serverInfo.setDiskSpaceUsedPercent(0);
		if (roots.length > 0) {
			long totalSpace = roots[0].getTotalSpace();
			long freeSpace = roots[0].getFreeSpace();
			
			int usedSpacePercent = (int) (100 - freeSpace * 100 /totalSpace);
			
			serverInfo.setTotalDiskSpace(formatGBSize(totalSpace));
			serverInfo.setFreeDiskSpace(formatGBSize(freeSpace));
			serverInfo.setDiskSpaceUsedPercent(usedSpacePercent);
			
		}		
		return serverInfo;
	}
	
	@RequestMapping(value = "system/datetime", method = RequestMethod.GET, produces = "application/json; charset=utf-8")
	@ResponseBody
	public DateTimeDto getServerDateTime() {
		Date date = new Date();
		DateTimeDto dateTime = new DateTimeDto();
		dateTime.setDate(DATE_FORMATTER.format(date));
		dateTime.setTime(TIME_FORMATTER.format(date));
		return dateTime;
	}
	

	@RequestMapping(value = "versions/upgrade",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public VersionDto upgradeLatestVersion(HttpServletRequest request) {
		VersionDto versionDto = checkLatestVersion(request);
		versionDto = getLatestVersion(request);
		VersionDto currentVersion = getCurrentVersion();
		if (currentVersion.getDateTime() != null && versionDto.getDateTime() != null 
				&& currentVersion.getDateTime().compareTo(versionDto.getDateTime()) < 0) {
			
			logger.info("ServerThreadPool is shutting down...");
			serverThreadPool.shutdown();
			
			closeSessionFactory();
			
			String hostName = getUpdatorServiceUrl(request);
			String upgradeVersionPath = AppConfig.getInstance().UPDATOR_SERVICE_CONTEXT + "/versions/upgrade";
			String latestVersionUrl = hostName + "/" + upgradeVersionPath + "?currentVerionDateTime=" + currentVersion.getDateTime();
			RestTemplate restTemplate = new RestTemplate();
			ResponseEntity<String> response = restTemplate.postForEntity(latestVersionUrl, new HashMap<String, String>() , String.class);
			versionDto = new Gson().fromJson(response.getBody(), VersionDto.class);
		}
		return versionDto;
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
			v.setName("1.0.1");
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
	public VersionDto getLatestVersion(HttpServletRequest request) {
		VersionDto versionDto = new VersionDto();
		
		VersionDto currentVersion = getCurrentVersion();
		RestTemplate restTemplate = new RestTemplate();
		String hostName = getUpdatorServiceUrl(request);
		String latestVersionPath = AppConfig.getInstance().UPDATOR_SERVICE_CONTEXT + "/versions/latest";
		String latestVersionUrl = hostName  + latestVersionPath;
		try {
			ResponseEntity<String> response = restTemplate.getForEntity(latestVersionUrl, String.class);
			logger.info("Latest version return from updator service: \n" + response.getBody());
			versionDto = new Gson().fromJson(response.getBody(), VersionDto.class);
			if (currentVersion.getDateTime() == null || 
					versionDto.getDateTime().compareTo(currentVersion.getDateTime()) > 0) {
				
				versionDto.setDateTime(versionDto.getDateTime());
				versionDto.setName(versionDto.getName() + "-" + versionDto.getDateTime());
			}
			else {
				versionDto.setName(null);
				versionDto.setDateTime(null);
			}
			return versionDto;
		}
		catch (Exception e) {
			return versionDto;
		}
	}
	
	@RequestMapping(value = "versions/check",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public VersionDto checkLatestVersion(HttpServletRequest request) {
		VersionDto versionDto = new VersionDto();
		
		RestTemplate restTemplate = new RestTemplate();
		String hostName = getUpdatorServiceUrl(request);
		String latestVersionPath = AppConfig.getInstance().UPDATOR_SERVICE_CONTEXT + "/versions/check";
		String latestVersionUrl = hostName  + latestVersionPath;
		try {
			ResponseEntity<String> response = restTemplate.getForEntity(latestVersionUrl, String.class);
			
			logger.info("Latest version return from updator service: \n" + response.getBody());
			
			versionDto = new Gson().fromJson(response.getBody(), VersionDto.class);
			
			VersionDto currentVersion = getCurrentVersion();
			if (currentVersion.getDateTime() == null || 
					versionDto.getDateTime().compareTo(currentVersion.getDateTime()) > 0) {
				
				versionDto.setName(versionDto.getName() + "-" + versionDto.getDateTime());
				versionDto.setDateTime(versionDto.getDateTime());
			}
			else {
				versionDto.setName(null);
				versionDto.setDateTime(null);
			}
			return versionDto;
			
		}
		catch (Exception e) {
			logger.error(e.getMessage(), e);
			return versionDto;
		}
	}
	
	private boolean closeSessionFactory() {
	    boolean done = false;
	    SessionFactory factory = sessionFactory.getObject();
	    if(factory instanceof SessionFactoryImpl) {
	        SessionFactoryImpl sf = (SessionFactoryImpl)factory;
	        ConnectionProvider conn = sf.getConnectionProvider();
	        if(conn instanceof C3P0ConnectionProvider) { 
	            ((C3P0ConnectionProvider)conn).stop();
	            try {
	                Thread.sleep(2000); //Let give it time...it is enough...probably
	            } catch (InterruptedException e) {
	                // ignore
	            }
	            done = true;
	        }
	        factory.close();
	    }
	    return done;

	}
	
	private static String getUpdatorServiceUrl(HttpServletRequest request) {
		String url = request.getRequestURL().toString();
		String contextPath = request.getContextPath().toString();
		return url.substring(0, url.indexOf(contextPath));
	}

	@SuppressWarnings("unused")
	private String formatByteSize(long size) {
		String hrSize = null;

		double b = size;
		double k = size / 1024.0;
		double m = ((size / 1024.0) / 1024.0);
		double g = (((size / 1024.0) / 1024.0) / 1024.0);
		double t = ((((size / 1024.0) / 1024.0) / 1024.0) / 1024.0);

		DecimalFormat dec = new DecimalFormat("0.00");

		if (t > 1) {
			hrSize = dec.format(t).concat(" TB");
		} else if (g > 1) {
			hrSize = dec.format(g).concat(" GB");
		} else if (m > 1) {
			hrSize = dec.format(m).concat(" MB");
		} else if (k > 1) {
			hrSize = dec.format(k).concat(" KB");
		} else {
			hrSize = dec.format(b).concat(" Bytes");
		}

		return hrSize;
	}
	
	private double formatMBSize(long size) {
		return ((size / 1024.0) / 1024.0);
	}
	
	private double formatGBSize(long size) {
		return (((size / 1024.0) / 1024.0) / 1024.0);
	}
}
