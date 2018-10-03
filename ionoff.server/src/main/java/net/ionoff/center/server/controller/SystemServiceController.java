package net.ionoff.center.server.controller;

import java.io.File;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate4.LocalSessionFactoryBean;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

import net.ionoff.center.shared.dto.DateTimeDto;
import net.ionoff.center.shared.dto.ServerInfoDto;

@RestController
@EnableWebMvc
public class SystemServiceController {
	
	private final Logger logger = Logger.getLogger(SystemServiceController.class.getName());

	private static final SimpleDateFormat TIME_FORMATTER = new SimpleDateFormat("HH:mm");
	private static final SimpleDateFormat DATE_FORMATTER = new SimpleDateFormat("dd/MM/yyyy");

	@Autowired
	private LocalSessionFactoryBean sessionFactory;
	
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
