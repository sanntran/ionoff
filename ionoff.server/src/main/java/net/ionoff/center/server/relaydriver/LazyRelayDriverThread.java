package net.ionoff.center.server.relaydriver;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import net.ionoff.center.server.entity.RelayDriver;
import net.ionoff.center.server.persistence.service.IRelayDriverService;
import net.ionoff.center.server.util.HttpRequestUtil;

@Component
@EnableAsync
@EnableScheduling
public class LazyRelayDriverThread {
	
	private static final Logger LOGGER = Logger.getLogger(LazyRelayDriverThread.class.getName());

	private static final long INTERVAl = 5000; // 5 seconds
	private static final int CONNECT_TIME_OUT = 2000; // milliseconds
	private static final int READ_TIME_OUT = 2000; // milliseconds

	@Autowired
	IRelayDriverService relayDriverService;

	@Autowired
	RelayDriverHandler relayDriverHandler;

	@Scheduled(fixedDelay = INTERVAl)
    public void scanRelayDriversStatus() {
		List<RelayDriver> lazyRelayDrivers = relayDriverService.findByIsLazy();
		for (RelayDriver relayDriver : lazyRelayDrivers) {
			scanRelayDriverStatus(relayDriver);
		}
	}
	
	@Async
	protected void scanRelayDriverStatus(RelayDriver relayDriver) {
		try {
			final String status = sendStatusRequest(relayDriver);
			relayDriverHandler.onMessageArrived(status);
		}
		catch (final Exception e) {
			LOGGER.error("Error "  + relayDriver.getIp() + ":" + relayDriver.getPort() + ": "+ e.getMessage());
		}
	}

	private String sendStatusRequest(RelayDriver relayDriver) throws IOException {
		List<String> urls;
		String command = relayDriver.getCommandStatus();
		if (command != null && command.startsWith("[") && command.endsWith("]")) {
			String items[] = command.substring(1, command.length() - 1).split(", ");
			urls = Arrays.asList(items);
		}
		else {
			urls = new ArrayList<>();
			urls.add(command);
		}
		StringBuilder responseBuilder = new StringBuilder();
		responseBuilder.append("key=").append(relayDriver.getKey());
		responseBuilder.append("&status=");
		for (int i = 0; i < urls.size(); i++) {
			String url = urls.get(i);
			if (i > 0) {
				responseBuilder.append(";");
			}
			String response = HttpRequestUtil.sendGetRequest(url, CONNECT_TIME_OUT, READ_TIME_OUT);
			responseBuilder.append(response);
			try {
				Thread.sleep(500);
			} catch (InterruptedException e) {
				LOGGER.error("InterruptedException " + e.getMessage());
			}
		}
		return responseBuilder.toString();
	}
}
