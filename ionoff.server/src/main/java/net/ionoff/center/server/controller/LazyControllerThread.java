package net.ionoff.center.server.controller;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import net.ionoff.center.server.entity.Controller;
import net.ionoff.center.server.persistence.service.IControllerService;
import net.ionoff.center.server.util.HttpRequestUtil;

@Component
@EnableAsync
@EnableScheduling
public class LazyControllerThread {
	
	private static final Logger LOGGER = LoggerFactory.getLogger(LazyControllerThread.class.getName());

	private static final long INTERVAl = 5000; // 5 seconds
	private static final int CONNECT_TIME_OUT = 2000; // milliseconds
	private static final int READ_TIME_OUT = 2000; // milliseconds

	@Lazy
	@Autowired
    IControllerService controllerService;

	@Lazy
	@Autowired
	ControllerHandler controllerHandler;

	@Scheduled(fixedDelay = INTERVAl)
    public void scanControllersStatus() {
		List<Controller> lazyControllers = controllerService.findByIsLazy();
		for (Controller controller : lazyControllers) {
			scanControllerStatus(controller);
		}
	}
	
	@Async
	protected void scanControllerStatus(Controller controller) {
		try {
			final String status = sendStatusRequest(controller);
			controllerHandler.onMessageArrived(status);
		}
		catch (final Exception e) {
			LOGGER.error("Error "  + controller.getIp() + ":" + controller.getPort() + ": "+ e.getMessage());
		}
	}

	private String sendStatusRequest(Controller controller) throws IOException {
		List<String> urls;
		String command = controller.getCommandStatus();
		if (command != null && command.startsWith("[") && command.endsWith("]")) {
			String items[] = command.substring(1, command.length() - 1).split(", ");
			urls = Arrays.asList(items);
		}
		else {
			urls = new ArrayList<>();
			urls.add(command);
		}
		StringBuilder responseBuilder = new StringBuilder();
		responseBuilder.append("key=").append(controller.getKey());
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