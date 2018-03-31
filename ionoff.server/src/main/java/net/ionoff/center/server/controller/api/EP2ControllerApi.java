package net.ionoff.center.server.controller.api;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;

import net.ionoff.center.server.entity.Controller;
import net.ionoff.center.shared.entity.ControllerModel;

public class EP2ControllerApi implements IControllerApi {
	
	@Override
	public ControllerStatus getStatus(Controller controller) 
			throws ControllerException {
		if (!controller.isConnected()) {
			throw new ControllerConnectException(controller.getIp());
		}
		return sendHttpRequestStatus(controller);
	}
	
	@Override
	public void openRelay(Controller controller, int relayIndex) 
			throws ControllerException {
		if (!controller.isConnected()) {
			throw new ControllerConnectException(controller.getIp());
		}
		String params = getParamOpenRelay(relayIndex);
		sendHttpRequestControl(controller, params);
	}
	
	@Override
	public void closeRelay(Controller controller, int relayIndex) 
			throws ControllerException {
		if (!controller.isConnected()) {
			throw new ControllerConnectException(controller.getIp());
		}
		String params = getParamCloseRelay(relayIndex);
		sendHttpRequestControl(controller, params);
	}
	
	@Override
	public void closeOpenRelay(Controller controller, int relayIndex)
			throws ControllerException {
		if (!controller.isConnected()) {
			throw new ControllerConnectException(controller.getIp());
		}
		String params = getParamCloseOpenRelay(relayIndex);
		sendHttpRequestControl(controller, params);
	}
	
	private ControllerStatus sendHttpRequestStatus(Controller controller) throws ControllerException {
		String params = "AllIOS.cgi";
		String resp = null;
		try {
			resp = sendHttpGETRequest(buildRequestUrl(controller.getIp(), controller.getPort(),  params));
		} catch (IOException e) {
			throw new ControllerConnectException(controller.getIp());
		}
		ControllerStatus status = new ControllerStatus();
		String s[] = resp.split(" ");
		if (s.length != ControllerModel.HLAB_EP2.getRelayOutput()) {
			throw new ControllerApiException("Invalid response string format!");
		}
		for (int i = 0; i < ControllerModel.HLAB_EP2.getRelayOutput(); i++) {
			if ("1".equals(s[i])) {
				status.getRelayOutputStatus().add(true);
			}
			else {
				status.getRelayOutputStatus().add(false);
			}
		}
		return status;
	}	
	
	private ControllerStatus sendHttpRequestControl(Controller controller, String params) throws ControllerException {
		String reqUrl = buildRequestUrl(controller.getIp(), controller.getPort(), params);
		try {
			getResponseCodeHttpGETRequest(reqUrl);
		} catch (IOException e) {
			throw new ControllerConnectException(controller.getIp());
		}
		return null;
	}
	
	private String buildRequestUrl(String ip, int port, String params) {
		return "http://" + ip + ":" + port + "/" + params;
	}
	
	@SuppressWarnings("unused")
	private String getParamChangeStatus(int relayIndex) {
		String code = "";
		switch (relayIndex) {
		case 0:
			code = "2?2=IO0";
			break;
		case 1:
			code = "2?3=IO1";
			break;
		case 2:
			code = "2?4=IO2";
			break;
		case 3:
			code = "2?5=IO3";
			break;
		case 4:
			code = "2?6=IO4";
			break;
		case 5:
			code = "2?7=IO5";
			break;
		case 6:
			code = "2?8=IO6";
			break;
		case 7:
			code = "2?9=IO7";
			break;
		case 8:
			code = "3?0=IO8";
			break;
		case 9:
			code = "3?1=IO9";
			break;
		case 10:
			code = "3?2=IO10";
			break;
		case 11:
			code = "3?3=IO11";
			break;
		case 12:
			code = "3?4=IO12";
			break;
		case 13:
			code = "3?5=IO13";
			break;
		case 14:
			code = "3?6=IO14";
			break;
		case 15:
			code = "3?7=IO15";
			break;
		case 16:
			code = "3?8=IO16";
			break;
		case 17:
			code = "3?9=IO17";
			break;
		case 18:
			code = "3?a=IO18";
			break;
		case 19:
			code = "3?b=IO19";
			break;
		}
		return code;
	}

	private String getParamOpenRelay(int relayIndex) {
		String code = "";
		switch (relayIndex) {
		case 0:
			code = "6?2=IO0";
			break;
		case 1:
			code = "6?3=IO1";
			break;
		case 2:
			code = "6?4=IO2";
			break;
		case 3:
			code = "6?5=IO3";
			break;
		case 4:
			code = "6?6=IO4";
			break;
		case 5:
			code = "6?7=IO5";
			break;
		case 6:
			code = "6?8=IO6";
			break;
		case 7:
			code = "6?9=IO7";
			break;
		case 8:
			code = "7?0=IO8";
			break;
		case 9:
			code = "7?1=IO9";
			break;
		case 10:
			code = "7?2=IO10";
			break;
		case 11:
			code = "7?3=IO11";
			break;
		case 12:
			code = "7?4=IO12";
			break;
		case 13:
			code = "7?5=IO13";
			break;
		case 14:
			code = "7?6=IO14";
			break;
		case 15:
			code = "7?7=IO15";
			break;
		case 16:
			code = "7?8=IO16";
			break;
		case 17:
			code = "7?9=IO17";
			break;
		case 18:
			code = "7?a=IO18";
			break;
		case 19:
			code = "7?b=IO19";
			break;
		}
		return code;
	}

	private String getParamCloseRelay(int relayIndex) {
		String code = "";
		switch (relayIndex) {
		case 0:
			code = "8?2=IO0";
			break;
		case 1:
			code = "8?3=IO1";
			break;
		case 2:
			code = "8?4=IO2";
			break;
		case 3:
			code = "8?5=IO3";
			break;
		case 4:
			code = "8?6=IO4";
			break;
		case 5:
			code = "8?7=IO5";
			break;
		case 6:
			code = "8?8=IO6";
			break;
		case 7:
			code = "8?9=IO7";
			break;
		case 8:
			code = "9?0=IO8";
			break;
		case 9:
			code = "9?1=IO9";
			break;
		case 10:
			code = "9?2=IO10";
			break;
		case 11:
			code = "9?3=IO11";
			break;
		case 12:
			code = "9?4=IO12";
			break;
		case 13:
			code = "9?5=IO13";
			break;
		case 14:
			code = "9?6=IO14";
			break;
		case 15:
			code = "9?7=IO15";
			break;
		case 16:
			code = "9?8=IO16";
			break;
		case 17:
			code = "9?9=IO17";
			break;
		case 18:
			code = "9?a=IO18";
			break;
		case 19:
			code = "9?b=IO19";
			break;
		}
		return code;
	}

	String getParamCloseOpenRelay(int relayIndex) {
		String code = "";
		switch (relayIndex) {
		case 0:
			code = "4?2=IO0";
			break;
		case 1:
			code = "4?3=IO1";
			break;
		case 2:
			code = "4?4=IO2";
			break;
		case 3:
			code = "4?5=IO3";
			break;
		case 4:
			code = "4?6=IO4";
			break;
		case 5:
			code = "4?7=IO5";
			break;
		case 6:
			code = "4?8=IO6";
			break;
		case 7:
			code = "4?9=IO7";
			break;
		case 8:
			code = "5?0=IO8";
			break;
		case 9:
			code = "5?1=IO9";
			break;
		case 10:
			code = "5?2=IO10";
			break;
		case 11:
			code = "5?3=IO11";
			break;
		case 12:
			code = "5?4=IO12";
			break;
		case 13:
			code = "5?5=IO13";
			break;
		case 14:
			code = "5?6=IO14";
			break;
		case 15:
			code = "5?7=IO15";
			break;
		case 16:
			code = "5?8=IO16";
			break;
		case 17:
			code = "5?9=IO17";
			break;
		case 18:
			code = "5?a=IO18";
			break;
		case 19:
			code = "5?b=IO19";
			break;
		}
		return code;
	}
	
	public static String sendHttpGETRequest(String url) throws IOException  {
		//LOGGER.info("URL: " + url);
		URL obj = new URL(url);
		HttpURLConnection con = (HttpURLConnection) obj.openConnection();

		// optional default is GET
		con.setRequestMethod("GET");
		con.setConnectTimeout(5000);
		con.setReadTimeout(5000);
		// add request header
		con.setRequestProperty("User-Agent", "Mozilla/5.0");
		// int responseCode = con.getResponseCode();
		
		BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream()));
		String inputLine;
		StringBuffer response = new StringBuffer();

		while ((inputLine = in.readLine()) != null) {
			response.append(inputLine);
		}
		in.close();
		// print result
		//LOGGER.info("Response: " + response);
		return response.toString();
	}
	
	public static int getResponseCodeHttpGETRequest(String url) throws IOException {
		//LOGGER.info("URL: " + url);
		URL obj = new URL(url);
		HttpURLConnection con = (HttpURLConnection) obj.openConnection();

		// optional default is GET
		con.setRequestMethod("GET");
		// add request header
		con.setRequestProperty("User-Agent", "Mozilla/5.0");
		return con.getResponseCode(); // 200; 404...
	}
}
