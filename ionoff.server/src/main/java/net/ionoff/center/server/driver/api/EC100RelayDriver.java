package net.ionoff.center.server.driver.api;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.log4j.Logger;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

import net.ionoff.center.server.entity.RelayDriver;

public class EC100RelayDriver implements IRelayDriver {
	
	private static Logger LOGGER = Logger.getLogger(EC100RelayDriver.class.getName());
	
	@Override
	public RelayDriverStatus getStatus(RelayDriver driver) 
			throws RelayDriverException {
		
		if (!driver.isConnected()) {
			throw new RelayDriverConnectException(driver.getIp());
		}
		
		RelayDriverStatus driverStatus = new RelayDriverStatus();
		
		//$SS/0630,0590,0000,0000/1,1,1,1,z,z,z,z/0,0,0,0,z,z,z,z
		String[] responseRo1 = requestGetStatusArr(driver.getIp(), driver.getPort(), ROOM_ID.ROOM1.getValue());
		List<Boolean> inputStatusRo1 = getDigitalInputsStatus(responseRo1, ROOM_ID.ROOM1.getValue());
		List<Boolean> relayStatusRo1 = getRelayOutputsStatus(responseRo1, ROOM_ID.ROOM1.getValue());
		try {
			Thread.sleep(500);
		} catch (InterruptedException e) {
			// ignore this exception
		}
		
		String[] responseRo2 = requestGetStatusArr(driver.getIp(), driver.getPort(), ROOM_ID.ROOM2.getValue());
		List<Boolean> inputStatusRo2 = getDigitalInputsStatus(responseRo2, ROOM_ID.ROOM2.getValue());
		List<Boolean> relayStatusRo2 = getRelayOutputsStatus(responseRo2, ROOM_ID.ROOM2.getValue());
		try {
			Thread.sleep(500);
		} catch (InterruptedException e) {
			// ignore this exception
		}
		
		String[] responseRo3 = requestGetStatusArr(driver.getIp(), driver.getPort(), ROOM_ID.ROOM3.getValue());
		List<Boolean> inputStatusRo3 = getDigitalInputsStatus(responseRo3, ROOM_ID.ROOM3.getValue());
		List<Boolean> relayStatusRo3 = getRelayOutputsStatus(responseRo3, ROOM_ID.ROOM3.getValue());
		try {
			Thread.sleep(500);
		} catch (InterruptedException e) {
			// ignore this exception
		}
		
		String[] responseR4 = requestGetStatusArr(driver.getIp(), driver.getPort(), ROOM_ID.ROOM4.getValue());
		List<Boolean> inputStatusRo4 = getDigitalInputsStatus(responseR4, ROOM_ID.ROOM4.getValue());
		List<Boolean> relayStatusRo4 = getRelayOutputsStatus(responseR4, ROOM_ID.ROOM4.getValue());
		//LOGGER.info(Joiner.on(",").join(relayStatusRo4));
	
		driverStatus.getDigitalInputStatus().addAll(inputStatusRo1);
		driverStatus.getDigitalInputStatus().addAll(inputStatusRo2);
		driverStatus.getDigitalInputStatus().addAll(inputStatusRo3);
		driverStatus.getDigitalInputStatus().addAll(inputStatusRo4);
		
		driverStatus.getRelayOutputStatus().addAll(relayStatusRo1);
		driverStatus.getRelayOutputStatus().addAll(relayStatusRo2);
		driverStatus.getRelayOutputStatus().addAll(relayStatusRo3);
		driverStatus.getRelayOutputStatus().addAll(relayStatusRo4);
		
		return driverStatus;
	}
	
	@Override
	public void openRelay(RelayDriver driver, int relayIndex) 
			throws RelayDriverException {
		if (!driver.isConnected()) {
			throw new RelayDriverConnectException(driver.getIp());
		}
		String params = "zctl.cgi?" + getRoomId(relayIndex) + "" + getRelayId(relayIndex) + "=0&0=OFF";
		String response;
		try {
			response = sendHttpGETRequest(buildRequestUrl(driver.getIp(), driver.getPort(), params));
		} catch (IOException e) {
			throw new RelayDriverConnectException(driver.getIp());
		}
		LOGGER.info("Change relay output status, driver ip: " + driver.getIp() + ". Response: " + response);
	}
	

	private int getRelayId(int relayIndex) {
		if (relayIndex < 4) {
			return relayIndex + 1;
		}
		if (relayIndex < 12) {
			return relayIndex - 4 + 1;
		}
		if (relayIndex < 20) {
			return relayIndex - 12 + 1;
		}
		// relayIndex < 28
		return relayIndex - 20 + 1;
	}
	
	private int getRoomId(int relayIndex) {
		if (relayIndex < 4) {
			return ROOM_ID.ROOM1.getValue();
		}
		if (relayIndex < 12) {
			return ROOM_ID.ROOM2.getValue();
		}
		if (relayIndex < 20) {
			return ROOM_ID.ROOM3.getValue();
		}
		// relayIndex < 28
		return ROOM_ID.ROOM4.getValue();
	}

	private String buildRequestUrl(String ip, int port, String params) {
		return "http://" + ip + ":" + port + "/" + params;
	}
	
	@Override
	public void closeRelay(RelayDriver driver, int relayIndex) 
			throws RelayDriverException {
		if (!driver.isConnected()) {
			throw new RelayDriverConnectException(driver.getIp());
		}
		String params = "zctl.cgi?" + getRoomId(relayIndex) + "" + getRelayId(relayIndex) + "=0&0=ON";
		String response;
		try {
			response = sendHttpGETRequest(buildRequestUrl(driver.getIp(), driver.getPort(), params));
		} catch (IOException e) {
			throw new RelayDriverConnectException(driver.getIp());
		}
		LOGGER.info("Change relay output status, driver ip: " + driver.getIp() + ". Response: " + response);
	}

	public enum ROOM_ID {
		ROOM1(1), ROOM2(2), ROOM3(3), ROOM4(4);
		private int value;
		private ROOM_ID(int value) {
			this.value = value;
		}
		public int getValue() {
			return this.value;
		}
	}

	private String[] requestGetStatusArr(String ip, int port, int roomId) throws RelayDriverApiException, RelayDriverConnectException {
		String uri = "http://" + ip + ":" + port + "/ss.cgi?" + roomId;
		String response;
		try {
			response = sendHttpGETRequest(uri);
		} catch (IOException e) {
			throw new RelayDriverConnectException(ip);
		}

		String responses[] = response.split("/");
		if (responses.length != 4) {
			throw new RelayDriverApiException("Unknown status format");
		}
		return responses;
	}

	private List<Boolean> getDigitalInputsStatus(String responses[], int roomId) {
		List<Boolean> inputStatus = new ArrayList<>();
		String digitalInputStatus = responses[2];
		String digitalInputStatusArr[] = digitalInputStatus.split(",");
		for (int i = 0; i < 4; i++) {
			inputStatus.add(toBoolean(digitalInputStatusArr[i]));
		}
		if (roomId != ROOM_ID.ROOM1.getValue()) {
			for (int i = 4; i < 8; i++) {
				inputStatus.add(toBoolean(digitalInputStatusArr[i]));
			}
		}
		return inputStatus;
	}

	private Boolean toBoolean(String string) {
		if ("1".equals(string)) {
			return true;
		}
		return false;
	}

	private List<Boolean> getRelayOutputsStatus(String responses[], int roomId) {
		List<Boolean> relayStatus = new ArrayList<>();
		String relayOutputStatus = responses[3];
		String relayOutputStatusArr[] = relayOutputStatus.split(",");
		for (int i = 0; i < 4; i++) {
			relayStatus.add(toBoolean(relayOutputStatusArr[i]));
		}
		if (roomId != ROOM_ID.ROOM1.getValue()) {
			for (int i = 4; i < 8; i++) {
				relayStatus.add(toBoolean(relayOutputStatusArr[i]));
			}
		}
		return relayStatus;
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

	public static Document sendHttpGETRequestDocument(String uri, String username, String password) throws IOException, ParserConfigurationException, SAXException   {

			URL obj = new URL(uri);
			HttpURLConnection con = (HttpURLConnection) obj.openConnection();

			// optional default is GET
			con.setRequestMethod("GET");
			// add request header
			con.setRequestProperty("User-Agent", "Mozilla/5.0");

			String userpass = username + ":" + password;
			String basicAuth = "Basic " + javax.xml.bind.DatatypeConverter.printBase64Binary(userpass.getBytes());
			con.setRequestProperty("Authorization", basicAuth);

			DocumentBuilderFactory objDocumentBuilderFactory = DocumentBuilderFactory.newInstance();
			DocumentBuilder objDocumentBuilder = objDocumentBuilderFactory.newDocumentBuilder();
			Document document = objDocumentBuilder.parse(con.getInputStream());
			return document;
		
	}

	/*
	 * public Document sendHttpGETRequestDocumen(String uri) throws Exception {
	 * URL obj = new URL(uri); HttpURLConnection con = (HttpURLConnection)
	 * obj.openConnection();
	 * 
	 * // optional default is GET con.setRequestMethod("GET"); //add request
	 * header con.setRequestProperty("User-Agent", "Mozilla/5.0"); int
	 * responseCode = con.getResponseCode();
	 * 
	 * logger.info("Response code: " + responseCode);
	 * 
	 * DocumentBuilderFactory objDocumentBuilderFactory =
	 * DocumentBuilderFactory.newInstance(); DocumentBuilder objDocumentBuilder
	 * = objDocumentBuilderFactory.newDocumentBuilder(); Document document =
	 * objDocumentBuilder.parse(con.getInputStream()); return document; }
	 */

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

	@SuppressWarnings("rawtypes")
	public static String sendHttpPOSTRequest(String url, HashMap<String, String> paramMap) throws IOException {

		URL obj = new URL(url);
		HttpURLConnection con = (HttpURLConnection) obj.openConnection();

		// add reuqest header
		con.setRequestMethod("POST");
		con.setRequestProperty("User-Agent", "Mozilla/5.0");
		con.setRequestProperty("Accept-Language", "en-US,en;q=0.5");

		String urlParameters = "";
		int paramCount = 0;
		for (Map.Entry entry : paramMap.entrySet()) {
			if (paramCount == 0) {
				urlParameters = urlParameters + (String) entry.getKey() + "=" + (String) entry.getValue();
			}
			else {
				urlParameters = urlParameters + "&" + (String) entry.getKey() + "=" + (String) entry.getValue();
			}
		}

		// Send post request
		con.setDoOutput(true);
		DataOutputStream wr = new DataOutputStream(con.getOutputStream());
		wr.writeBytes(urlParameters);
		wr.flush();
		wr.close();

		int responseCode = con.getResponseCode();
		//System.out.println("Response Code : " + responseCode);

		BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream()));
		String inputLine;
		StringBuffer response = new StringBuffer();

		while ((inputLine = in.readLine()) != null) {
			response.append(inputLine);
		}
		in.close();

		return response.toString();
	}

	@Override
	public void openRelay(RelayDriver driver, int relayIndex, Integer autoRevert) throws RelayDriverException {
		openRelay(driver, relayIndex);
	}

	@Override
	public void closeRelay(RelayDriver driver, int relayIndex, Integer autoRevert) throws RelayDriverException {
		closeRelay(driver, relayIndex);
		if (autoRevert != null && autoRevert.intValue() == 1) {
			try {
				Thread.sleep(1000);
			} catch (InterruptedException e) {
				//
			}
		}
		openRelay(driver, relayIndex);
	}
}
