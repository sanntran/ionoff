package net.ionoff.center.server.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;

import net.ionoff.center.server.relaydriver.LazyRelayDriverThread;
import org.apache.log4j.Logger;

public class HttpRequestUtil {
	
	private static final int DEFAULT_READ_IME_OUT = 3000;
	private static final int DEFAULT_CONNECT_TIME_OUT = 3000;

	private static final Logger LOGGER = Logger.getLogger(HttpRequestUtil.class.getName());

	public static String sendGetRequest(String url) throws IOException {
		return sendGetRequest(url, DEFAULT_CONNECT_TIME_OUT, DEFAULT_READ_IME_OUT);
	}

	public static String sendGetRequest(String url, int connectTimeOut, int readTimeOut) throws IOException {
		LOGGER.debug(">>>>>>>> Send GET request to URL: " + url);
		HttpURLConnection httpConnection = null;
		BufferedReader bufferedReader = null;
		try {
			URL obj = new URL(url);
			httpConnection = (HttpURLConnection) obj.openConnection();
			httpConnection.setRequestMethod("GET");
			httpConnection.setConnectTimeout(connectTimeOut);
			httpConnection.setReadTimeout(readTimeOut);
			httpConnection.setRequestProperty("User-Agent", "Mozilla/5.0");
			bufferedReader = new BufferedReader(new InputStreamReader(httpConnection.getInputStream()));
			String line;
			StringBuffer response = new StringBuffer();
			while ((line = bufferedReader.readLine()) != null) {
				response.append(line);
			}
			return response.toString();
		} catch (IOException e) {
			throw e;
		} finally {
			try {
				if (bufferedReader != null) {
					bufferedReader.close();
				}
				if (httpConnection != null) {
					httpConnection.disconnect();
				}
			} catch (Exception e) {
				LOGGER.error("Error close http connection to " + url + " " + e.getMessage());
			}
		}




		// print result
		// logger.info("Response: " + response);

	}

}
