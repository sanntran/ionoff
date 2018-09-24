package net.ionoff.broker.tcp.handler;

import com.google.gson.Gson;
import net.ionoff.broker.mqtt.MqttBroker;
import net.ionoff.broker.tcp.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.net.Socket;
import java.util.*;

public class SocketHandler extends Thread {

	private static final Logger LOGGER = LoggerFactory.getLogger(SocketHandler.class);
	public static final int DEFAULT_TIMEOUT = 32000;
	private static final Gson GSON = new Gson();

	private final Socket socket;
	private PrintWriter printWriter;

	private String socketId;
	private final String clientIp;
	private final long initTime;
	private long timeOut;

	private boolean closed = false;
	private final TcpBroker tcpBroker;
	private final InputStream inputStream;
	private final OutputStream  outputStream;
	private final BufferedReader bufferedReader;
	private final MqttBroker mqttBroker;

	public SocketHandler(Socket socket, TcpBroker tcpBroker,
	                     MqttBroker mqttBroker) throws IOException {
		this.socket = socket;
		this.socketId = UUID.randomUUID().toString();
		this.tcpBroker = tcpBroker;
		this.mqttBroker = mqttBroker;

		initTime = System.currentTimeMillis();
		timeOut = initTime + DEFAULT_TIMEOUT;

		inputStream = socket.getInputStream();
		outputStream = socket.getOutputStream();
		bufferedReader = new BufferedReader(new InputStreamReader(inputStream));

		clientIp = socket.getRemoteSocketAddress().toString().replace("/", "").split(":")[0];
		printWriter = new PrintWriter(outputStream, true);
	}

	public String getSocketId() {
		return socketId;
	}

	private void handleSocket() {
		try {
			HttpRequest httpRequest =  new HttpRequest(inputStream);
			httpRequest.readRequest();
			handleHttpRequest(httpRequest);
			close();
		}
		catch (HttpException he) {
			String message = he.getMessage();
			if (message != null && message.split(":").length == 2) {
				handlePicMessage(message);
			}
			else {
				printWriter.println("Bad Request: " + message);
				close();
			}
		} catch (ClientException ce) {
			ResponseBody response = new ResponseBody(ce.getStatus().getStatus(),
					ce.getStatus().getDescription(), ce.getMessage());
			String data = GSON.toJson(response);
			sendResponse(ce.getStatus(), data);

		} catch (ServerException se) {
			ResponseBody response = new ResponseBody(Status.INTERNAL_ERROR.getStatus(),
					Status.INTERNAL_ERROR.getDescription(), se.getMessage());
			String data = GSON.toJson(response);
			sendResponse(Status.INTERNAL_ERROR, data);
		} catch (Throwable t) {
			ResponseBody response = new ResponseBody(Status.INTERNAL_ERROR.getStatus(),
					Status.INTERNAL_ERROR.getDescription(), t.getMessage());
			String data = GSON.toJson(response);
			sendResponse(Status.INTERNAL_ERROR, data);
		}
	}

	private void handlePicMessage(String message) {
		PicMessage picMessage = new PicMessage(message);
		addClientIdToSocketManager(picMessage);
		publishMessageToCollector(message);
	}

	private void publishMessageToCollector(String message) {
		mqttBroker.publishMessage(MqttBroker.TOPIC_IONOFF, message);
	}

	private void handleHttpRequest(HttpRequest httpRequest) {
		ResponseBody response = new HttpHandler(tcpBroker, mqttBroker).handleHttpRequest(httpRequest);
		sendResponse(Status.OK, GSON.toJson(response));
	}

	@Override
	public void run() {
		handleSocket();
	}

	private void addClientIdToSocketManager(PicMessage picMessage) {
		SocketHandler socketHandler = tcpBroker.getSocketManager().findSocketHandler(picMessage.getKeyId());
		if (socketHandler != null) {
			socketHandler.close();
			tcpBroker.getSocketManager().removeSocketHandler(picMessage.getKeyId());
		}
		tcpBroker.getSocketManager().putToSocketIds(picMessage.getKeyId(), socketId);
	}

	public synchronized String sendCommand(String command) throws IOException {
		timeOut = System.currentTimeMillis() + 5000;
		printWriter.println(command);
		String resp = bufferedReader.readLine();
		close();
		return resp;
	}

	synchronized void sendResponse(Status status, String response) {
		timeOut = System.currentTimeMillis() + 5000;
		try {
			HttpResponse.newFixedLengthResponse(status, response).send(outputStream);
		} catch (Throwable t) {
			LOGGER.error(t.getMessage(), t);
		}
		close();
	}

	public String getClientIp() {
		return clientIp;
	}

	public long getInitTime() {
		return initTime;
	}


	public boolean isTimeOut() {
		return System.currentTimeMillis() > timeOut;
	}

	public boolean isLocked() {
		if (!closed) {
			return isTimeOut();
		}
		return true;
	}

	public long getDuration() {
		return System.currentTimeMillis() - initTime;
	}

	public void close() {
		if (!closed) {
			closed = true;
			try {
				printWriter.close();
			}
			catch (Exception e) {
				// ignore
			}
			try {
				outputStream.close();
			}
			catch (Exception e) {
				// ignore
			}
			try {
				inputStream.close();
			}
			catch (Exception e) {
				// ignore
			}
			try {
				socket.shutdownInput();
				socket.shutdownOutput();
				socket.close();
			}
			catch (Exception e) {
				// Ignore this exception
			}
			finally {
				LOGGER.info("Connection " + socketId + " is closed");
				tcpBroker.getSocketManager().removeSocketHandler(socketId);
			}
		}
	}
	
	@Override
	public String toString() {
		return new StringBuilder().append("socketId: ").append(socketId).append(", clientIp: ").append(clientIp).toString();
	}

	public Date getDate() {
		return new Date(initTime);
	}
}
