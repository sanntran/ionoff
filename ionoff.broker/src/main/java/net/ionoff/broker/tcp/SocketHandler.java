package net.ionoff.broker.tcp;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.net.Socket;
import java.util.*;

public class SocketHandler extends Thread {

	private static final Logger LOGGER = LoggerFactory.getLogger(SocketHandler.class);
	static final int DEFAULT_TIMEOUT = 32000;
	static final int BUFSIZE = 8129;


	private final Socket socket;
	private PrintWriter writer;

	private String socketId;
	private final String clientIp;
	private final long initTime;
	private long timeOut;

	private boolean closed = false;
	private final SocketManager socketManager;
	InputStream inputStream;

	OutputStream  outputStream;

	public SocketHandler(Socket socket, SocketManager socketManager) throws IOException {
		this.socket = socket;
		this.socketId = UUID.randomUUID().toString();
		this.socketManager = socketManager;
		initTime = System.currentTimeMillis();
		timeOut = initTime + DEFAULT_TIMEOUT;
		//dataInputStream = new DataInputStream(socket.getInputStream());

		inputStream = socket.getInputStream();
		outputStream = socket.getOutputStream();


		clientIp = socket.getRemoteSocketAddress().toString().replace("/", "").split(":")[0];
		//inputStream = new BufferedInputStream(socket.getInputStream(), BUFSIZE);
		writer = new PrintWriter(outputStream, true);
	}
	
	private void handleSocket() throws IOException {
		try {
			HttpRequest httpRequest =  new HttpRequest(inputStream);
			httpRequest.readRequest();
			handleHttpRequest(httpRequest);
		}
		catch (HttpException he) {
			String message = he.getMessage();
			handlePicMessage(message);
		} catch (ClientException ce) {

		} catch (ServerException se) {

		} catch (Throwable t) {

		}
	}

	private void handlePicMessage(String message) {

	}

	private void handleHttpRequest(HttpRequest httpRequest) {

	}






	@Override
	public void run() {
		addToSocketManager();
		try {
			handleSocket();
		} catch (IOException e) {
			LOGGER.info("Error when handle socket. " + e.getMessage(), e);
		}
		if (!isLocked()) {
			LOGGER.info("Connection of " + clientId + " is locked. " + getDate());
			close();
		}
	}

	private void addToSocketManager() {
		socketManager.putToSocketHandlers(socketId, this);
	}

	private void addClientIdToSocketManager() {
		SocketHandler conn = socketManager.findSocketHandler(clientId);
		if (conn != null) {
			conn.close();
			socketManager.removeSocketHandler(clientId);
		}
		socketManager.putToSocketIds(clientId, socketId);
	}

	synchronized String sendCommand(String command) throws IOException {
		timeOut = System.currentTimeMillis() + 5000;
		writer.println(command);
		//String resp = reader.readLine();
		close();
		return "sd";
	}

	synchronized void sendResponse(String response) throws IOException {
		timeOut = System.currentTimeMillis() + 5000;

		writer.println("HTTP/1.1 200 OK\r\n");
		writer.println("Content-Type: text/html\r\n\r\n");
		writer.println("<html><head></head><body><h1>Hello</h1></body></html>");

		close();
	}

	public String getClientId() {
		return clientId;
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
			return isTimeOut();  // 32 seconds
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
				writer.close();
			}
			catch (Exception e) {
				// ignore
			}
			try {
				//reader.close();
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
				LOGGER.info("Connection " + clientId + " is closed");
			}
		}
	}
	
	@Override
	public String toString() {
		return new StringBuilder().append("clientId: ").append(clientId).append(", clientIp: ").append(clientIp).toString();
	}


	public Date getDate() {
		return new Date(initTime);
	}
}
