package net.xapxinh.center.server.api.player;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.Map;
import java.util.Observable;
import java.util.Observer;

import org.apache.log4j.Logger;
import com.google.gson.Gson;

public class PlayerConnection extends Observable implements Runnable {

	private static final Logger LOGGER = Logger.getLogger(PlayerConnection.class.getName());

	private Long id;
	private String name;
	private String pass;
	private String password;
	private final Socket socket;
	private final PrintWriter writer;
	private final BufferedReader reader;
	private Thread thread;
	private boolean keepAlive;

	public PlayerConnection(Socket socket, Observer ...observers) throws IOException {
		this.socket = socket;
		for (Observer observer : observers) {
			addObserver(observer);
		}
		this.reader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
		this.writer = new PrintWriter(socket.getOutputStream(), true);
	}

	public void close() {
		writer.close();
		try {
			reader.close();
		} catch (IOException e) {
			// Ignore
		}
		try {
			socket.close();
		} catch (IOException e) {
			// Ignore
		}
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getPass() {
		return pass;
	}

	public void setPass(String pass) {
		this.pass = pass;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public Socket getSocket() {
		return socket;
	}

	public PrintWriter getWriter() {
		return writer;
	}

	public BufferedReader getReader() {
		return reader;
	}

	public String getIP() {
		return socket.getLocalSocketAddress().toString();
	}

	public String getClientIP() {
		return socket.getLocalSocketAddress().toString();
	}

	public void start() throws IOException {
		thread = new Thread(this);
		thread.start();
	}

	@Override
	public void run() {
		try {
			readMessage();
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
		}
	}

	private void readMessage() throws IOException {

		final BufferedReader reader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
		final Map<String, String> requestMessages = getRequestParams(reader.readLine());

		final String status = requestMessages.get("status");
		if (status != null) {
			// this message is just a interval pinging
			keepAlive = false;
		} else {
			keepAlive = true;
		}
		onRecievedMesasge(requestMessages);
	}

	@SuppressWarnings("unchecked")
	private Map<String, String> getRequestParams(String readLine) {
		return new Gson().fromJson(readLine, Map.class);
	}

	private void onRecievedMesasge(Map<String, String> requestMessages) {
		setChanged();
		notifyObservers(new RecievedMessageEvent(this, requestMessages));
	}

	public String sendString(String tcpParametter) throws IOException {
		getWriter().println(tcpParametter);
		return getReader().readLine();
	}

	public boolean isKeepAlive() {
		return this.keepAlive;
	}
}
