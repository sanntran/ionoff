package net.ionoff.center.server.relaydriver.api;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.Date;
import java.util.Observable;
import java.util.Observer;

public class RelayDriverConnection extends Observable implements Runnable {
	
	private int timeOut = 32000; // 32 seconds
	
	private final Socket socket;
	private final PrintWriter writer;
	private final BufferedReader reader;
	private final String ip;
	private final long time;
	
	private String message; 
	private String key;
	private String input; 
	private String output; 
	private boolean closed = false;
	private Thread thread;
	
	public RelayDriverConnection(Socket socket, Observer ...observers) throws IOException {
		this.socket = socket;
		time = System.currentTimeMillis();
		//System.out.println("Create connection " + getDate());
		for (Observer observer : observers) {
			addObserver(observer);
		}
		ip = socket.getRemoteSocketAddress().toString().replace("/", "").split(":")[0];
		reader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
		writer = new PrintWriter(socket.getOutputStream(), true);
	}
	
	public void start() throws IOException {
		thread = new Thread(this);
		thread.start();
		readMessage();
	}
	
	private void readMessage() throws IOException {
		message = reader.readLine();
		if (message != null && message.split(":").length == 2) {
			String messageItems[] = message.split(":");
			String dataItems[] = messageItems[1].split(",");
			if (dataItems.length == 3) {
				input = dataItems[0];
				output = dataItems[1];
				key = dataItems[2];
			}
		}
		onRecievedMesasge();
	}
	
	@Override
	public void run() {
		for (; timeOut > 0;) {
			try {
				Thread.sleep(100);
				//System.out.println("Connection " + getDate() + " " +  timeOut);
			} catch (InterruptedException e) {
				// Ignore this error
			}
			timeOut = timeOut - 100;
		}
		if (!closed) {
			System.out.println("Connection " + getDate() + " timeout ");
			close();
		}
	}

	synchronized String sendCommand(String req) throws IOException {
		//System.out.println("Connection " + getDate() + " sending command...");
		timeOut = 4000;
		writer.println(req);
		String resp = reader.readLine();
		close();
		return resp;
	}


	public String getMessage() {
		return message;
	}
	
	public String getKey() {
		return key;
	}

	public String getIp() {
		return ip;
	}

	public long getTime() {
		return time;
	}

	public boolean isClosed() {
		if (!closed) {
			return (System.currentTimeMillis() - getTime()) > 32000;  // 32 seconds
		}
		return true;
	}

	public void close() {
		//System.out.println("Close connection of " + getDate());
		if (!closed) {
			closed = true;
			try {
				writer.close();
			}
			catch (Exception e) {
				// ignore
			}
			try {
				reader.close();
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
				onClosedConnection();
			}
		}
	}
	
	private void onClosedConnection() {
		setChanged();
		notifyObservers(new ConnectionClosedEvent(this));
	}

	private void onRecievedMesasge() {
		setChanged();
		notifyObservers(new RecievedMessageEvent(this));
	}
	
	@Override
	public String toString() {
		return new StringBuilder().append("Key: ").append(key).append(", IP: ").append(ip).toString();
	}

	public String getInput() {
		return input;
	}

	public String getOutput() {
		return output;
	}

	public Date getDate() {
		return new Date(time);
	}
}
