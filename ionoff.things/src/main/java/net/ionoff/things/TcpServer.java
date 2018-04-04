package net.ionoff.things;

import java.awt.Color;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import net.ionoff.things.e4.E4Config;
import net.ionoff.things.e4.E4Status;
import net.ionoff.things.e4.TcpConnection;

public class TcpServer extends Thread {

	private static final DateFormat DATE_FORMATTER = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss"); 
	
	private ConfigTool e4Tool;
	
	private ServerSocket serverSocket;
	private List<TcpConnection> connections;
	
	public TcpServer() {
		connections = new ArrayList<>();
	}

	public void setP8Tool(ConfigTool e4Tool) {
		this.e4Tool = e4Tool;
	}

	@Override
	public void run() {
		try {
			serverSocket = new ServerSocket(8118);
			for (; true;) {
				try {
					if (serverSocket.isClosed()) {
						break;
					}
					final Socket socket = serverSocket.accept();
					handleConnection(socket);
				} catch (final Throwable t) {
					t.printStackTrace();
				}
			}
		} catch (final IOException e) {
			e.printStackTrace();
		}
	}

	private void handleConnection(Socket socket) throws Exception {
		
		TcpConnection newConnection = new TcpConnection(socket);
		if (connections.size() > 3) {
			for (; connections.size() > 3; connections.remove(0)) {
				//
			}
		}
		connections.add(newConnection);
		
		String message = newConnection.readLine();
		Date date = new Date();
		String mes = "Update status at " + DATE_FORMATTER.format(date);
		System.out.println(mes);
		
		e4Tool.showMessage("Current connection: " + DATE_FORMATTER.format(date), Color.BLUE);
		if (!ConfigTool.PUBLIC_MODE) {
			System.out.println("Message: " + message);
		}
		E4Status status = parseW4Status(message);
		if (message.startsWith("RS")) {
			e4Tool.resetConfig();
		}
		String key = message.split(",")[2];
		e4Tool.getKeyTextField().setText(key);
		newConnection.setKey(key);
		
		String ip = socket.getRemoteSocketAddress().toString().replace("/", "").split(":")[0];
		newConnection.setIp(ip);
		
		e4Tool.setStatus(status);
	}
	
	private static E4Status parseW4Status(String response) {
		String contents[] = response.split(":");
		String states[] = contents[1].split(",");
		return E4Status.fromStrings(states[0], states[1]); 
	}
	
	private static TcpServer instance;
	
	public static TcpServer getInstance() {
		if (instance == null) {
			instance = new TcpServer();
		}
		return instance;
	}
	
	public TcpConnection getTcpConnection(String key) {
		for (int i = 0; i < 4; i++) {
			TcpConnection connection = null;
			if (!connections.isEmpty() && connections.get(connections.size() - 1).isAlive()) {
				connection = connections.get(connections.size() - 1);
			}
			if (connection == null) {
				try {
					Thread.sleep(1000);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			}
			else {
				return connection;
			}
		}
		return null;
	}

	public E4Status sendReqSetStatus(String key, int relayIndex, int command) throws Exception {
		TcpConnection conn = getTcpConnection(key);
		if (conn == null) {
			throw new Exception("No connection or connection expired");
		}
		return conn.sendReqSetStatus(relayIndex, command);
	}

	public E4Config sendReqGetConf(String key) throws Exception {
		TcpConnection conn = getTcpConnection(key);
		if (conn == null) {
			throw new Exception("No connection or connection expired");
		}
		return conn.sendReqGetConf();
	}

	public E4Config sendReqConfigSrvIp(String key, String newSrvIp) throws Exception {
		TcpConnection conn = getTcpConnection(key);
		if (conn == null) {
			throw new Exception("No connection or connection expired");
		}
		return conn.sendReqConfigSrvIp(newSrvIp);
	}

	public E4Config sendReqConfigMac(String key, String newMac) throws Exception {
		TcpConnection conn = getTcpConnection(key);
		if (conn == null) {
			throw new Exception("No connection or connection expired");
		}
		return conn.sendReqConfigMac(newMac);
	}

	public E4Config sendReqConfigInputTypes(String key, List<Integer> newInputTypes) throws Exception {
		TcpConnection conn = getTcpConnection(key);
		if (conn == null) {
			throw new Exception("No connection or connection expired");
		}
		return conn.sendReqConfigInputTypes(newInputTypes);
	}

    public E4Config sendReqConfigWifiId(String key, String newWifiId) throws Exception {
        TcpConnection conn = getTcpConnection(key);
        if (conn == null) {
            throw new Exception("No connection or connection expired");
        }
        return conn.sendReqConfigWifiId(newWifiId);
    }

    public E4Config sendReqConfigWifiPw(String key, String newWifiPw) throws Exception {
        TcpConnection conn = getTcpConnection(key);
        if (conn == null) {
            throw new Exception("No connection or connection expired");
        }
        return conn.sendReqConfigWifiPw(newWifiPw);
    }
}
