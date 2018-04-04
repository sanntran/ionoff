package net.ionoff.things.p8;

import java.awt.Color;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

class TcpServer extends Thread {

	private static final DateFormat DATE_FORMATTER = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss"); 
	
	private P8Tool p8Tool;
	
	private ServerSocket serverSocket;
	private List<TcpConnection> connections;
	
	TcpServer() {
		connections = new ArrayList<>();
	}

	void setP8Tool(P8Tool p8Tool) {
		this.p8Tool = p8Tool;
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
		if (connections.size() > 9) {
			for (; connections.size() > 9; connections.remove(0)) {
				//
			}
		}
		connections.add(newConnection);
		
		String message = newConnection.readLine();
		Date date = new Date();
		String mes = "Update status at " + DATE_FORMATTER.format(date);
		System.out.println(mes);
		
		p8Tool.showMessage("Current connection: " + DATE_FORMATTER.format(date), Color.BLUE);
		if (!P8Tool.PUBLIC_MODE) {
			System.out.println("Message: " + message);
		}
		P8Status status = parseP8Status(message);
		if (message.startsWith("RS")) {
			p8Tool.resetConfig();
		}
		String pw = message.split(",")[2];
		p8Tool.getPwTextField().setText(pw);
		newConnection.setKey(pw);
		
		String ip = socket.getRemoteSocketAddress().toString().replace("/", "").split(":")[0];
		p8Tool.getIpTextField().setText(ip);
		newConnection.setIp(ip);
		
		p8Tool.setStatus(status);
	}
	
	private static P8Status parseP8Status(String response) {
		String contents[] = response.split(":");
		String states[] = contents[1].split(",");
		return P8Status.parseP8Status(states[0], states[1]); 
	}
	
	private static TcpServer instance;
	
	static TcpServer getInstance() {
		if (instance == null) {
			instance = new TcpServer();
		}
		return instance;
	}
	
	TcpConnection getTcpConnection(String key) {
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

	P8Status sendReqSetStatus(String pw, int relayIndex, int command) throws Exception {
		TcpConnection conn = getTcpConnection(pw);
		if (conn == null) {
			throw new Exception("No connection or connection expired");
		}
		return conn.sendReqSetStatus(relayIndex, command);
	}

	P8Config sendReqGetConf(String pw) throws Exception {
		TcpConnection conn = getTcpConnection(pw);
		if (conn == null) {
			throw new Exception("No connection or connection expired");
		}
		return conn.sendReqGetConf();
	}

	P8Config sendReqConfigIp(String newIp, String pw) throws Exception {
		TcpConnection conn = getTcpConnection(pw);
		if (conn == null) {
			throw new Exception("No connection or connection expired");
		}
		return conn.sendReqConfigIp(newIp);
	}

	P8Config sendReqConfigSubnetMask(String newSubnet, String pw) throws Exception {
		TcpConnection conn = getTcpConnection(pw);
		if (conn == null) {
			throw new Exception("No connection or connection expired");
		}
		return conn.sendReqConfigSubnetMask(newSubnet);
	}

	P8Config sendReqConfigGateway(String newGateway, String pw) throws Exception {
		TcpConnection conn = getTcpConnection(pw);
		if (conn == null) {
			throw new Exception("No connection or connection expired");
		}
		return conn.sendReqConfigGateway(newGateway);
	}

	P8Config sendReqConfigSrvIp(String newSrvIp, String pw) throws Exception {
		TcpConnection conn = getTcpConnection(pw);
		if (conn == null) {
			throw new Exception("No connection or connection expired");
		}
		return conn.sendReqConfigSrvIp(newSrvIp);
	}

	P8Config sendReqConfigMac(String newMac, String pw) throws Exception {
		TcpConnection conn = getTcpConnection(pw);
		if (conn == null) {
			throw new Exception("No connection or connection expired");
		}
		return conn.sendReqConfigMac(newMac);
	}

	P8Config sendReqConfigInputTypes(List<Integer> newInputTypes, String pw) throws Exception {
		TcpConnection conn = getTcpConnection(pw);
		if (conn == null) {
			throw new Exception("No connection or connection expired");
		}
		return conn.sendReqConfigInputTypes(newInputTypes);
	}
}
