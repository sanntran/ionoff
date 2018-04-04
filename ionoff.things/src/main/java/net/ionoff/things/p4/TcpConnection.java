package net.ionoff.things.p4;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.List;

class TcpConnection {
	
	private Socket socket;
	private PrintWriter writer;
	private BufferedReader reader;
	private boolean isReady;
	private long time;
	private String key; 
	private String ip;
	
	TcpConnection(Socket socket) throws IOException {
		setSocket(socket);
		time = System.currentTimeMillis();
	}

	String readLine() throws IOException {
		return reader.readLine();		
	}

	synchronized String sendCommand(String req) throws IOException {
		writer.println(req);		
		String resp = readLine();
		writer.close();
		reader.close();
		socket.close();
		setReady(false);
		return resp;
	}

	void setSocket(Socket socket) throws IOException {
		this.socket = socket;
		writer = new PrintWriter(socket.getOutputStream(), true);
		reader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
		setReady(true);
	}

	boolean isReady() {
		return isReady;
	}

	void setReady(boolean isReady) {
		this.isReady = isReady;
	}
	
	private String sendTcpRequest(String req) throws Exception {
		try {
			if (!P4Tool.PUBLIC_MODE) {
				System.out.println("Send: " + req);
			}
			String message = sendCommand(req);
			if (!P4Tool.PUBLIC_MODE) {
				System.out.println("Received: " + message);		
			}
			return message;
		}
		catch (Exception e) {
			throw e;
		}
	}
	
	P4Config sendReqConfigInputTypes(List<Integer> newInputTypes) throws Exception {
		String inputTypes = "";
		for (Integer i : newInputTypes) {
			inputTypes = inputTypes + i;
		}
		
		String reqStr = new TcpRequest().setCfObj().setSetMethod()
				.setTnputTypeAtt().setAttValue(inputTypes).build();
		String response = sendTcpRequest(reqStr);
		return parseConfig(response);
	}
	
	P4Config sendReqConfigSrvIp(String newSrvIp) throws Exception {
		String reqStr = new TcpRequest().setCfObj().setSetMethod()
				.setServerIpAtt().setAttValue(newSrvIp).build();
		String response = sendTcpRequest(reqStr);
		return parseConfig(response);
	}
	
	P4Config sendReqConfigIp(String newIp) throws Exception {
		String reqStr = new TcpRequest().setCfObj().setSetMethod()
				.setIpAtt().setAttValue(newIp).build();
		String response = sendTcpRequest(reqStr);
		return parseConfig(response);
	}
	
	P4Config sendReqConfigSubnetMask(String newSubnetMask) throws Exception {
		String reqStr = new TcpRequest().setCfObj().setSetMethod()
				.setSubnetMaskAtt().setAttValue(newSubnetMask).build();
		String response = sendTcpRequest(reqStr);
		return parseConfig(response);
	}
	
	P4Config sendReqConfigGateway(String newGateway) throws Exception {
		String reqStr = new TcpRequest().setCfObj().setSetMethod()
				.setGatewayAtt().setAttValue(newGateway).build();
		String response = sendTcpRequest(reqStr);
		return parseConfig(response);
	}
	
	P4Config sendReqConfigMac(String newMac) throws Exception {
		String reqStr = new TcpRequest().setCfObj().setSetMethod()
				.setMacAtt().setAttValue(newMac).build();
		String response = sendTcpRequest(reqStr);
		return parseConfig(response);
	}
	
	P4Config sendReqGetConf() throws Exception {
		String reqStr = new TcpRequest().setCfObj().setGetMethod().build();
		String response = sendTcpRequest(reqStr);
		return parseConfig(response);
	}
	
	// 200:01,192.168.1.253,192.168.1.129,255.255.255.0,192.168.1.1,00-04-A3-00-00-01
	private P4Config parseConfig(String configStr) {
		if (configStr == null) {
			throw new IllegalArgumentException(configStr);
		}
		String contents[] = configStr.split(":");
		String pieces[] = contents[1].split(",");
		if (pieces.length != 6) {
			throw new IllegalArgumentException(configStr);
		}
		P4Config conf = new P4Config();
		
		conf.setSrvIp(pieces[0]);
		conf.setIp(pieces[1]);
		conf.setSubnetMask(pieces[2]);
		conf.setGateway(pieces[3]);
		conf.setMac(pieces[4]);
		conf.setInputTypes(pieces[5]);
		return conf;
	}
	
	P8Status sendReqGetStatus() throws Exception {
		String reqStr = new TcpRequest().setIOObj().setGetMethod().build();
		String response = sendTcpRequest(reqStr);
		return parseP8Status(response);
	}

	private P8Status parseP8Status(String response) {
		String contents[] = response.split(":");
		String states[] = contents[1].split(",");
		return P8Status.parseP8Status(states[0], states[1]); 
	}

	P8Status sendReqSetStatus(int relayIndex, int command) throws Exception {
		String reqStr = new TcpRequest().setIOObj()
				.setSetMethod().seOutAtt(relayIndex).setAttValue(command + "").build();
		String response = sendTcpRequest(reqStr);
		return parseP8Status(response);
	}

	public String getKey() {
		return key;
	}

	public void setKey(String key) {
		this.key = key;
	}

	public String getIp() {
		return ip;
	}

	public void setIp(String ip) {
		this.ip = ip;
	}

	public long getTime() {
		return time;
	}

	public void setTime(long time) {
		this.time = time;
	}

	public boolean isAlive() {
		return (System.currentTimeMillis() - getTime()) < 30000; // 30 seconds
	}
}
