package net.ionoff.things.e4;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.List;

import net.ionoff.things.ConfigTool;
import net.ionoff.things.TcpRequest;

public class TcpConnection {
	
	private Socket socket;
	private PrintWriter writer;
	private BufferedReader reader;
	private boolean isReady;
	private long time;
	private String key; 
	private String ip;
	
	public TcpConnection(Socket socket) throws IOException {
		setSocket(socket);
		time = System.currentTimeMillis();
	}

	public String readLine() throws IOException {
		return reader.readLine();		
	}

	public synchronized String sendCommand(String req) throws IOException {
		writer.println(req);		
		String resp = readLine();
		return resp;
	}

	public void setSocket(Socket socket) throws IOException {
		this.socket = socket;
		writer = new PrintWriter(socket.getOutputStream(), true);
		reader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
		setReady(true);
	}

	boolean isReady() {
		return isReady;
	}

	public void setReady(boolean isReady) {
		this.isReady = isReady;
	}
	
	private String sendTcpRequest(String req) throws Exception {
		try {
			if (!ConfigTool.PUBLIC_MODE) {
				System.out.println("Send: " + req);
			}
			String message = sendCommand(req);
			if (!ConfigTool.PUBLIC_MODE) {
				System.out.println("Received: " + message);		
			}
			return message;
		}
		catch (Exception e) {
			throw e;
		}
	}
	
	public E4Config sendReqConfigInputTypes(List<Integer> newInputTypes) throws Exception {
		String inputTypes = "";
		for (Integer i : newInputTypes) {
			inputTypes = inputTypes + i;
		}
		
		String reqStr = new TcpRequest().setCfObj().setSetMethod()
				.setTnputTypeAtt().setAttValue(inputTypes).build();
		String response = sendTcpRequest(reqStr);
		return parseConfig(response);
	}
	
	public E4Config sendReqConfigSrvIp(String newSrvIp) throws Exception {
		String reqStr = new TcpRequest().setCfObj().setSetMethod()
				.setServerIpAtt().setAttValue(newSrvIp).build();
		String response = sendTcpRequest(reqStr);
		return parseConfig(response);
	}
	
	 public E4Config sendReqConfigWifiId(String newWifiId) throws Exception {
        String reqStr = new TcpRequest().setCfObj().setSetMethod()
                .setWifiIdAtt().setAttValue(newWifiId).build();
        String response = sendTcpRequest(reqStr);
        return parseConfig(response);
    }

    public E4Config sendReqConfigWifiPw(String newWifiPw) throws Exception {
        String reqStr = new TcpRequest().setCfObj().setSetMethod()
                .setWifiPwAtt().setAttValue(newWifiPw).build();
        String response = sendTcpRequest(reqStr);
        return parseConfig(response);
    }
	
    public E4Config sendReqConfigMac(String newMac) throws Exception {
		String reqStr = new TcpRequest().setCfObj().setSetMethod()
				.setMacAtt().setAttValue(newMac).build();
		String response = sendTcpRequest(reqStr);
		return parseConfig(response);
	}
	
    public E4Config sendReqGetConf() throws Exception {
		String reqStr = new TcpRequest().setCfObj().setGetMethod().build();
		String response = sendTcpRequest(reqStr);
		return parseConfig(response);
	}
	
	// CF:A3-00-00-00,C0.A8.01.FC,1111,PHUCLOC,meyeucon
	private E4Config parseConfig(String configStr) {
		if (configStr == null) {
			throw new IllegalArgumentException(configStr);
		}
		String contents[] = configStr.split(":");
		String pieces[] = contents[1].split(",");
		if (pieces.length != 5) {
			throw new IllegalArgumentException(configStr);
		}
		E4Config conf = new E4Config();
		
		conf.setSn(pieces[0]);
		conf.setBroker(pieces[1]);
		conf.setInputTypes(pieces[2]);
		conf.setWifiId(pieces[3]);
		conf.setWifiPass(pieces[4]);		
		return conf;
	}
	
	public E4Status sendReqGetStatus() throws Exception {
		String reqStr = new TcpRequest().setIOObj().setGetMethod().build();
		String response = sendTcpRequest(reqStr);
		return parseW4Status(response);
	}

	private E4Status parseW4Status(String response) {
		String contents[] = response.split(":");
		String states[] = contents[1].split(",");
		return E4Status.fromStrings(states[0], states[1]); 
	}

	public E4Status sendReqSetStatus(int relayIndex, int command) throws Exception {
		String reqStr = new TcpRequest().setIOObj()
				.setSetMethod().seOutAtt(relayIndex).setAttValue(command + "").build();
		String response = sendTcpRequest(reqStr);
		return parseW4Status(response);
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
