package net.ionoff.center.server.relaydriver.api;

import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.SocketAddress;

public class RelayDriverApiUtil {
	private static final int TIME_OUT = 2000; // milliseconds

	public static boolean ping(String host, Integer port) {
		Socket socket = null;
		SocketAddress endpoint = null;
		InetAddress address = null;
		try {
			address = InetAddress.getByName(host);
			endpoint = new InetSocketAddress(address, port);
			socket = new Socket();
			socket.connect(endpoint, TIME_OUT);
			return true;
		}
		catch (Throwable t) {
			return false;
		}
		finally {
			if (socket != null) {
				try {
					socket.close();
				}
				catch (Throwable t) {
					// Does not care
				}
			}
		}
	}
}
