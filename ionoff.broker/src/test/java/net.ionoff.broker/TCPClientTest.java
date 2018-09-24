package net.ionoff.broker;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.net.UnknownHostException;

public class TCPClientTest {

    public static void main(String args[]) throws IOException {
        for (; true; ) {
            try {
                send();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    public static void send() throws IOException {

        System.out.println("Create socket to localhost:8181");

        Socket socket = new Socket("localhost", 8118);
        socket.setSoTimeout(4*1000); // 4 seconds
        PrintWriter writer = new PrintWriter(socket.getOutputStream(), true);
        BufferedReader reader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        // -----Step 3

        writer.println("ST:11111111,11111111,A30000C5");
        socket.setSoTimeout(32000);
        final String received = reader.readLine();
        System.out.println("Response from broker: " + received);

        writer.println("ST:11111111,00000000,A30000C5");

        if (writer != null) {
            writer.close();
        }
        if (reader != null) {
            reader.close();
        }
        if (socket != null) {
            socket.close();
        }
    }

}
