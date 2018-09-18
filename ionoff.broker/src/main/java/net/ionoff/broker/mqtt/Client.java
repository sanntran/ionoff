package net.ionoff.broker.mqtt;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.net.UnknownHostException;

public class Client {


    public static void main(String args[]) throws IOException {
        send();
    }

    public static void send() throws IOException {
        Socket socket = new Socket("localhost", 8118);
        socket.setSoTimeout(4*1000); // 4 seconds
        PrintWriter writer = new PrintWriter(socket.getOutputStream(), true);
        BufferedReader reader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        // -----Step 3

        writer.println("heheheheh");

        final String received = reader.readLine();
        System.out.println("Response from center: " + received);

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
