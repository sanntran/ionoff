package net.ionoff.things.e4;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.Socket;
import java.net.UnknownHostException;

public class E4Client {

    public static void main(String[] args) throws UnknownHostException, IOException {
        for (;true;) {
            openConnection();
        }
    }

    static void openConnection() {
        try {
            Socket skt = new Socket("localhost", 8118);
            BufferedReader in = new BufferedReader(new InputStreamReader(skt.getInputStream()));
            DataOutputStream os = new DataOutputStream(skt.getOutputStream());

            os.writeBytes("ST:0000,1111,A3000000\n");
            System.out.println("Connect server: ST:0000,1111,A3000000");
            String cmd = in.readLine(); // Read one line and output it
            System.out.println("Server command: " + cmd);
            if (cmd.startsWith("{cf")) {
                os.writeBytes("CF:E4-17-00-AA,C0.A8.1.FC,IOnOff,123456789,2211\n");
            }
            if (cmd.startsWith("{io")) {
                os.writeBytes("IO:1100,0011\n");
            }
            in.close();
            os.close();
            skt.close();
        } catch (Exception e) {
           e.printStackTrace();
        }
    }
}
