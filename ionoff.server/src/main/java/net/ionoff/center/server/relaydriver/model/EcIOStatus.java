package net.ionoff.center.server.relaydriver.model;

import net.ionoff.center.server.relaydriver.exception.MessageFormatException;

import java.util.ArrayList;

public class EcIOStatus extends BaseStatus {

    // key=EC100ABCDEFGHIJK&status=$SS/0630,0590,0000,0000/1,1,1,1,z,z,z,z/0,0,0,0,z,z,z,z;$SS/0630,0590,0000,0000/1,1,1,1,1,1,1,1/0,0,0,0,0,1,0,1;$SS/0630,0590,0000,0000/1,1,1,1,1,0,0,1/0,0,0,0,0,0,0,0;$SS/0630,0590,0000,0000/1,1,1,1,1,1,1,1/0,0,0,0,1,1,1,1

    private String status;

    public EcIOStatus(String message) {
        super(message);
        String[] params = message.split("&");
        for (String param : params) {
            String[] pairs = param.split("=");
            if ("key".equals(pairs[0])) {
                key = pairs[1];
            }
            else if ("status".equals(pairs[0])) {
                status = pairs[1];
            }
        }
        if (key == null || status == null) {
            throw new MessageFormatException(message);
        }
        String rooms[] = status.split(";");
        if (rooms.length != 4) {
            throw new MessageFormatException(message);
        }
        inputs = new ArrayList<>();
        outputs = new ArrayList<>();
        for (int r = 0; r < rooms.length; r++) {
            String values[] = rooms[r].split("/");
            if (values.length != 4) {
                throw new MessageFormatException(message);
            }

            String inputArr[] = values[2].split(",");
            for (int i = 0; i < 4; i++) {
                inputs.add(toBoolean(inputArr[i]));
            }
            if (r != 0) {
                for (int i = 4; i < 8; i++) {
                    inputs.add(toBoolean(inputArr[i]));
                }
            }

            String outArr[] = values[3].split(",");
            for (int i = 0; i < 4; i++) {
                outputs.add(toBoolean(outArr[i]));
            }
            if (r != 0) {
                for (int i = 4; i < 8; i++) {
                    outputs.add(toBoolean(outArr[i]));
                }
            }
        }
    }

    @Override
    protected Boolean toBoolean(String status) {
        if ("0".equals(status)) {
            return false;
        }
        if ("1".equals(status)) {
            return true;
        }
        return false;
    }

    public static boolean accept(String msg) {
        return msg != null && msg.startsWith("key=EC");
    }
}
