package net.ionoff.center.server.relaydriver;

import net.ionoff.center.server.relaydriver.exception.DataFormatException;

import java.util.ArrayList;
import java.util.List;

public class EpIOStatus extends BaseIOStatus{

    // key=EP2ABCDEFGHIJK&status=1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 1 1

    private String status;

    public EpIOStatus(String message) {
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
            throw new DataFormatException(message);
        }
        String outs[] = status.split(" ");
        outputs = new ArrayList<>(outs.length);
        for (int i = 0; i < outs.length; i++) {
            outputs.add(toBoolean(outs[i]));
        }
    }

    public static boolean accept(String msg) {
        return msg != null && msg.startsWith("key=EP");
    }
}
