package net.ionoff.center.server.relaydriver.model;

import net.ionoff.center.server.relaydriver.exception.DataFormatException;

import java.util.ArrayList;

public class PxIOStatus extends BaseStatus {

    // ST:11111111,00000000,A30000AA
    // CH:11111111,00000000,A30000AA
    // RS:11111111,00000000,A30000AA

    private String input;
    private String output;

    public PxIOStatus(String message) {
        super(message);
        String messageItems[] = message.split(":");
        code = messageItems[0];
        String dataItems[] = messageItems[1].split(",");
        if (dataItems.length == 3) {
            input = dataItems[0];
            output = dataItems[1];
            key = dataItems[2];
        }
        if (key == null || input == null || output == null) {
            throw new DataFormatException(message);
        }
        inputs = new ArrayList<>(input.length());
        outputs = new ArrayList<>(output.length());
        for (int i = 0; i < input.length(); i++) {
            inputs.add(toBoolean(input.charAt(i)));
        }
        for (int i = 0; i < output.length(); i++) {
            outputs.add(toBoolean(output.charAt(i)));
        }
    }

    private boolean toBoolean(char status) {
        if ('0' == status) {
            return false;
        }
        if ('1' == status) {
            return true;
        }
        throw new DataFormatException(message);
    }

    @Override
    public boolean isChanged() {
        return "CH".equals(code);
    }

    @Override
    public boolean isStarted() {
        return "RS".equals(code);
    }

    public String getInput() {
        return input;
    }

    public String getOutput() {
        return output;
    }

    public static boolean accept(String msg) {
        return msg != null && msg.indexOf(":") == 2;
    }
}
