package net.ionoff.center.server.relaydriver.model;

import java.util.ArrayList;

import net.ionoff.center.server.relaydriver.exception.MessageFormatException;

public class ExIOStatus extends BaseStatus {

    // id=E313180909E610BD&code=status&io=101,110
    // id=E313180909E610BD&code=crash&io=101,110
    // id=E313180909E610BD&code=reset&io=101,110
    // id=E313180909E610BD&code=hello&io=101,110
    // id=E313180909E610BD&code=changed&io=101,110

    private String code;
    private String input;
    private String output;

    public ExIOStatus(String message) {
        super(message);
        String[] params = message.split("&");
        for (String param : params) {
            String[] pairs = param.split("=");
            if ("id".equals(pairs[0])) {
                key = pairs[1];
            }
            else if ("code".equals(pairs[0])) {
                code = pairs[1];
            }
            else if ("io".equals(pairs[0])) {
                String io[] = pairs[1].split(",");
                input = io[0];
                output = io[1];
            }
        }
        if (key == null || code == null || input == null || output == null) {
            throw new MessageFormatException(message);
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
        if ('1' == status) {
            return false;
        }
        if ('0' == status) {
            return true;
        }
        throw new MessageFormatException(message);
    }

    @Override
    public boolean isChanged() {
        return "changed".equals(code);
    }

    @Override
    public boolean isStarted() {
        return "reset".equals(code);
    }

    @Override
    public boolean isCrashed() {
        return "crash".equals(code);
    }

    public static boolean accept(String msg) {
        if (msg == null || msg.isEmpty()) {
            return false;
        }
        String params[] = msg.split("&");
        String values[] = msg.split("=");
        return msg.startsWith("id=E") & params.length > 0 && params.length == values.length - 1;
    }
}
