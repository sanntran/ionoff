package net.ionoff.center.server.relaydriver;

import net.ionoff.center.server.relaydriver.exception.DataFormatException;

import java.util.ArrayList;
import java.util.List;

public class BaseIOStatus {

    protected final String message;
    protected String code;
    protected String key;
    protected List<Boolean> inputs;
    protected List<Boolean> outputs;

    public BaseIOStatus(String message) {
        this.message = message;
    }

    protected Boolean toBoolean(String status) {
        if ("0".equals(status)) {
            return false;
        }
        if ("1".equals(status)) {
            return true;
        }
        throw new DataFormatException(message);
    }

    public String getCode() {
        return code;
    }

    public String getKey() {
        return key;
    }

    public void setKey(String key) {
        this.key = key;
    }

    public List<Boolean> getInputs() {
        return inputs;
    }

    public List<Boolean> getOutputs() {
        return outputs;
    }

    public boolean isChanged() {
        return false;
    }

    public static boolean accept(String msg) {
        return msg != null && msg.startsWith("key=EC");
    }
}
