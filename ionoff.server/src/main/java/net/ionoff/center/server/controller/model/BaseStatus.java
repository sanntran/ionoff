package net.ionoff.center.server.controller.model;

import java.util.List;

import net.ionoff.center.server.controller.exception.MessageFormatException;

public class BaseStatus {

    protected final String message;

    /**
     * Status code of relay driver
     */
    protected String code;

    /**
     * Key to find relay driver (mac of relay dirver)
     */
    protected String key;

    protected List<Boolean> inputs;
    protected List<Boolean> outputs;

    public BaseStatus(String message) {
        this.message = message;
    }

    protected Boolean toBoolean(String status) {
        if ("0".equals(status)) {
            return false;
        }
        if ("1".equals(status)) {
            return true;
        }
        throw new MessageFormatException(message);
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

    public boolean isStarted() {
        return false;
    }

    public boolean isCrashed() {
        return false;
    }

    public static boolean accept(String msg) {
        return msg != null && msg.startsWith("key=EC");
    }
}
