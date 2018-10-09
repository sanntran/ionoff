package net.ionoff.broker.http.handler;

import java.util.Arrays;
import java.util.List;

public class Command {

    private String protocol;
    private String address;
    private String method;
    /**
     * delay after send to address, in milli-second
     */
    private Integer delay;
    private String subscription;
    private String keyword;
    private Object content;

    public String getProtocol() {
        return protocol;
    }

    public void setProtocol(String protocol) {
        this.protocol = protocol;
    }

    public String getAddress() {
        return address;
    }

    public void setAddress(String address) {
        this.address = address;
    }

    public String getMethod() {
        return method;
    }

    public void setMethod(String method) {
        this.method = method;
    }

    public String getSubscription() {
        return subscription;
    }

    public void setSubscription(String subscription) {
        this.subscription = subscription;
    }

    public Object getContent() {
        return content;
    }

    public void setContent(Object content) {
        this.content = content;
    }

    public String getKeyword() {
        return keyword;
    }

    public void setKeyword(String keyword) {
        this.keyword = keyword;
    }

    public Integer getDelay() {
        return delay;
    }

    public void setDelay(Integer delay) {
        this.delay = delay;
    }

    public List<String> listAddress() {
        if (address != null && address.startsWith("[") && address.endsWith("]")) {
            return Arrays.asList(address.substring(1, address.length() - 1).split(", "));
        }
        return Arrays.asList(address);
    }

}
