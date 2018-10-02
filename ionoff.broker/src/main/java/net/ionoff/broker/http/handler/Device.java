package net.ionoff.broker.http.handler;

import java.util.List;

public class Device {

    private String uid;
    private List<String> urls;

    public String getUid() {
        return uid;
    }

    public void setUid(String uid) {
        this.uid = uid;
    }

    public List<String> getUrls() {
        return urls;
    }

    public void setUrls(List<String> urls) {
        this.urls = urls;
    }
}
