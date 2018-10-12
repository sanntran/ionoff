package net.ionoff.center.server.mediaplayer.model;

import net.ionoff.center.shared.dto.player.StatusDto;

public class MediaPlayerMessage {

    private String mac;
    private StatusDto status;

    public String getMac() {
        return mac;
    }

    public void setMac(String mac) {
        this.mac = mac;
    }

    public StatusDto getStatus() {
        return status;
    }

    public void setStatus(StatusDto status) {
        this.status = status;
    }

}
