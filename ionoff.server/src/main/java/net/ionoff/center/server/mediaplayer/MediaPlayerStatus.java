package net.ionoff.center.server.mediaplayer;

import net.xapxinh.center.shared.dto.PlayListDto;
import net.xapxinh.center.shared.dto.StatusDto;

public class MediaPlayerStatus {

    private String mac;
    private StatusDto status;
    private PlayListDto playlist;

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

    public PlayListDto getPlaylist() {
        return playlist;
    }

    public void setPlaylist(PlayListDto playlist) {
        this.playlist = playlist;
    }
}
