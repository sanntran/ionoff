package net.ionoff.player.storage;

import net.ionoff.player.model.Schedule;

import java.io.Serializable;

public class Data implements Serializable {

    // random password
    private String password;
    private Integer volume;
    private Schedule schedule;

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public Integer getVolume() {
        return volume;
    }

    public void setVolume(Integer volume) {
        this.volume = volume;
    }

    public Schedule getSchedule() {
        return schedule;
    }

    public void setSchedule(Schedule schedule) {
        this.schedule = schedule;
    }
}
