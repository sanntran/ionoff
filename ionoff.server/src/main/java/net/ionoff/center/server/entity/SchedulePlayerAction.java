package net.ionoff.center.server.entity;

import net.ionoff.center.shared.entity.PlayerAction;

public class SchedulePlayerAction extends ScheduleAction implements PlayerAction {

	private static final long serialVersionUID = 1L;
	
	public MediaPlayer player;
	private String volume;
	private String album;
	private String albumType;
	
	public String getVolume() {
		return volume;
	}
	public void setVolume(String volume) {
		this.volume = volume;
	}
	
	public String getAlbum() {
		return album;
	}
	public void setAlbum(String album) {
		this.album = album;
	}
	
	public String getAlbumType() {
		return albumType;
	}
	public void setAlbumType(String albumType) {
		this.albumType = albumType;
	}
	
	public MediaPlayer getPlayer() {
		return player;
	}
	public void setPlayer(MediaPlayer player) {
		this.player = player;
	}
}
