package net.ionoff.center.shared.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;

import net.ionoff.center.shared.entity.PlayerAction;


@JsonTypeName("ScenePlayerActionDto") 
public class ScenePlayerActionDto extends SceneActionDto implements PlayerAction  {

	private static final long serialVersionUID = 1L;
	
	private String volume;
	private String album;
	private String albumType;
	private Long playerId;
	private String playerName;
	
	public String getClazz() {
		return "ScenePlayerActionDto";
	}
	
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
	public Long getPlayerId() {
		return playerId;
	}
	public void setPlayerId(Long playerId) {
		this.playerId = playerId;
	}
	
	public String getPlayerName() {
		return playerName;
	}
	public void setPlayerName(String playerName) {
		this.playerName = playerName;
	}
}
