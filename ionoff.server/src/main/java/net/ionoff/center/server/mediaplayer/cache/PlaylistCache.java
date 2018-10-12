package net.ionoff.center.server.mediaplayer.cache;

import net.ionoff.center.shared.dto.player.PlayListDto;

public class PlaylistCache extends PlayerCache{
	
	private PlayListDto playlist;

	public PlayListDto getPlaylist() {
		return playlist;
	}
	
	public void setPlaylist(PlayListDto playlist) {
		this.playlist = playlist;
	}

	@Override
	protected long getLivingTime() {
		return 1000;
	}
}
