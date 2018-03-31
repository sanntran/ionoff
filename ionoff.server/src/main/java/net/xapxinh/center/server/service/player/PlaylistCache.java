package net.xapxinh.center.server.service.player;

import net.xapxinh.center.shared.dto.PlayListDto;

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
