package net.xapxinh.center.server.service.player;

import java.util.HashMap;
import java.util.Map;

import net.xapxinh.center.shared.dto.PlayListDto;
import net.xapxinh.center.shared.dto.Status;

public class PlayerCaches {

	private final Map<Long, StatusCache> statusCaches;
	private final Map<Long, PlaylistCache> playlistCaches;

	public PlayerCaches() {
		statusCaches = new HashMap<Long, StatusCache>();
		playlistCaches = new HashMap<Long, PlaylistCache>();
	}

	public Map<Long, StatusCache> getStatusCaches() {
		return statusCaches;
	}

	public Map<Long, PlaylistCache> getPlaylistCaches() {
		return playlistCaches;
	}

	public Status getStatus(long playerId) {
		if (statusCaches.containsKey(playerId)) {
			final StatusCache cache = statusCaches.get(playerId);
			if (cache.isLiving()) {
				return cache.getStatus();
			}
		}
		return null;
	}

	public boolean hasStatus(long playerId) {
		final StatusCache cache = statusCaches.get(playerId);
		return cache != null && cache.getStatus() != null;
	}

	public PlayListDto getPlaylist(long playerId) {
		if (playlistCaches.containsKey(playerId)) {
			final PlaylistCache cache = playlistCaches.get(playerId);
			if (cache.isLiving()) {
				return cache.getPlaylist();
			}
		}
		return null;
	}

	public void storeStatus(long playerId, Status status) {
		final long time = System.currentTimeMillis();
		final StatusCache cache = getStatusCache(playerId);
		cache.setTime(time);
		cache.setStatus(status);
		statusCaches.put(playerId, cache);
	}

	public void storePlaylist(long playerId, PlayListDto playlist) {
		final long time = System.currentTimeMillis();
		final PlaylistCache cache = getPlaylistCache(playerId);
		cache.setTime(time);
		cache.setPlaylist(playlist);
		playlistCaches.put(playerId, cache);
	}

	private StatusCache getStatusCache(long playerId) {
		if (statusCaches.containsKey(playerId)) {
			return statusCaches.get(playerId);
		}
		return new StatusCache();
	}
	private PlaylistCache getPlaylistCache(long playerId) {
		if (playlistCaches.containsKey(playerId)) {
			return playlistCaches.get(playerId);
		}
		return new PlaylistCache();
	}
}
