package net.xapxinh.center.server.api.player;

import java.util.List;
import java.util.Map;

import net.xapxinh.center.server.entity.Player;
import net.xapxinh.center.server.exception.PlayerConnectException;
import net.xapxinh.center.shared.dto.MediaFile;
import net.xapxinh.center.shared.dto.PlayListDto;
import net.xapxinh.center.shared.dto.Schedule;
import net.xapxinh.center.shared.dto.Status;

public interface IPlayerApi {
	
	static final String CONTEXT_STATUS = "/requests/status";
	static final String CONTEXT_PLAYLIST = "/requests/playlist";
	static final String CONTEXT_BROWSE = "/requests/browse";
	static final String CONTEXT_SCHEDULE = "/requests/schedule";
	
	Status requestStatus(Player player, Map<String, Object> params) throws PlayerConnectException;

	PlayListDto requestPlaylist(Player player) throws PlayerConnectException;

	List<MediaFile> requestBrowse(Player player, Map<String, Object> params) throws PlayerConnectException;

	Schedule requestSchedule(Player player, Map<String, Object> params) throws PlayerConnectException;

	boolean hasPlayerConnection(Player player);

	PlayListDto updatePlaylist(Player player, Map<String, Object> params) throws PlayerConnectException;
}
