package net.ionoff.center.server.mediaplayer.connector;

import java.util.List;
import java.util.Map;

import net.ionoff.center.server.mediaplayer.model.MediaPlayer;
import net.ionoff.center.server.mediaplayer.exception.MediaPlayerConnectException;
import net.xapxinh.center.shared.dto.MediaFile;
import net.xapxinh.center.shared.dto.PlayListDto;
import net.xapxinh.center.shared.dto.ScheduleDto;
import net.xapxinh.center.shared.dto.StatusDto;

public interface IMediaPlayerConnector {
	
	String CONTEXT_STATUS = "/requests/status";
	String CONTEXT_PLAYLIST = "/requests/playlist";
	String CONTEXT_BROWSE = "/requests/browse";
	String CONTEXT_SCHEDULE = "/requests/scheduler";
	
	StatusDto requestStatus(MediaPlayer player, Map<String, Object> params);

	PlayListDto requestPlaylist(MediaPlayer player);

	List<MediaFile> requestBrowse(MediaPlayer player, Map<String, Object> params);

	ScheduleDto requestSchedule(MediaPlayer player, Map<String, Object> params);

	PlayListDto updatePlaylist(MediaPlayer player, Map<String, Object> params);
}
