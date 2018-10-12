package net.ionoff.center.server.mediaplayer.connector;

import java.util.List;
import java.util.Map;

import net.ionoff.center.server.mediaplayer.model.MediaPlayer;
import net.ionoff.center.shared.dto.player.MediaFile;
import net.ionoff.center.shared.dto.player.PlayListDto;
import net.ionoff.center.shared.dto.player.ScheduleDto;
import net.ionoff.center.shared.dto.player.StatusDto;

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
