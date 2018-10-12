package net.ionoff.center.server.mediaplayer.service;

import java.util.List;
import java.util.Map;

import net.ionoff.center.server.mediaplayer.model.MediaPlayer;
import net.ionoff.center.shared.dto.player.MediaFile;
import net.ionoff.center.shared.dto.player.PlayListDto;
import net.ionoff.center.shared.dto.player.ScheduleDto;
import net.ionoff.center.shared.dto.player.StatusDto;

public interface IMediaPlayerService {

	StatusDto requesStatus(MediaPlayer player, Map<String, Object> params);

	PlayListDto requestPlaylist(MediaPlayer player);

	List<MediaFile> requestMediaFiles(MediaPlayer player, Map<String, Object> params);

	ScheduleDto requestSchedule(MediaPlayer player, Map<String, Object> params);

	PlayListDto updatePlaylist(MediaPlayer player, PlayListDto playlist);
}
