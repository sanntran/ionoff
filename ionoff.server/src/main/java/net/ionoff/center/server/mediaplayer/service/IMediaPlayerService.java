package net.ionoff.center.server.mediaplayer.service;

import java.util.List;
import java.util.Map;

import net.ionoff.center.server.mediaplayer.model.MediaPlayer;
import net.xapxinh.center.shared.dto.MediaFile;
import net.xapxinh.center.shared.dto.PlayListDto;
import net.xapxinh.center.shared.dto.ScheduleDto;
import net.xapxinh.center.shared.dto.StatusDto;

public interface IMediaPlayerService {

	StatusDto requesStatus(MediaPlayer player, Map<String, Object> params);

	PlayListDto requestPlaylist(MediaPlayer player);

	List<MediaFile> requestMediaFiles(MediaPlayer player, Map<String, Object> params);

	ScheduleDto requestSchedule(MediaPlayer player, Map<String, Object> params);

	PlayListDto updatePlaylist(MediaPlayer player, PlayListDto playlist);
}
