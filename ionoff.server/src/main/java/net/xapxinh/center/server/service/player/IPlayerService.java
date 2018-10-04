package net.xapxinh.center.server.service.player;

import java.util.List;
import java.util.Map;

import net.xapxinh.center.server.entity.Player;
import net.xapxinh.center.shared.dto.MediaFile;
import net.xapxinh.center.shared.dto.PlayListDto;
import net.xapxinh.center.shared.dto.ScheduleDto;
import net.xapxinh.center.shared.dto.StatusDto;

public interface IPlayerService {

	StatusDto requesStatus(Player player, Map<String, Object> params);

	PlayListDto requestPlaylist(Player player);

	List<MediaFile> requestMediaFiles(Player player, Map<String, Object> params);

	boolean hasStatusCache(Player player);

	boolean hasConnection(Player player);

	ScheduleDto requestSchedule(Player player, Map<String, Object> params);

	PlayListDto updatePlaylist(Player player, PlayListDto playlist);
}
