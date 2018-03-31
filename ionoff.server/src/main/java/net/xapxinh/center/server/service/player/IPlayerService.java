package net.xapxinh.center.server.service.player;

import java.util.List;
import java.util.Map;

import net.xapxinh.center.server.entity.Player;
import net.xapxinh.center.shared.dto.MediaFile;
import net.xapxinh.center.shared.dto.PlayListDto;
import net.xapxinh.center.shared.dto.Schedule;
import net.xapxinh.center.shared.dto.Status;

public interface IPlayerService {

	Status requesStatus(Player player, Map<String, Object> params);

	PlayListDto requestPlaylist(Player player);

	List<MediaFile> requestMediaFiles(Player player, Map<String, Object> params);

	boolean hasStatusCache(Player player);

	boolean hasConnection(Player player);

	Schedule requestSchedule(Player player, Map<String, Object> params);

	PlayListDto updatePlaylist(Player player, PlayListDto playlist);
}
