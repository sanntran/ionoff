package net.ionoff.center.server.persistence.service;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.User;
import net.xapxinh.center.server.entity.PlayLeaf;
import net.xapxinh.center.server.entity.PlayNode;
import net.xapxinh.center.shared.dto.PlayLeafDto;
import net.xapxinh.center.shared.dto.PlayNodeDto;

@Transactional
public interface IPlayNodeService extends IGenericService<PlayNode, PlayNodeDto> {
	
	List<PlayNode> findByPlayListId(long playListId);

	PlayLeaf findLeafById(long leafId);

	List<PlayNodeDto> findDtoByPlayListId(User user, Long playlistId);
	
	PlayNodeDto findNodeDtoById(Long playNodeId);

	PlayLeafDto findLeafDtoById(Long playLeafId);
}
