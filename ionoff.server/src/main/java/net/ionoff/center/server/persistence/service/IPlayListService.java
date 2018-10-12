package net.ionoff.center.server.persistence.service;

import java.util.List;

import net.ionoff.center.server.entity.PlayList;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.User;
import net.ionoff.center.shared.dto.player.PlayListDto;

@Transactional
public interface IPlayListService extends IGenericService<PlayList, PlayListDto> {
	
	List<PlayListDto> findDtoByUser(User user);

}
