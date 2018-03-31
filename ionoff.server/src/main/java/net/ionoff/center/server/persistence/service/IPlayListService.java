package net.ionoff.center.server.persistence.service;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.PlayList;
import net.ionoff.center.server.entity.User;
import net.xapxinh.center.shared.dto.PlayListDto;

@Transactional
public interface IPlayListService extends IGenericService<PlayList, PlayListDto> {
	
	List<PlayListDto> findDtoByUser(User user);

}
