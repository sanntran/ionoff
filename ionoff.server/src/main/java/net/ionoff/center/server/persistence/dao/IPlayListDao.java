package net.ionoff.center.server.persistence.dao;

import java.util.List;

import net.ionoff.center.server.entity.PlayList;
import org.springframework.transaction.annotation.Transactional;

@Transactional
public interface IPlayListDao extends IGenericDao<PlayList> {

	List<PlayList> findByUserId(long userId);

	List<PlayList> findByUserName(String userName);
}
