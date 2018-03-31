package net.ionoff.center.server.persistence.dao;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.PlayList;

@Transactional
public interface IPlayListDao extends IGenericDao<PlayList> {

	List<PlayList> findByUserId(long userId);

	List<PlayList> findByUserName(String userName);
}
