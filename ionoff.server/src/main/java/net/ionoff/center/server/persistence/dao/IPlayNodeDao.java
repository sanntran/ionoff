package net.ionoff.center.server.persistence.dao;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.xapxinh.center.server.entity.PlayNode;

@Transactional
public interface IPlayNodeDao extends IGenericDao<PlayNode> {

	List<PlayNode> findByPlayListId(long playListId);
}
