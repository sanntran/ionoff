package net.ionoff.center.server.persistence.dao;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.xapxinh.center.server.entity.PlayLeaf;

@Transactional
public interface IPlayLeafDao extends IGenericDao<PlayLeaf> {

	List<PlayLeaf> findByNodeId(long nodeId);
}
