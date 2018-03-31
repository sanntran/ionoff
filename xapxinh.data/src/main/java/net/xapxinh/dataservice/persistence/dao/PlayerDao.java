package net.xapxinh.dataservice.persistence.dao;

import net.xapxinh.dataservice.entity.Player;

import org.springframework.transaction.annotation.Transactional;

@Transactional
public interface PlayerDao extends IGenericDao<Player> {

	public Player findByMac(String key);
}
