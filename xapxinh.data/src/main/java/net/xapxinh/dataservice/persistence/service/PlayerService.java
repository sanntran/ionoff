package net.xapxinh.dataservice.persistence.service;

import net.xapxinh.dataservice.entity.Player;
import net.xapxinh.dataservice.exception.UnknownPlayerException;

import org.springframework.transaction.annotation.Transactional;

@Transactional
public interface PlayerService extends IGenericService<Player> {		

	public Player findByMac(String mac) throws UnknownPlayerException;

}
