package net.xapxinh.dataservice.persistence.service.impl;

import org.springframework.transaction.annotation.Transactional;

import net.xapxinh.dataservice.entity.Player;
import net.xapxinh.dataservice.exception.UnknownPlayerException;
import net.xapxinh.dataservice.persistence.dao.PlayerDao;
import net.xapxinh.dataservice.persistence.service.PlayerService;

@Transactional
public class PlayerServiceImpl extends AbstractGenericService<Player> implements PlayerService {

	private PlayerDao playerDao;
	
	public PlayerServiceImpl(PlayerDao playerDao) {
		this.playerDao = playerDao;
	}

	@Override
	protected PlayerDao getDao() {
		return playerDao;
	}

	@Override
	public Player findByMac(String mac) throws UnknownPlayerException {
		if (mac ==  null || mac.isEmpty()) {
			throw new UnknownPlayerException(mac);
		}
		Player player = getDao().findByMac(mac);
		if (player == null) {
			throw new UnknownPlayerException(mac);
		}
		return player;
	}
}
