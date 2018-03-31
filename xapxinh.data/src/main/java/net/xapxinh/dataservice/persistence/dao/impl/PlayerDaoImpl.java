package net.xapxinh.dataservice.persistence.dao.impl;

import java.util.List;

import net.xapxinh.dataservice.entity.Player;
import net.xapxinh.dataservice.persistence.dao.PlayerDao;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.transaction.annotation.Transactional;

@Transactional
public class PlayerDaoImpl extends AbstractGenericDao<Player> implements PlayerDao {

	public PlayerDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(Player.class);
	}
	
	@Override
	public Player findByMac(String mac) {
		
		String sql = "select distinct player "
					+ "from Player as player "
					+ "where player.mac = :mac ";
		
		Query query = getCurrentSession().createQuery(sql)						
					.setString("mac", mac);
		
		List<Player> players = findMany(query);	
		
		return getFirst(players);
	}
}
