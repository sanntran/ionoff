package net.ionoff.center.server.persistence.dao.impl;

import java.util.List;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.RelayDriver;
import net.ionoff.center.server.entity.Switch;
import net.ionoff.center.server.persistence.dao.IRelayDriverDao;
import net.ionoff.center.server.persistence.dao.ISwitchDao;

@Transactional
public class SwitchDaoImpl extends AbstractGenericDao<Switch> implements ISwitchDao {
	
	@Autowired
	private IRelayDriverDao relayDriverDao;
	
	public SwitchDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(Switch.class);
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<Switch> findByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Switch findByDriverId(Long driverId, Integer index) {
		String sql = "select distinct zwitch"
				+ " from Switch as zwitch" 
				+ " where zwitch.driver.id = :driverId"
				+ " and zwitch.index = :index";
	
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("driverId", driverId)
				.setParameter("index", index);
	
		List<Switch> zwitchs = findMany(query);
		Switch zwitch = getFirst(zwitchs);
		if (zwitch == null) {
			RelayDriver relayDriver = relayDriverDao.findById(driverId);
			if (relayDriver == null) {
				return zwitch;
			}
			if (relayDriver.getInput() > index) {
				zwitch = new Switch();
				zwitch.setDriver(relayDriver);
				zwitch.setIndex(index);
				return insert(zwitch);
			}
		}
		return zwitch;
	}
}
