package net.ionoff.center.server.persistence.dao.impl;

import java.util.List;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.Controller;
import net.ionoff.center.server.entity.Switch;
import net.ionoff.center.server.persistence.dao.IControllerDao;
import net.ionoff.center.server.persistence.dao.ISwitchDao;

@Repository
@Transactional
public class SwitchDaoImpl extends AbstractGenericDao<Switch> implements ISwitchDao {
	
	@Autowired
	private IControllerDao controllerDao;

	@Autowired
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
			Controller controller = controllerDao.findById(driverId);
			if (controller == null) {
				return zwitch;
			}
			if (controller.getInput() > index) {
				zwitch = new Switch();
				zwitch.setDriver(controller);
				zwitch.setIndex(index);
				return insert(zwitch);
			}
		}
		return zwitch;
	}
}
