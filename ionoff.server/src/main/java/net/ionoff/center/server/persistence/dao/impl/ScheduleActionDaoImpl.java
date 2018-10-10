package net.ionoff.center.server.persistence.dao.impl;

import java.util.List;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.ScheduleAction;
import net.ionoff.center.server.persistence.dao.IScheduleActionDao;

@Repository
@Transactional
public class ScheduleActionDaoImpl extends AbstractGenericDao<ScheduleAction> implements IScheduleActionDao {

	@Autowired
	public ScheduleActionDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(ScheduleAction.class);
	}

	@Override
	public void deleteByRelayId(long relayId) {
		String sql = "delete from ScheduleAction where relay.id = :relayId";
		Query query = getCurrentSession().createQuery(sql)
					.setParameter("relayId", relayId);
		query.executeUpdate();
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<ScheduleAction> findByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}
}
