package net.ionoff.center.server.persistence.dao.impl;

import java.util.List;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.RelayGroupRelay;
import net.ionoff.center.server.persistence.dao.IRelayGroupRelayDao;

@Repository
@Transactional
public class RelayGroupRelayDaoImpl extends AbstractGenericDao<RelayGroupRelay> implements IRelayGroupRelayDao {

	@Autowired
	public RelayGroupRelayDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(RelayGroupRelay.class);
	}

	@Override
	public synchronized RelayGroupRelay update(RelayGroupRelay relayGroupRelay) {
		return super.update(relayGroupRelay);
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<RelayGroupRelay> findByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public RelayGroupRelay findByRelayIdGroupId(Long relayId, Long groupId) {
		String sql = "SELECT DISTINCT relayGroupRelay" 
				+ " FROM RelayGroupRelay AS relayGroupRelay" 
				+ " WHERE relayGroupRelay.group.id = :groupId"
				+ " AND relayGroupRelay.relay.id = :relayId";
		
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("relayId", relayId)
				.setParameter("groupId", groupId);
		
		return getFirst(findMany(query));
	}
	
}
