package net.ionoff.center.server.persistence.dao.impl;

import java.util.List;

import javax.persistence.Query;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.RelayGroup;
import net.ionoff.center.server.persistence.dao.IRelayGroupDao;

@Repository
@Transactional
public class RelayGroupDaoImpl extends AbstractGenericDao<RelayGroup> implements IRelayGroupDao {

	public RelayGroupDaoImpl() {
		super();
		setClass(RelayGroup.class);
	}

	@Override
	public synchronized RelayGroup update(RelayGroup relayGroup) {
		return super.update(relayGroup);
		
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<RelayGroup> findByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<RelayGroup> findByRelayId(Long relayId) {
		String sql = "SELECT DISTINCT relayGroup" 
				+ " FROM RelayGroup AS relayGroup, RelayGroupRelay as relayGroupRelay" 
				+ " WHERE relayGroupRelay.group.id = relayGroup.id"
				+ " AND relayGroupRelay.relay.id = :relayId";
		
		Query query = entityManager.createQuery(sql)
				.setParameter("relayId", relayId);
		
		return findMany(query);
	}
	
}
