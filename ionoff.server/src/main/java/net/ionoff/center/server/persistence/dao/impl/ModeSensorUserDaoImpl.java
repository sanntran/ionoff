package net.ionoff.center.server.persistence.dao.impl;

import java.util.List;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.ModeSensorUser;
import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.persistence.dao.IModeSensorUserDao;

@Transactional
public class ModeSensorUserDaoImpl extends AbstractGenericDao<ModeSensorUser> implements IModeSensorUserDao {
	
	public ModeSensorUserDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(ModeSensorUser.class);
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<ModeSensorUser> findByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public void removeByUserProjectId(long userId, long projectId) {
		String sql = "DELETE FROM ModeSensorUser AS modeSensorUser"
				+ " WHERE modeSensorUser.user.id= :userId"
				+ " AND modeSensorUser.project.id= :projectId";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("userId", userId)
				.setParameter("projectId", projectId);
	
		query.executeUpdate();
	}
}
