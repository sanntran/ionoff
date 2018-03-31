package net.ionoff.center.server.persistence.dao.impl;

import java.util.Collections;
import java.util.List;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Controller;
import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.persistence.dao.IControllerDao;

@Transactional
public class ControllerDaoImpl extends AbstractGenericDao<Controller> implements IControllerDao {

	public ControllerDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(Controller.class);
	}

	private long countByProjectIdName(long projectId, String keyWord) {
		if (keyWord == null || keyWord.isEmpty()) {
			String sql = "select count(controller)"
					+ " from Controller as controller"
					+ " where controller.project.id = :projectId";
			Query query = getCurrentSession().createQuery(sql)
					.setParameter("projectId", projectId);
			return countObjects(query);
			
		}
		String sql = "select count(controller)"
					+ " from Controller as controller" 
					+ " where lower(controller.name) like :keyWord"
					+ " and controller.project.id = :projectId";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("keyWord", "%" + keyWord.toLowerCase() + "%");
		return countObjects(query);
	}
	
	private List<Controller> findByName(long projectId, String keyWord, int fromIndex, int maxResults, boolean isAscending) {
		if (keyWord == null || keyWord.isEmpty()) {
			return findByProjectId(projectId, fromIndex, maxResults, isAscending);
		}
		String sql = "select distinct controller" 
					+ " from Controller as controller" 
					+ " where controller.name like :keyWord"
					+ " and controller.project.id = :projectId" 
					+ " order by controller.name";
		if (!isAscending) {
			sql = sql + " desc";
		}
		
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("keyWord", "%" + keyWord + "%");
		return findMany(query, fromIndex, maxResults);
	}

	private List<Controller> findByProjectId(long projectId, int fromIndex, int maxResults, boolean isAscending) {
		String sql = "select distinct controller"
					+ " from Controller as controller"
					+ " where controller.project.id = :projectId"
					+ " order by controller.name";
		if (!isAscending) {
			sql = sql + " desc";
		}
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId);
		return findMany(query, fromIndex, maxResults);
	}
	
	@Override
	public List<Controller> findByProjectId(long projectId) {
		String sql = "select distinct controller"
				+ " from Controller as controller"
				+ " where controller.project.id = :projectId"
				+ " order by controller.name";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId);
		return findMany(query);
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		if (NAME.equals(criteria.getSearchField())) {
			return countByProjectIdName(criteria.getProjectId(), criteria.getSearchKey());
		}
		return 0;
	}

	@Override
	public List<Controller> findByCriteria(QueryCriteria criteria) {
		if (NAME.equals(criteria.getSearchField())) {
			return findByName(criteria.getProjectId(), criteria.getSearchKey(), 
					criteria.getFromIndex(), criteria.getMaxResults(), criteria.getIsAscending());
		}
		return Collections.emptyList();
	}

	@Override
	public List<Controller> findByIpPort(String ip, Integer port) {
		String sql = "select distinct controller"
				+ " from Controller as controller"
				+ " where controller.ip = :ip"
				+ " and controller.port = :port"
				+ " order by controller.model";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("ip", ip)
				.setParameter("port", port);
		return findMany(query);
	}

	@Override
	public List<Controller> findByMac(String mac) {
		String sql = "select distinct controller"
				+ " from Controller as controller"
				+ " where controller.key = :mac"
				+ " order by controller.model";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("mac", mac);
		return findMany(query);
	}

	@Override
	public List<Controller> findByIp(String ip) {
		String sql = "select distinct controller"
				+ " from Controller as controller"
				+ " where controller.ip = :ip"
				+ " order by controller.model";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("ip", ip);
		return findMany(query);
	}
}
