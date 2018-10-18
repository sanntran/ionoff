package net.ionoff.center.server.persistence.dao.impl;

import java.util.Collections;
import java.util.List;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.Controller;
import net.ionoff.center.server.persistence.dao.IControllerDao;

@Repository
@Transactional
public class ControllerDaoImpl extends AbstractGenericDao<Controller> implements IControllerDao {

	@Autowired
	public ControllerDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(Controller.class);
	}

	private long countByProjectIdName(long projectId, String keyWord) {
		if (keyWord == null || keyWord.isEmpty()) {
			return countByProjectId(projectId);
		}
		String sql = "select count(restcontroller)"
					+ " from Controller as restcontroller"
					+ " where lower(restcontroller.name) like :keyWord"
					+ " and restcontroller.project.id = :projectId";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("keyWord", "%" + keyWord.toLowerCase() + "%");
		return countObjects(query);
	}

	@Override
	public long countByProjectId(long projectId) {
		String sql = "select count(restcontroller)"
				+ " from Controller as restcontroller"
				+ " where restcontroller.project.id = :projectId";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId);
		return countObjects(query);
	}

	private List<Controller> findByProjectIdName(long projectId, String keyWord, int fromIndex, int maxResults,
                                                 String orderBy, boolean isAscending) {
		if (keyWord == null || keyWord.isEmpty()) {
			return findByProjectId(projectId, fromIndex, maxResults, orderBy, isAscending);
		}
		String sql = "select distinct restcontroller"
					+ " from Controller as restcontroller"
					+ " where restcontroller.name like :keyWord"
					+ " and restcontroller.project.id = :projectId"
					+ " order by restcontroller." + orderBy;
		if (!isAscending) {
			sql = sql + " desc";
		}
		
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("keyWord", "%" + keyWord + "%");
		return findMany(query, fromIndex, maxResults);
	}

	private List<Controller> findByProjectId(long projectId, int fromIndex, int maxResults,
                                             String orderBy, boolean isAscending) {
		String sql = "select distinct restcontroller"
					+ " from Controller as restcontroller"
					+ " where restcontroller.project.id = :projectId"
					+ " order by restcontroller." + orderBy;
		if (!isAscending) {
			sql = sql + " desc";
		}
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId);
		return findMany(query, fromIndex, maxResults);
	}

	@Override
	public List<Controller> findByIsLazy() {
		String sql = "select * from controllers where type_='Ep2Controller'" +
				" or type_='Ec100Controller'";
		Query query = getCurrentSession().
                    createSQLQuery(sql).addEntity(Controller.class);
		return query.list();
	}

	@Override
	public List<Controller> findByProjectId(long projectId) {
		String sql = "select distinct restcontroller"
				+ " from Controller as restcontroller"
				+ " where restcontroller.project.id = :projectId"
				+ " order by restcontroller.name";
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
			return findByProjectIdName(criteria.getProjectId(), criteria.getSearchKey(), 
					criteria.getFromIndex(), criteria.getMaxResults(), criteria.getSortBy(), criteria.getIsAscending());
		}
		return Collections.emptyList();
	}

	@Override
	public List<Controller> findByIpPort(String ip, Integer port) {
		String sql = "select distinct restcontroller"
				+ " from Controller as restcontroller"
				+ " where restcontroller.ip = :ip"
				+ " and restcontroller.port = :port"
				+ " order by restcontroller.name";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("ip", ip)
				.setParameter("port", port);
		return findMany(query);
	}

	@Override
	public List<Controller> findByMac(String mac) {
		String sql = "select distinct restcontroller"
				+ " from Controller as restcontroller"
				+ " where restcontroller.key = :mac"
				+ " order by restcontroller.name";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("mac", mac);
		return findMany(query);
	}

	@Override
	public List<Controller> findByIp(String ip) {
		String sql = "select distinct restcontroller"
				+ " from Controller as restcontroller"
				+ " where restcontroller.ip = :ip"
				+ " order by restcontroller.name";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("ip", ip);
		return findMany(query);
	}
}
