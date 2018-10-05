package net.ionoff.center.server.persistence.dao.impl;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import net.ionoff.center.server.entity.relaydriver.Ec100RelayDriver;
import net.ionoff.center.server.entity.relaydriver.Ep2RelayDriver;
import org.hibernate.Criteria;
import org.hibernate.Query;
import org.hibernate.SQLQuery;
import org.hibernate.SessionFactory;
import org.hibernate.criterion.Restrictions;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.RelayDriver;
import net.ionoff.center.server.persistence.dao.IRelayDriverDao;

@Transactional
public class RelayDriverDaoImpl extends AbstractGenericDao<RelayDriver> implements IRelayDriverDao {

	public RelayDriverDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(RelayDriver.class);
	}

	private long countByProjectIdName(long projectId, String keyWord) {
		if (keyWord == null || keyWord.isEmpty()) {
			return countByProjectId(projectId);
		}
		String sql = "select count(relayDriver)"
					+ " from RelayDriver as relayDriver" 
					+ " where lower(relayDriver.name) like :keyWord"
					+ " and relayDriver.project.id = :projectId";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("keyWord", "%" + keyWord.toLowerCase() + "%");
		return countObjects(query);
	}
	
	private long countByProjectId(long projectId) {
		String sql = "select count(relayDriver)"
				+ " from RelayDriver as relayDriver"
				+ " where relayDriver.project.id = :projectId";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId);
		return countObjects(query);
	}

	private List<RelayDriver> findByProjectIdName(long projectId, String keyWord, int fromIndex, int maxResults, 
			String orderBy, boolean isAscending) {
		if (keyWord == null || keyWord.isEmpty()) {
			return findByProjectId(projectId, fromIndex, maxResults, orderBy, isAscending);
		}
		String sql = "select distinct relayDriver" 
					+ " from RelayDriver as relayDriver" 
					+ " where relayDriver.name like :keyWord"
					+ " and relayDriver.project.id = :projectId" 
					+ " order by relayDriver." + orderBy;
		if (!isAscending) {
			sql = sql + " desc";
		}
		
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("keyWord", "%" + keyWord + "%");
		return findMany(query, fromIndex, maxResults);
	}

	private List<RelayDriver> findByProjectId(long projectId, int fromIndex, int maxResults, 
			String orderBy, boolean isAscending) {
		String sql = "select distinct relayDriver"
					+ " from RelayDriver as relayDriver"
					+ " where relayDriver.project.id = :projectId"
					+ " order by relayDriver." + orderBy;
		if (!isAscending) {
			sql = sql + " desc";
		}
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId);
		return findMany(query, fromIndex, maxResults);
	}

	@Override
	public List<RelayDriver> findByIsLazy() {
		String sql = "select * from relaydrivers where type_='Ep2RelayDriver'" +
				" or type_='Ec100RelayDriver'";
		Query query = getCurrentSession().
                    createSQLQuery(sql).addEntity(RelayDriver.class);
		return query.list();
	}

	@Override
	public List<RelayDriver> findByProjectId(long projectId) {
		String sql = "select distinct relayDriver"
				+ " from RelayDriver as relayDriver"
				+ " where relayDriver.project.id = :projectId"
				+ " order by relayDriver.name";
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
	public List<RelayDriver> findByCriteria(QueryCriteria criteria) {
		if (NAME.equals(criteria.getSearchField())) {
			return findByProjectIdName(criteria.getProjectId(), criteria.getSearchKey(), 
					criteria.getFromIndex(), criteria.getMaxResults(), criteria.getSortBy(), criteria.getIsAscending());
		}
		return Collections.emptyList();
	}

	@Override
	public List<RelayDriver> findByIpPort(String ip, Integer port) {
		String sql = "select distinct relayDriver"
				+ " from RelayDriver as relayDriver"
				+ " where relayDriver.ip = :ip"
				+ " and relayDriver.port = :port"
				+ " order by relayDriver.model";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("ip", ip)
				.setParameter("port", port);
		return findMany(query);
	}

	@Override
	public List<RelayDriver> findByMac(String mac) {
		String sql = "select distinct relayDriver"
				+ " from RelayDriver as relayDriver"
				+ " where relayDriver.key = :mac"
				+ " order by relayDriver.model";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("mac", mac);
		return findMany(query);
	}

	@Override
	public List<RelayDriver> findByIp(String ip) {
		String sql = "select distinct relayDriver"
				+ " from RelayDriver as relayDriver"
				+ " where relayDriver.ip = :ip"
				+ " order by relayDriver.model";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("ip", ip);
		return findMany(query);
	}
}
