package net.ionoff.center.server.persistence.dao.impl;

import net.ionoff.center.server.entity.Controller;
import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.persistence.dao.IControllerDao;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.Query;
import java.util.Collections;
import java.util.List;

@Repository
@Transactional
public class ControllerDaoImpl extends AbstractGenericDao<Controller> implements IControllerDao {

	public ControllerDaoImpl() {
		super();
		setClass(Controller.class);
	}

	private long countByProjectIdName(long projectId, String keyWord) {
		if (keyWord == null || keyWord.isEmpty()) {
			return countByProjectId(projectId);
		}
		String sql = "select count(controller)"
					+ " from Controller as controller"
					+ " where lower(controller.name) like :keyWord"
					+ " and controller.project.id = :projectId";
		Query query = entityManager.createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("keyWord", "%" + keyWord.toLowerCase() + "%");
		return countObjects(query);
	}

	@Override
	public long countByProjectId(long projectId) {
		String sql = "select count(controller)"
				+ " from Controller as controller"
				+ " where controller.project.id = :projectId";
		Query query = entityManager.createQuery(sql)
				.setParameter("projectId", projectId);
		return countObjects(query);
	}

	private List<Controller> findByProjectIdName(long projectId, String keyWord, int fromIndex, int maxResults,
                                                 String orderBy, boolean isAscending) {
		if (keyWord == null || keyWord.isEmpty()) {
			return findByProjectId(projectId, fromIndex, maxResults, orderBy, isAscending);
		}
		String sql = "select distinct controller"
					+ " from Controller as controller"
					+ " where controller.name like :keyWord"
					+ " and controller.project.id = :projectId"
					+ " order by controller." + orderBy;
		if (!isAscending) {
			sql = sql + " desc";
		}
		
		Query query = entityManager.createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("keyWord", "%" + keyWord + "%");
		return findMany(query, fromIndex, maxResults);
	}

	private List<Controller> findByProjectId(long projectId, int fromIndex, int maxResults,
                                             String orderBy, boolean isAscending) {
		String sql = "select distinct controller"
					+ " from Controller as controller"
					+ " where controller.project.id = :projectId"
					+ " order by controller." + orderBy;
		if (!isAscending) {
			sql = sql + " desc";
		}
		Query query = entityManager.createQuery(sql)
				.setParameter("projectId", projectId);
		return findMany(query, fromIndex, maxResults);
	}

	@Override
	public List<Controller> findByIsLazy() {
		String sql = "select * from controllers where type_='Ep2Controller'" +
				" or type_='Ec100Controller'";
		Query query = entityManager.createNativeQuery(sql, Controller.class);
		return query.getResultList();
	}

	@Override
	public List<Controller> findByProjectId(long projectId) {
		String sql = "select distinct controller"
				+ " from Controller as controller"
				+ " where controller.project.id = :projectId"
				+ " order by controller.name";
		Query query = entityManager.createQuery(sql)
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
		String sql = "select distinct controller"
				+ " from Controller as controller"
				+ " where controller.ip = :ip"
				+ " and controller.port = :port"
				+ " order by controller.name";
		Query query = entityManager.createQuery(sql)
				.setParameter("ip", ip)
				.setParameter("port", port);
		return findMany(query);
	}

	@Override
	public List<Controller> findByMac(String mac) {
		String sql = "select distinct controller"
				+ " from Controller as controller"
				+ " where controller.key = :mac"
				+ " order by controller.name";
		Query query = entityManager.createQuery(sql)
				.setParameter("mac", mac);
		return findMany(query);
	}

	@Override
	public List<Controller> findByIp(String ip) {
		String sql = "select distinct controller"
				+ " from Controller as controller"
				+ " where controller.ip = :ip"
				+ " order by controller.name";
		Query query = entityManager.createQuery(sql)
				.setParameter("ip", ip);
		return findMany(query);
	}
}
