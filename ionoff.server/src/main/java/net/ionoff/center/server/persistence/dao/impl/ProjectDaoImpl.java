package net.ionoff.center.server.persistence.dao.impl;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

import javax.persistence.Query;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Project;
import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.persistence.dao.IProjectDao;

@Repository
@Transactional
public class ProjectDaoImpl extends AbstractGenericDao<Project> implements IProjectDao {

	public ProjectDaoImpl() {
		super();
		setClass(Project.class);
	}
	
	public List<Project> findByName(String name, int fromIndex, int maxResults, boolean isAscending) {
		if (name == null || name.isEmpty()) {
			return findAll(fromIndex, maxResults, isAscending);
		}
		String sql = "select distinct project" 
					+ " from Project as project" 
					+ " where project.name like :name" 
					+ " order by project.name";
		if (!isAscending) {
			sql = sql + " desc";
		}
		
		Query query = entityManager.createQuery(sql)
				.setParameter("name", "%" + name + "%");
		return findMany(query, fromIndex, maxResults);
	}

	private List<Project> findAll(int fromIndex, int maxResults, boolean isAscending) {
		String sql = "select distinct project"
					+ " from Project as project"
					+ " order by project.name";
		if (!isAscending) {
			sql = sql + " desc";
		}
		Query query = entityManager.createQuery(sql);
		return findMany(query, fromIndex, maxResults);
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		if (criteria.isBlankKey()) {
			String sql = "select count(project)"
					+ " from Project as project";
			Query query = entityManager.createQuery(sql);
			return countObjects(query);
			
		}
		String sql = "select count(project)"
					+ " from Project as project" 
					+ " where lower(project.name) like :name";
		Query query = entityManager.createQuery(sql)
				.setParameter("name", "%" + criteria.getSearchKey().toLowerCase() + "%");
		return countObjects(query);
	}

	@Override
	public List<Project> findByCriteria(QueryCriteria criteria) {
		if (NAME.equals(criteria.getSearchField())) {
			return findByName(criteria.getSearchKey(), 
					criteria.getFromIndex(), criteria.getMaxResults(), criteria.getIsAscending());
		}
		return Collections.emptyList();
	}

	@Override
	public List<Project> findByUserId(Long userId) {
		String sql = "select distinct project" 
				+ " from Project as project, UserProject as userProject" 
				+ " where project.id = userProject.project.id"
				+ " and userProject.user.id = :userId"
				+ " and userProject.role = true" 
				+ " order by project.id";
		Query query = entityManager.createQuery(sql).setParameter("userId", userId);
		return findMany(query);
	}

	@Override
	public Optional<Project> findFirst() {
		String sql = "SELECT DISTINCT * FROM ionoff.projects ORDER BY id LIMIT 1";
		Query query = entityManager.createNativeQuery(sql, Project.class);
		List<Project> projects = findMany(query);
		return projects.isEmpty() ? Optional.empty() : Optional.of(projects.get(0));
	}
}
