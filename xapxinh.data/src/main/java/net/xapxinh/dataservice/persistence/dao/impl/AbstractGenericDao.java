package net.xapxinh.dataservice.persistence.dao.impl;

import java.io.Serializable;
import java.util.List;

import net.xapxinh.dataservice.persistence.dao.IGenericDao;

import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.base.Preconditions;

@Transactional
@SuppressWarnings("unchecked")
public class AbstractGenericDao<T extends Serializable> implements IGenericDao<T> {

	private Class<T> clazz;
	
	private SessionFactory sessionFactory;
	
	public AbstractGenericDao(SessionFactory sessionFactory) {
		this.sessionFactory = sessionFactory;
	}
	
	protected final void setClass(final Class<T> clasz) {
		clazz = Preconditions.checkNotNull(clasz);
	}

	@Override
	public T insert(T entity) {
		Preconditions.checkNotNull(entity);
		getCurrentSession().save(entity);
		return entity;
	}

	@Override
	public final T update(T entity) {
		Preconditions.checkNotNull(entity);
		return (T) getCurrentSession().merge(entity);
	}
	
	@Override
	public final T saveOrUpdate(T entity) {
		Preconditions.checkNotNull(entity);
		getCurrentSession().saveOrUpdate(entity);
		return entity;
	}

	@Override
	public void delete(T entity) {
		Preconditions.checkNotNull(entity);
		getCurrentSession().delete(entity);
	}
	
	

	@Override
	public void deleteById(Long id) {
		final T entity = findById(id);
		Preconditions.checkState(entity != null);
		delete(entity);
	}
	
	@Override
	public List<T> loadAll() {
		return getCurrentSession().createQuery("from " + clazz.getName()).list();
	}
	
	@Override
	public T findById(Long id) {
		return (T) getCurrentSession().get(clazz, id);
	}
	
	@Override
	public List<T> findMany(Query query) {
		return query.list();
	}
	
	protected T getFirst(List<T> list) {
		if (list == null || list.isEmpty()) {
			return null;
		}
		return list.get(0);
	}
	
	protected final Session getCurrentSession() {
		return sessionFactory.getCurrentSession();
	}
	
	protected int getStartIndex(int page, int maxResult) {
		int start = 0;
		if (page >= 1) {
			start = (page - 1) * maxResult;
		}
		return start;
	}
}
