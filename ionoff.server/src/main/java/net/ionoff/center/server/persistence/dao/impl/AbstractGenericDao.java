package net.ionoff.center.server.persistence.dao.impl;

import java.io.Serializable;
import java.util.List;

import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.base.Preconditions;

import net.ionoff.center.server.persistence.dao.IGenericDao;

@Transactional
@SuppressWarnings("unchecked")
public abstract class AbstractGenericDao<T extends Serializable> implements IGenericDao<T> {

	public static final String NAME = "name";
	public static final String AREA_NAME = "areaName";
	public static final String ZONE_NAME = "zoneName";
	public static final String DEVICE_NAME = "deviceName";

	private Class<T> clazz;

	protected final SessionFactory sessionFactory;

	public AbstractGenericDao(SessionFactory sessionFactory) {
		this.sessionFactory = sessionFactory;
	}

	protected final void setClass(final Class<T> clasz) {
		clazz = Preconditions.checkNotNull(clasz);
	}
	
	@Override
	public final String getClazz() {
		return clazz.getSimpleName();
	}

	@Override
	public T insert(T entity) {
		Preconditions.checkNotNull(entity);
		getCurrentSession().save(entity);
		return entity;
	}

	@Override

	public T update(T entity) {
		synchronized (entity) {
			Preconditions.checkNotNull(entity);
			getCurrentSession().merge(entity);
			return entity;
		}
	}

	@Override
	public void delete(T entity) {
		Preconditions.checkNotNull(entity);
		getCurrentSession().delete(entity);
	}

	@Override
	public void deleteById(long id) {
		final T entity = findById(id);
		Preconditions.checkState(entity != null);
		delete(entity);
	}

	@Override
	public List<T> loadAll() {
		return getCurrentSession().createQuery("from " + clazz.getName()).list();
	}

	@Override
	public T findById(long id) {
		return (T) getCurrentSession().get(clazz, id);
	}

	@Override
	public T loadById(long id) {
		return (T) getCurrentSession().load(clazz, id);
	}

	@Override
	public List<T> findMany(Query query) {
		return query.list();
	}

	@Override
	public List<T> findMany(Query query, int fromIndex, int maxResults) {
		return query.setFirstResult(fromIndex).setMaxResults(maxResults).list();
	}

	protected T getFirst(List<T> list) {
		if (list == null || list.isEmpty()) {
			return null;
		}
		return list.get(0);
	}

	protected long countObjects(Query query) {
		final Long result = Long.parseLong(query.list().get(0).toString());
		return result;
	}
	
	@Override
	public final Session getCurrentSession() {
		return sessionFactory.getCurrentSession();
	}
	
	@Override
	public final SessionFactory getSessionFactory() {
		return sessionFactory;
	}
}
