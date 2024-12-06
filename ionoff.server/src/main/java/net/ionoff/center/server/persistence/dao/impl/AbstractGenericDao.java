package net.ionoff.center.server.persistence.dao.impl;

import com.google.common.base.Preconditions;
import net.ionoff.center.server.persistence.dao.IGenericDao;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.EntityManager;
import javax.persistence.Query;
import java.io.Serializable;
import java.util.List;

@Transactional
@SuppressWarnings("unchecked")
public abstract class AbstractGenericDao<T extends Serializable> implements IGenericDao<T> {

	public static final String NAME = "name";
	public static final String AREA_NAME = "areaName";
	public static final String ZONE_NAME = "zoneName";
	public static final String DEVICE_NAME = "deviceName";

	private Class<T> clazz;

	@Autowired
	protected EntityManager entityManager;

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
		entityManager.persist(entity);
		return entity;
	}

	@Override

	public T update(T entity) {
		synchronized (entity) {
			Preconditions.checkNotNull(entity);
			entityManager.merge(entity);
			return entity;
		}
	}

	@Override
	public void delete(T entity) {
		Preconditions.checkNotNull(entity);
		entityManager.remove(entity);
	}

	@Override
	public void deleteById(long id) {
		final T entity = findById(id);
		Preconditions.checkState(entity != null);
		delete(entity);
	}

	@Override
	public List<T> loadAll() {
		return entityManager.createQuery("from " + clazz.getName()).getResultList();
	}

	@Override
	public T findById(long id) {
		return (T) entityManager.find(clazz, id);
	}

	@Override
	public T loadById(long id) {
		return (T) entityManager.find(clazz, id);
	}

	public List<T> findMany(Query query) {
		return query.getResultList();
	}

	public List<T> findMany(Query query, int fromIndex, int maxResults) {
		return query.setFirstResult(fromIndex).setMaxResults(maxResults).getResultList();
	}

	protected T getFirst(List<T> list) {
		if (list == null || list.isEmpty()) {
			return null;
		}
		return list.get(0);
	}

	protected long countObjects(Query query) {
		final Long result = Long.parseLong(query.getResultList().get(0).toString());
		return result;
	}
}
