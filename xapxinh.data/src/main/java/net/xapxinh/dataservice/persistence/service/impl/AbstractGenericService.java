package net.xapxinh.dataservice.persistence.service.impl;

import java.io.Serializable;
import java.util.List;

import net.xapxinh.dataservice.persistence.dao.IGenericDao;
import net.xapxinh.dataservice.persistence.service.IGenericService;

import org.springframework.transaction.annotation.Transactional;

@Transactional
public abstract class AbstractGenericService<T extends Serializable> implements IGenericService<T> {	
	
	public List<T> loadAll() {
		return getDao().loadAll();
	}
	public T findById(final Long id) {
		return getDao().findById(id);
	}
	public T insert(final T entity) {
		return getDao().insert(entity);
	}
	public T saveOrUpdate(final T entity) {
		return getDao().saveOrUpdate(entity);
	}
	public T update(final T entity) {
		return getDao().update(entity);
	}
	public void delete(final T entity) {
		getDao().delete(entity);
	}
	public void deleteById(final Long entityId) {
		getDao().deleteById(entityId);
	}
	
	protected abstract IGenericDao<T> getDao();
}
