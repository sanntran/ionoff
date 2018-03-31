package net.ionoff.center.server.persistence.service.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import org.hibernate.Cache;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.exception.EntityNotFoundException;
import net.ionoff.center.server.objmapper.QueryCriteriaMapper;
import net.ionoff.center.server.persistence.dao.IGenericDao;
import net.ionoff.center.server.persistence.dao.impl.AbstractGenericDao;
import net.ionoff.center.server.persistence.service.IGenericService;
import net.ionoff.center.shared.dto.QueryCriteriaDto;

@Transactional
public abstract class AbstractGenericService<T extends Serializable, D extends Serializable>
	implements IGenericService<T, D> {
	
	@Override
	public List<T> loadAll() {
		return getDao().loadAll();
	}
	
	@Override
	public T findById(final long id) {
		return getDao().findById(id);
	}
	
	@Override
	public T loadById(final long id) {
		return getDao().loadById(id);
	}
	
	@Override
	public T insert(final T entity) {
		return getDao().insert(entity);
	}
	
	@Override
	public T update(final T entity) {
		return getDao().update(entity);
	}
	
	@Override
	public void delete(final T entity) {
		getDao().delete(entity);
	}
	
	@Override
	public void deleteById(final long entityId) {
		getDao().deleteById(entityId);
		Cache cache = getDao().getSessionFactory().getCache();
		if (cache != null) {
		    cache.evictAllRegions();
		}
	}

	@Override
	public long countByCriteria(QueryCriteriaDto criteriaDto) {
		try {
			return countById(criteriaDto.getSearchKey(), criteriaDto.getSearchField());
		}
		catch (NumberFormatException e) {
			QueryCriteria criteria = QueryCriteriaMapper.toQueryCriteria(criteriaDto);
			return getDao().countByCriteria(criteria);
		}
	}

	protected boolean isIdSearchKey(String searchKey, String searchField) {
		return AbstractGenericDao.NAME.equals(searchField) && searchKey.startsWith("#");
	}
	
	protected long countById(String searchKey, String searchField) {
		if (isIdSearchKey(searchKey, searchField)) {
			String id = searchKey.replace("#", "");
			Long idNumber = Long.parseLong(id);
			if (findById(idNumber) != null) {
				return 1;
			}
			return 0;
		}
		throw new NumberFormatException();
	}
	
	protected List<T> findById(String searchKey, String searchField) {
		if (isIdSearchKey(searchKey, searchField)) {
			List<T> list = new ArrayList<T>();
			String id = searchKey.replace("#", "");
			Long idNumber = Long.parseLong(id);
			T entity = findById(idNumber);
			if (entity != null) {
				list.add(entity);
			}
			return list;
		}
		throw new NumberFormatException();
	}
	

	@Override
	public List<D> searchByCriteria(QueryCriteriaDto criteriaDto) {
		try {
			List<T> entities = findById(criteriaDto.getSearchKey(), criteriaDto.getSearchField());
			return createDtoList(entities);
		}
		catch (NumberFormatException e) {
			QueryCriteria criteria = QueryCriteriaMapper.toQueryCriteria(criteriaDto);
			List<T> entities = getDao().findByCriteria(criteria);
			return createDtoList(entities);
		}
	}
	
	protected abstract IGenericDao<T> getDao();
	
	protected abstract List<D> createDtoList(List<T> entities);
	
	@Override
	public T requireById(final long id) {
		T t = getDao().findById(id);
		if (t == null) {
			throw new EntityNotFoundException(id, getDao().getClazz());
		}
		return t;
	}

}
