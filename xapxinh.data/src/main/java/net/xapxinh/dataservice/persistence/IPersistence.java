package net.xapxinh.dataservice.persistence;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

@Transactional
public interface IPersistence<T> {
	
	public T insert(T entity);
	
	public T saveOrUpdate(T entity);
	
	public T update(T entity);

	public void delete(T entity);
	
	public void deleteById(Long id);
	
	public List<T> loadAll();
	
	public T findById(Long id);
}
