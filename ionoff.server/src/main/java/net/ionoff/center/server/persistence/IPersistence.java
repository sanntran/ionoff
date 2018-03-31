package net.ionoff.center.server.persistence;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

@Transactional
public interface IPersistence<T> {
	
	T insert(T entity);

	T update(T entity);

	void delete(T entity);
	
	void deleteById(long id);
	
	List<T> loadAll();
	
	T findById(long id);
	
	T loadById(long id);
}
