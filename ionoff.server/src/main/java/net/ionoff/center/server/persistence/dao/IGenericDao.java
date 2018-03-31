package net.ionoff.center.server.persistence.dao;

import java.io.Serializable;
import java.util.List;

import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.persistence.IPersistence;

@Transactional
public interface IGenericDao<T extends Serializable> extends IPersistence<T> {
	
	String getClazz();
	
	List<T> findMany(Query query);
	
	List<T> findMany(Query query, int fromIndex, int maxResults);
	
	long countByCriteria(QueryCriteria criteria);
	
	List<T> findByCriteria(QueryCriteria criteria);

	Session getCurrentSession();

	SessionFactory getSessionFactory();
}
