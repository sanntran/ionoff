package net.ionoff.center.server.persistence.dao;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.persistence.IPersistence;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.Query;
import java.io.Serializable;
import java.util.List;

;

@Transactional
public interface IGenericDao<T extends Serializable> extends IPersistence<T> {
	
	String getClazz();
	
	List<T> findMany(Query query);
	
	List<T> findMany(Query query, int fromIndex, int maxResults);
	
	long countByCriteria(QueryCriteria criteria);
	
	List<T> findByCriteria(QueryCriteria criteria);

}
