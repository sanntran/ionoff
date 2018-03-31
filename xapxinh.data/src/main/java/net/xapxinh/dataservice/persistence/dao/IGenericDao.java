package net.xapxinh.dataservice.persistence.dao;

import java.io.Serializable;
import java.util.List;

import net.xapxinh.dataservice.persistence.IPersistence;

import org.hibernate.Query;
import org.springframework.transaction.annotation.Transactional;

@Transactional
public interface IGenericDao<T extends Serializable> extends IPersistence<T> {	
	public List<T> findMany(Query query);
}
