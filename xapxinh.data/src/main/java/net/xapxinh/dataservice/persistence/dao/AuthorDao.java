package net.xapxinh.dataservice.persistence.dao;

import net.xapxinh.dataservice.entity.Author;

import org.springframework.transaction.annotation.Transactional;

@Transactional
public interface AuthorDao extends IGenericDao<Author> {
	
	public Author findByName(String name);
}
