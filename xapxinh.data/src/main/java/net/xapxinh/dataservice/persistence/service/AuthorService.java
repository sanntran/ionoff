package net.xapxinh.dataservice.persistence.service;

import net.xapxinh.dataservice.entity.Author;

import org.springframework.transaction.annotation.Transactional;

@Transactional
public interface AuthorService extends IGenericService<Author> {

	public Author findByName(String name);
}
