package net.xapxinh.dataservice.persistence.service.impl;

import org.springframework.transaction.annotation.Transactional;

import net.xapxinh.dataservice.entity.Author;
import net.xapxinh.dataservice.persistence.dao.AuthorDao;
import net.xapxinh.dataservice.persistence.service.AuthorService;

@Transactional
public class AuthorServiceImpl extends AbstractGenericService<Author> implements AuthorService {

	private AuthorDao authorDao;
	
	public AuthorServiceImpl(AuthorDao authorDao) {
		this.authorDao = authorDao;
	}

	@Override
	protected AuthorDao getDao() {
		return authorDao;
	}

	@Override
	public Author findByName(String name) {
		return getDao().findByName(name);
	}
}
