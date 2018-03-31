package net.xapxinh.dataservice.persistence.dao.impl;

import java.util.List;

import net.xapxinh.dataservice.entity.Author;
import net.xapxinh.dataservice.persistence.dao.AuthorDao;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.transaction.annotation.Transactional;

@Transactional
public class AuthorDaoImpl extends AbstractGenericDao<Author> implements AuthorDao {

	protected AuthorDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(Author.class);
	}

	@Override
	public Author findByName(String name) {
		String sql = "select distinct author "
			 		+ "from Author as author " 
			 		+ "where author.name = :name ";
	
		Query query = getCurrentSession().createQuery(sql)
					.setParameter("name", name);
		
		// hibernate does not care about accent, so we have to check accent here 
		List<Author> authors = findMany(query);
		if (authors == null) { // 
			return null;
		}
		for (Author author : authors) {
			if (author.getName().equals(name)) {
				return author;
			}
		}
		return null;
	}
}
