package net.xapxinh.dataservice.persistence.dao.impl;

import net.xapxinh.dataservice.entity.AuthorAlbum;
import net.xapxinh.dataservice.persistence.dao.AuthorAlbumDao;

import org.hibernate.SessionFactory;
import org.springframework.transaction.annotation.Transactional;

@Transactional
public class AuthorAlbumDaoImpl extends AbstractGenericDao<AuthorAlbum> implements AuthorAlbumDao {

	protected AuthorAlbumDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(AuthorAlbum.class);
	}
}
