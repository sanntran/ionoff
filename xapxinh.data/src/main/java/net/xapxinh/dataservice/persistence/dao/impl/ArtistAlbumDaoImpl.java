package net.xapxinh.dataservice.persistence.dao.impl;

import net.xapxinh.dataservice.entity.ArtistAlbum;
import net.xapxinh.dataservice.persistence.dao.ArtistAlbumDao;

import org.hibernate.SessionFactory;
import org.springframework.transaction.annotation.Transactional;

@Transactional
public class ArtistAlbumDaoImpl extends AbstractGenericDao<ArtistAlbum> implements ArtistAlbumDao {

	protected ArtistAlbumDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(ArtistAlbum.class);
	}
}
