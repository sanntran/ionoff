package net.xapxinh.dataservice.persistence.dao.impl;

import net.xapxinh.dataservice.entity.AlbumSong;
import net.xapxinh.dataservice.persistence.dao.AlbumSongDao;

import org.hibernate.SessionFactory;
import org.springframework.transaction.annotation.Transactional;

@Transactional
public class AlbumSongDaoImpl extends AbstractGenericDao<AlbumSong> implements AlbumSongDao {

	protected AlbumSongDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(AlbumSong.class);
	}
}
