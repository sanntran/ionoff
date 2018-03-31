package net.xapxinh.dataservice.persistence.dao.impl;

import java.util.List;

import net.xapxinh.dataservice.entity.Song;
import net.xapxinh.dataservice.persistence.dao.SongDao;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.transaction.annotation.Transactional;

@Transactional
public class SongDaoImpl extends AbstractGenericDao<Song> implements SongDao {

	protected SongDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(Song.class);
	}

	@Override
	public Song findByName(String name) {
		String sql = "select distinct song "
			 		+ "from Song as song " 
			 		+ "where song.name = :name ";
	
		Query query = getCurrentSession().createQuery(sql)
					.setParameter("name", name);
		
		return getFirst(findMany(query));
	}

	@Override
	public List<Song> findByTitle(String title) {
		String sql = "select distinct song "
		 		+ "from Song as song " 
		 		+ "where song.title = :title ";

		Query query = getCurrentSession().createQuery(sql)
					.setParameter("title", title);
		
		return findMany(query);
	}
}
